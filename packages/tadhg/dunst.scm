(define-module (tadhg dunst)
  #:use-module (guix gexp)
  #:use-module (gnu services configuration) ;; defining the configuration and serialization
  #:use-module (gnu home services) ;; home service definitions
  #:use-module (gnu home services shepherd) ;; for shepherd stuff
  #:use-module (gnu home services desktop) ;; for x11 service
  #:use-module (gnu packages wm) ;; for dunst package
  #:export (home-dunst-configuration
	    home-dunst-service-type)
  )

(define-configuration home-dunst-configuration
  (dunst
   (file-like dunst)
   "Dunst package to use.")
  (config-file
   (file-like (plain-file "empty.conf" ""))
   "config file to use."))

(define (dunst-shepherd-service config)
  (define config-file (home-dunst-configuration-config-file config))
    ;; (computed-file "redshift.conf"
    ;;                #~(call-with-output-file #$output
    ;;                    (lambda (port)
    ;;                      (display #$(serialize-redshift-configuration config)
    ;;                               port)))))

  (list (shepherd-service
         (documentation "Dunst program.")
         (provision '(dunst notification-daemon))

         ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
         ;; available, and fails to start otherwise.
         (requirement '(dbus x11-display))

         ;; (modules '((srfi srfi-1)
         ;;            (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     (list #$(file-append
                              (home-dunst-configuration-dunst config)
                              "/bin/dunst")
                           "-c" #$config-file)
                     ;; Inherit the 'DISPLAY' variable set by 'x11-display'.
                     #:environment-variables
                     (cons (string-append "DISPLAY=" (getenv "DISPLAY"))
                           (remove (cut string-prefix? "DISPLAY=" <>)
                                   (default-environment-variables))))))
         (stop #~(make-kill-destructor))
         (actions (list (shepherd-configuration-action config-file))))))

(define home-dunst-service-type
  (service-type
   (name 'home-dunst)
   (extensions (list (service-extension home-shepherd-service-type
                                        dunst-shepherd-service)
                     ;; Ensure 'home-x11-service-type' is instantiated so we
                     ;; can depend on the Shepherd 'x11-display' service.
                     (service-extension home-x11-service-type
                                        (const #t))))
   (default-value (home-dunst-configuration))
   (description
    "Run Dunst, a minimalist highly configurable notification daemon.")))
