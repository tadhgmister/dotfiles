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
(define (make-program-file rule-title script-function)
  (program-file (string-append "dunst-" rule-title "-script")
			       #~(#$script-function
				      #:app-name (getenv "DUNST_APP_NAME")
				      #:summary (getenv "DUNST_SUMMARY")
				      #:body (getenv "DUNST_BODY")
				      #:icon (getenv "DUNST_ICON_PATH")
				      #:urgency (getenv "DUNST_URGENCY")
				      #:id (getenv "DUNST_ID")
				      #:progress (getenv "DUNST_PROGRESS")
				      #:category (getenv "DUNST_CATEGORY")
				      #:urls (getenv "DUNST_URLS")
				      #:timeout (getenv "DUNST_TIMEOUT")
				      #:timestamp (getenv "DUNST_TIMESTAMP")
				      #:desktop-entry (getenv "DUNST_DESKTOP_ENTRY")
				      #:stack-tag (getenv "DUNST_STACK_TAG"))))

(define (serialize-new rule-title . modifiers)
  (use-modules (ice-9 format))
  (define (serialize-one-line pair)
    (let* ((selector (car pair))
	   (value-raw (cdr pair))
	   (value (if (eq? selector 'script)
		      (make-program-file rule-title value-raw)
		      value-raw)))
      #~(format #f "   ~s = ~s~%" #$selector #$value)))
  #~(string-append "[" #$rule-title "]\n" #$@(map serialize-one-line modifiers)))

(define (serialize-one-line pair rule-title)
  (let* ((field (symbol->string (car pair)))
      (value (if (and (eq? 'script (car pair)) (not (string? (cdr pair))))
		 (program-file (string-append "dunst-" rule-title "-script")
			       #~(#$(cdr pair)
				      #:app-name (getenv "DUNST_APP_NAME")
				      #:summary (getenv "DUNST_SUMMARY")
				      #:body (getenv "DUNST_BODY")
				      #:icon (getenv "DUNST_ICON_PATH")
				      #:urgency (getenv "DUNST_URGENCY")
				      #:id (getenv "DUNST_ID")
				      #:progress (getenv "DUNST_PROGRESS")
				      #:category (getenv "DUNST_CATEGORY")
				      #:urls (getenv "DUNST_URLS")
				      #:timeout (getenv "DUNST_TIMEOUT")
				      #:timestamp (getenv "DUNST_TIMESTAMP")
				      #:desktop-entry (getenv "DUNST_DESKTOP_ENTRY")
				      #:stack-tag (getenv "DUNST_STACK_TAG")))
		  (cdr pair))))
    #~(string-append "   " #$(symbol->string (car pair)) " = " #$value "\n")))
(define (serialize-one-section section)
  (let* ((rule-title (symbol->string (car section)))
	 (rule-opts  (cdr section)))
    #~(string-append "[" #$rule-title "]\n" #$@(map ((@ (srfi srfi-26) cut) serialize-one-line <> rule-title) rule-opts))))
(define (serialize-dunst-rules rules)
  #~(string-append #$@(map serialize-one-section rules)))
(define (serialize-list field-name value) (serialize-dunst-rules value))
(define-configuration home-dunst-configuration
  (dunst
   (file-like dunst)
   "Dunst package to use."
   (serializer (lambda (field-name value) "")))
  (rules
   (list '())
   ;; TODO: better document this.
   "rules to configure."))

(define (dunst-shepherd-service config)
  (define config-file ;;(home-dunst-configuration-config-file config))
    (computed-file "dunst.conf"
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (display #$(serialize-dunst-rules (home-dunst-configuration-rules config))
                                  port)))))

  (list (shepherd-service
         (documentation "Dunst program.")
         (provision '(dunst notification-daemon))

         ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
         ;; available, and fails to start otherwise.
         (requirement '(dbus x11-display))

         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     (list #$(file-append
                              (home-dunst-configuration-dunst config)
                              "/bin/dunst")
                           "-conf" #$config-file)
		     #:log-file "/home/tadhg/.local/state/log/dunst.log"
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
