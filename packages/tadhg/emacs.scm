(define-module (tadhg emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages emacs)
  #:export (home-emacs-service-type
	    home-emacs-configuration
	    org-tangle-file))


(define (emacs-code-to-tangle-file infile outfile)
  (object->string
   `(let ((results (org-babel-tangle-file ,infile)))
     (when (cdr results)
       (error "tangle output multiple files"))
     (copy-file (car results) ,outfile))))
;; TODO get this to somehow verify the init file, probably by
;; byte-compiling it.  will also need to include a way to get extra
;; emacs packages into that process which are provided by the emacs
;; service config which means the org file would need to be passed to
;; the service directly instead of using this wrapper to pass the
;; generated init.el file to the service.
(define* (org-tangle-file outfile orgfile #:optional lang-regex (emacs-package emacs))
  (computed-file
   outfile
      #~(system* #$(file-append emacs-package "/bin/emacs")
		"-Q" ;; quick, skips a bunch of stuff including loading X display resources
		"--batch" ;; no interactive display, implies -q which avoids loading user init script
		"--eval" "(require 'ob-tangle)" ;; load library with org-babel-tangle
		"--eval"
		(let ((out #$output)
		      (in #$orgfile))
	          (object->string
		   `(let ((results (org-babel-tangle-file ,in)))
		      (when (cdr results)
			(error "tangle output multiple files"))
		      (copy-file (car results) ,out)))
		))))
		
		 


(define-maybe file-like)
(define-configuration/no-serialization home-emacs-configuration
  (emacs
   (file-like emacs)
   "emacs package to use")
  (init.el
   (maybe-file-like %unset-value)
   "init file to load on startup")
  (extra-packages
   (list-of-strings '())
   "list of guix package names with 'emacs-' prefix removed to be installed to profile")
  )
;; the reload command is setup to not depend on the init file content
;; with the hopes that it can be recognized that on home config reload
;; the shepherd service data structure is exactly the same and thus
;; does not need to be restarted.
;; TODO: check if this ^ is true at all, not sure how the configuration changes get propogated.
(define (user-emacs-directory config)
  ".emacs.d")
(define* (emacsclient-command-to-reload-init config #:optional init.el-path)
  (let ((init.el (or init.el-path (string-append "~/" (user-emacs-directory config) "/init.el"))))
    (list (file-append (home-emacs-configuration-emacs config) "/bin/emacsclient")
	  "-e" #~(string-append "(load \"" #$init.el "\")"))))
(define (reload-init-shepherd-action config)
  (shepherd-action
   (name 'reload)
   (documentation "reloads the init.el file")
   (procedure
    #~(lambda* (details . args)
	(when args
	  (display "received unexpected arguments:")
	  (display args)
	  (newline))
	(if
	 (zero?
	  (system*
	   #$@(emacsclient-command-to-reload-init config)))
	 #t ;;return success
	 ;; return failure
	 (error "failed to load init.el"))))))


(define (emacs-init-files config)
  (list
   (list (string-append (user-emacs-directory config) "/init.el")
	 (home-emacs-configuration-init.el config))))
(define (emacs-shepherd-service config)
  (list (shepherd-service
	 (documentation "emacs daemon")
	 (provision '(emacs editor))
	 ;; TODO: maybe figure out a way to not depend on x11 display,
	 ;; not sure how emacs normally identifies the display
	 ;; environment in wayland
	 (requirement '(x11-display))
	 (start #~(lambda _
		    (fork+exec-command
		     (list #$(file-append
			      (home-emacs-configuration-emacs config)
			      "/bin/emacs")
			   "--fg-daemon"
			   "--display" (getenv "DISPLAY")))))
	 (stop #~(make-kill-destructor))
	 (actions (list
		   (reload-init-shepherd-action config)
		   (shepherd-action
		     (name 'rescue)
		     (documentation
		      "sends SIGUSR2 to the emacs daemon to interrupt any process that may be stuck")
		     (procedure #~(lambda* (details . args)
				    (when args
				      (display "received unexpected arguments:")
				      (display args)
				      (newline))
				    (kill (process-id details) SIGUSR2)
				    #t))))))))
(define (emacs-packages-for-config config)
  (cons
   (home-emacs-configuration-emacs config)
   (map (lambda (name-stub) (specification->package (string-append "emacs-" name-stub)))
	(home-emacs-configuration-extra-packages config))))

(define (emacs-reload-init-activation config)
  (list
   (list (string-append "files/" (user-emacs-directory config) "/init.el") #~(system* "herd" "reload" "emacs"))))
  ;; (let ((init.el (home-emacs-configuration-init.el config)))
  ;;   (if (maybe-value-set? init.el)
  ;; 	#~(let* ((old-init-path (string-append (getenv "GUIX_OLD_HOME") "/files/"
  ;; 					       #$(user-emacs-directory config) "/init.el"))
  ;; 		 (old-init-file (and (file-exists? old-init-path) (readlink old-init-path)))
  ;; 		 (new-init-file #$init.el))
  ;; 	    (when (and old-init-file (not (string=? old-init-file new-init-file)))
  ;; 	      (unless (zero? (system* #$@(emacsclient-command-to-reload-init config init.el)))
  ;; 		(warn "emacs failed to reload config")))))))
  
(define home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (extensions
    (list
     (service-extension home-files-service-type emacs-init-files)
     (service-extension home-shepherd-service-type emacs-shepherd-service)
     (service-extension home-profile-service-type emacs-packages-for-config)
     (service-extension home-run-on-change-service-type emacs-reload-init-activation)))
   (default-value (home-emacs-configuration))
   ;; TODO emacs has support for named servers, we could potentially allow multiple servers that explicitly give their own names and could potentially manage their own init files
   (compose identity)
					;(extend #f)
   (description "runs an emacs daemon with a specified init file and extra emacs packages")))
	 
  
  
