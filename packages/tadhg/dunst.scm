(define-module (tadhg dunst)
  #:use-module (srfi srfi-1)
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

(define (translate-value value)
  (cond ((gexp? value) value)
	((symbol? value) #~(quote #$value))
	((boolean? value) (if value ''true ''false))
	(else value)))
	  

(define (serialize-dunst-rule rule)
  (define label (symbol->string (list-ref rule 0)))
  (define selectors (list-tail rule 1))
  (define (serialize-selector selector)
    (let* ((key (list-ref selector 0))
	   ;; raw-value could be a gexp
	   (raw-value (list-ref selector 1))
	   (value (if (eq? key 'script)
		      (make-program-file label raw-value)
		      (translate-value raw-value))))
	#~(simple-format #f "  ~A = ~S\n" #$(symbol->string key) #$value)))
  #~(string-append "[" '#$label "]\n" #$@(map serialize-selector selectors)))
	


(define (serialize-list-of-dunst-rules field-name value)
  #~(string-append #$@(map serialize-dunst-rule value)))
(define (dunst-selector? pair)
  (and (list pair) (eqv? 2 (length pair)) (symbol? (list-ref pair 0))))
(define list-of-dunst-selector? (list-of dunst-selector?))

(define (dunst-rule? x)
  (and (list? x) (symbol? (list-ref x 0)) (list-of-dunst-selector? (list-tail x 1))))
(define list-of-dunst-rules? (list-of dunst-rule?))


(define (serialize-list-of-dunst-selector label value)
  ;; label is the symbol 'global' and value is the list of selectors to put in the global section
  ;; apply the serialize-dunst-rule to the pair `(global . ,value) but the symbol 'global is already stored in the label.
  (serialize-dunst-rule `(,label . ,value)))

(define-maybe string)

(define (string-or-false? x) (or (string? x) (eq? x #f)))

(define-configuration home-dunst-configuration
  (dunst
   (file-like dunst)
   "dunst package to use"
   empty-serializer)
  (startup-notification
   (boolean #f)
   "generate notification on startup?"
   empty-serializer)
  (verbosity
   (string "mesg")
   "verbosity level"
   empty-serializer)
  (log-file
   (string-or-false #f)
   "path to log file"
   empty-serializer)
  (global
   (list-of-dunst-selector '())
   "settings to put in the global section of configuration")
  (rules
   (list-of-dunst-rules '())
   "rules for configuration file"))

(define (make-start-command config config-file)
  (define verbosity (home-dunst-configuration-verbosity config))
  #~(append (list #$(file-append (home-dunst-configuration-dunst config) "/bin/dunst")
				      "-conf" #$config-file)
				(if #$(home-dunst-configuration-startup-notification config)
				    '("-startup_notification")
				    '())
				(if #$(maybe-value-set? verbosity)
				    (list "-verbosity" #$verbosity)
				    '())))

(define (dunst-shepherd-service config)
  (define config-file (mixed-text-file "dunst.conf" (serialize-configuration config home-dunst-configuration-fields)))
  (list (shepherd-service
         (documentation "Dunst program.")
         (provision '(dunst notification-daemon))

         ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
         ;; available, and fails to start otherwise.
	 ;; Also depend on dbus.
         (requirement '(dbus x11-display))
	 ;; the main reason it may crash is if the x server is closed in which case respawning isn't useful
	 (respawn? #f)

         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     #$(make-start-command config config-file)
		     #:log-file #$(home-dunst-configuration-log-file config)
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
   ;;; TODO: add extension of dbus service to ensure dbus is present 
   (extensions (list (service-extension home-shepherd-service-type
                                        dunst-shepherd-service)
                     ;; Ensure 'home-x11-service-type' is instantiated so we
                     ;; can depend on the Shepherd 'x11-display' service.
                     (service-extension home-x11-service-type
                                        (const #t))))
   (default-value (home-dunst-configuration))
   (description
    "Run Dunst, a minimalist highly configurable notification daemon.")
   ))
