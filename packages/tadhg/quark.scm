(define-module (tadhg quark)
  #:use-module (guix gexp)
  #:use-module (gnu services configuration) ;; defining the configuration and serialization
  #:use-module (gnu services) ;; home service definitions
  #:use-module (gnu services shepherd) ;; for shepherd stuff
  #:use-module ((gnu packages web) #:select(quark))
  #:export (quark-configuration quark-service-type))


(define-configuration/no-serialization quark-configuration
  (quark
   (file-like quark)
   "quark package to use")
  (directory
   file-like
   "directory to serve")
  (port
   integer
   "port to serve on")
  (host
    (string "::")
    "hostname to bind to, defaults to accept connections from anywhere")
  ;; TODO implement feature to serve a unix domain socket instead of TCP port
  ;; and also a complementary service to forward that socket connections to a HTTPS port
  )

;; (define (make-start-command config)
;;   #~(list #$(file-append (quark-configuration-quark config) "/bin/quark")
;; 		  "-p" #$(quark-configuration-port config)
;; 		  "-d" #$(quark-configuration-directory config)
;; 		  ))

(define (quark-shepherd-services config)
  (list (shepherd-service
	  (documentation "Quark static web server")
	  (provision '(quark))
	  (requirement '(user-processes networking))
	  (start #~(make-forkexec-constructor
		    `(#$(file-append (quark-configuration-quark config) "/bin/quark")
			  "-p" #$(number->string (quark-configuration-port config))
			  "-d" #$(quark-configuration-directory config)
			  "-h" #$(quark-configuration-host config)
			  )))
	  (stop #~(make-kill-destructor)))))

(define quark-service-type
  (service-type
    (name 'quark)
    (extensions
     (list
      (service-extension shepherd-root-service-type
			 quark-shepherd-services)))
    (description "Run the quark http static web server.")))

    
