(define-module (tadhg dbus-temp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:autoload   (gnu packages glib)    (dbus)
  #:autoload   (gnu packages xdisorg) (redshift unclutter)
  #:autoload   (gnu packages xorg) (setxkbmap xmodmap)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:export (home-dbus-configuration
            home-dbus-service-type))
(define build
    #~(begin
        (use-modules (sxml simple)
                     (srfi srfi-1))
	(call-with-output-file #$output
          (lambda (port)
	    (sxml->xml
	       '(
		 (includedir "/home/tadhg/.guix-home/profile/share/dbus-1/system.d")
		 (includedir "/home/tadhg/.guix-home/profile/etc/dbus-1/system.d")
		 (servicedir "/home/tadhg/.guix-home/profile/share/dbus-1/services")
		 (includedir "/home/tadhg/.guix-profile/share/dbus-1/system.d")
		 (includedir "/home/tadhg/.guix-profile/etc/dbus-1/system.d")
		 (servicedir "/home/tadhg/.guix-profile/share/dbus-1/services")
		 )
	       port)))))
	    
(define default-home-dbus-config-file
  (computed-file "dbus-home.conf" build))


(define-record-type* <home-dbus-configuration>
  home-dbus-configuration make-home-dbus-configuration
  home-dbus-configuration?
  (dbus home-dbus-dbus                  ;file-like
        (default dbus))
  (config-file home-dbus-config-file
	       (default default-home-dbus-config-file)))

(define (home-dbus-shepherd-services config)
  (list (shepherd-service
         (documentation "Run the D-Bus daemon in session-specific mode. THIS IS A TEMPORARY EDIT TO TEST")
         (provision '(dbus))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-dbus-dbus config)
                                        "/bin/dbus-daemon")
                         "--nofork" "--session" "--config-file" #$(home-dbus-config-file config)
                         (format #f "--address=unix:path=~a/bus"
                                 (or (getenv "XDG_RUNTIME_DIR")
                                     (format #f "/run/user/~a"
                                             (getuid)))))
                   #:environment-variables
                   (cons "DBUS_VERBOSE=1"
                         (default-environment-variables))
                   #:log-file
                   (format #f "~a/log/dbus.log"
                           (or (getenv "XDG_STATE_HOME")
                               (format #f "~a/.local/state"
                                       (getenv "HOME"))))))
         (stop #~(make-kill-destructor))
	 
         (actions (list (shepherd-configuration-action (home-dbus-config-file config)))))))

(define (home-dbus-environment-variables config)
  '(("DBUS_SESSION_BUS_ADDRESS"
     . "unix:path=${XDG_RUNTIME_DIR:-/run/user/$UID}/bus")))

(define home-dbus-service-type
  (service-type
   (name 'home-dbus)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-dbus-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-dbus-environment-variables)))
   (default-value (home-dbus-configuration))
   (description
    "Run the session-specific D-Bus inter-process message bus.")))
