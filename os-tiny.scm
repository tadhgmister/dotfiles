
;; tiny minimalist os config that makes use of the wrap-os technique that is setup by my initial install script
(use-modules
 (gnu)
 ((gnu packages linux) #:select (brightnessctl))
 ((gnu services desktop) #:select (%desktop-services elogind-service-type elogind-configuration))
 ((gnu services xorg) #:select (gdm-service-type))
 ((system-info setup) #:select(wrap-os))
 )


(define* (my-os #:key hostname filesystems boot-config username)
(operating-system
  (host-name hostname)
  (bootloader boot-config)
  (file-systems filesystems)

  (locale "en_CA.utf8")
  (timezone "America/Toronto")
  (keyboard-layout (keyboard-layout "us"))

  ;; The list of user accounts ('root' is implicit).
  (users (cons*
	  (user-account
                  (name username)
                  (comment username)
                  (group "users")
                  (home-directory (string-append "/home/" username))
                  (supplementary-groups '("wheel" ;; for sudo access
					  "audio" ;; to be able to use alsamixer etc
					  "video" ;; to control screen backlight
					  ;;"input" ;; to control caps lock light
					  )))
                %base-user-accounts))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (cons*
    ;; (service tlp-service-type ;; enables power optimizations
    ;; 	     (tlp-configuration
    ;; 	      (wifi-pwr-on-bat? #f))) ;; tried this to fix wifi issue, don't think it fixed
    ;;(service thermald-service-type) ;; helps with power, I think it does fan control.
    (udev-rules-service 'brightnessctl brightnessctl)
    (modify-services
        %desktop-services
	;;(guix-service-type config => (tadhg:substitutes config))
	;;(elogind-service-type
	;;  config =>
	;;  (elogind-configuration
        ;;   (inherit config)
	;;   (handle-power-key 'hibernate)
	;;   ;;(idle-action 'suspend)
        ;;   (handle-lid-switch 'hybrid-sleep)
	;;   ))
	(delete gdm-service-type)
	)))
  ;; allow using .local with mdns resolution, used for printer in particular
  ;;(name-service-switch %mdns-host-lookup-nss)
  ))
                      

(wrap-os my-os)
