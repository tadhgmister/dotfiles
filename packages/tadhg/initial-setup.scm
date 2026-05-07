;; contains nearly empty os and home definitions that still has channel and substitute info
;; so that this can be the initially installed, then do guix pull and nix-channel update
;; at which point the full os and home can be built and installed

(define-module (tadhg initial-setup)
  #:use-module (srfi srfi-1)
  
 ;; #:use-module ((nongnu packages linux) #:select(linux iwlwifi-firmware i915-firmware))
 ;; #:use-module ((nongnu system linux-initrd) #:select(microcode-initrd))
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module ((gnu home services) #:select(home-xdg-configuration-files-service-type))
  #:use-module ((gnu home services guix) #:select(home-channels-service-type))
  #:use-module ((gnu services) #:select(modify-services))
  #:use-module ((gnu services base) #:select(guix-service-type %base-services))
  #:use-module ((gnu services nix) #:select(nix-service-type))
  #:use-module (gnu services networking)
  #:use-module ((tadhg channels-and-subs) #:prefix tadhgs: )
  #:use-module ((system-info setup) #:select(wrap-os))
  #:export (tmp-os tmp-home)
  )



(define* (empty-os #:key hostname filesystems boot-config username #:allow-other-keys)
  (operating-system
   (host-name hostname)

  ;; COMMENT OUT THESE LINES TO REMOVE DEPENDENCY ON NON GUIX
  ;;(kernel linux)
  ;;(initrd microcode-initrd)
  ;;(firmware (list iwlwifi-firmware i915-firmware))
   
   (file-systems filesystems)
   (bootloader boot-config)
   
   ;; The list of user accounts ('root' is implicit).
   (users (cons*
	   (user-account
            (name username)
            (comment username)
            (group "users")
            (home-directory (string-append "/home/" username))
            (supplementary-groups '("wheel" ;; for sudo access
				    )))
           %base-user-accounts))
   (services
    (cons*
     (service nix-service-type)
     (service network-manager-service-type)
     (service wpa-supplicant-service-type)    ;needed by NetworkManager
     (modify-services
      %base-services
      (guix-service-type config => (tadhgs:substitutes config)))))))
(define tmp-os (wrap-os empty-os))

(define tmp-home
  (home-environment
   (services
    (list
     (simple-service 'channels home-channels-service-type tadhgs:channels)
     (simple-service 'configfiles home-xdg-configuration-files-service-type
		     `((".nix-channels" ,tadhgs:nix-channels)))
     ))
   ))

									      
   
