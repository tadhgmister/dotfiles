;; contains nearly empty os and home definitions that still has channel and substitute info
;; so that this can be the initially installed, then do guix pull and nix-channel update
;; at which point the full os and home can be built and installed

(define-module (tadhg initial-setup)
  #:use-module (srfi srfi-1)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module ((gnu home services) #:select(home-xdg-configuration-files-service-type))
  #:use-module ((gnu home services guix) #:select(home-channels-service-type))
  #:use-module ((gnu services) #:select(modify-services))
  #:use-module ((gnu services base) #:select(guix-service-type %base-services))
  #:use-module ((gnu services nix) #:select(nix-service-type))
  #:use-module ((tadhg channels-and-subs) #:prefix tadhgs: )
  #:use-module ((system-info setup) #:select(wrap-os))
  #:export (tmp-os tmp-home)
)
(define* (empty-os #:key hostname filesystems boot-config #:allow-other-keys)
  (operating-system
   (host-name hostname)
   (file-systems filesystems)
   (bootloader boot-config)
   (services
    (cons*
     (service nix-service-type)
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

									      
   
