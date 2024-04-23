;; This is a Guix deployment of a "bare bones" setup, with
;; no X11 display server, to a machine with an SSH daemon
;; listening on localhost:2222. A configuration such as this
;; may be appropriate for virtual machine with ports
;; forwarded to the host's loopback interface.

;(use-service-modules networking ssh)
(use-modules
 (gnu)
 (gnu image)
 (gnu system image)
 (guix gexp)

 ;;((gnu bootloader grub) #:select(grub-efi-bootloader))
 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))

)
;;; STUFF FROM UBOOT
;;; TODO: remove this after confirming extlinux works
;; (use-modules
;;  ((gnu packages bootloaders) #:select(make-u-boot-package))
;;  ((gnu packages tls) #:select(openssl))
;;  ((gnu packages algebra) #:select(bc)))
;; (define TURRIS_SELF_DECLARED_ARCHITECTURE "arm-linux-muslgnueabi")
;; (define WORKING_ARCHITECTURE                  "arm-linux-gnueabihf")
;; (define base (make-u-boot-package "turris_omnia" WORKING_ARCHITECTURE))
;; (define omnia-u-boot (package (inherit base) (native-inputs (modify-inputs (package-native-inputs base) (append openssl bc)))))

(define HOSTNAME "omniaguix")
(define DEVICENAME "/dev/mmc0p1")
(define my-system (operating-system
		    (host-name HOSTNAME)
		    (timezone "America/Toronto")
		    (bootloader (bootloader-configuration
				 (bootloader grub-efi-bootloader)
				 (targets (list DEVICENAME))))
		    (file-systems (cons (file-system
					  (mount-point "/")
					  (device DEVICENAME)
					  (type "ext4"))
					%base-file-systems))
		    (services
		     (cons*       (service dhcp-client-service-type)
				  (service openssh-service-type
					   (openssh-configuration
					    (permit-root-login #t)
					    (allow-empty-passwords? #t)))
				  %base-services))))

(image
 (format 'disk-image)
 (operating-system  my-system)
 (partitions
  (list
   (partition
    (size 'guess)
    (label root-label)
    (file-system "ext4")
    (flags '(boot))
    (initializer (gexp initialize-root-partition))))))

;; (list (machine
;;        (operating-system my-system)
;;        (environment managed-host-environment-type)
;;        (configuration (machine-ssh-configuration
;;                        (host-name HOSTNAME)
;;                        (system "armhf-linux")
;;                        (user "alice")
;;                        (identity "./id_rsa")
;;                        (port 2222)))))
