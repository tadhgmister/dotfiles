
(use-modules
 (gnu)
 (gnu image)
 (gnu system image)
 (guix gexp)
 ((guix packages) #:select (package origin base32 modify-inputs package-native-inputs))
 ((guix git-download) #:select (git-fetch git-reference git-file-name))

 ((gnu packages ssh) #:select(openssh))
 ((gnu packages autotools) #:select(automake autoconf))

 ;;((gnu bootloader grub) #:select(grub-efi-bootloader))
 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))

 )

;; this contains one commit to comment out the configure code for -fzero-call-used-regs
(define patched-ssh
  (package
    (inherit openssh)
    (name "openssh_fzero-call-used-regs-removed")
    ;; autoconf is used to build the configure.ac and it failed saying it needed aclocal which seems to come from automake
    (native-inputs (modify-inputs (package-native-inputs openssh) (append automake autoconf)))
     (source (origin
             (method git-fetch)
             (sha256 (base32 "09rih4paiw9hpchjplf8szl7z7w0pqqqx6bij5fkxxsxd5mvy00n"))
             (uri (git-reference (url "https://github.com/tadhgmister/openssh-portable")
                                 (commit "59758a3c3764b35dca7f22f8e37eb301f688eab0")))
	     (file-name (git-file-name "openssh" "tadhgpatch"))))))

;;; STUFF FROM UBOOT
;;; TODO: remove this after confirming grub-efi works
(use-modules
 ((gnu bootloader u-boot) #:select(u-boot-bootloader))
 ((gnu bootloader) #:select(bootloader))
  ((gnu packages bootloaders) #:select(make-u-boot-package))
  ((gnu packages tls) #:select(openssl))
  ((gnu packages algebra) #:select(bc)))
;;(define TURRIS_SELF_DECLARED_ARCHITECTURE "arm-linux-muslgnueabi")
;;(define WORKING_ARCHITECTURE                  "arm-linux-gnueabihf")
;;(define base (make-u-boot-package "turris_omnia" WORKING_ARCHITECTURE))
;;(define omnia-u-boot (package (inherit base) (native-inputs (modify-inputs (package-native-inputs base) (append openssl bc)))))
(define turris-omnia-u-boot-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package #f)
   (installer #f)
   (disk-image-installer #f)))


(define HOSTNAME "omniaguix")
(define DEVICENAME "/dev/mmc0p1")
(define my-system (operating-system
		    (host-name HOSTNAME)
		    (timezone "America/Toronto")
		    (bootloader (bootloader-configuration
				 (bootloader turris-omnia-u-boot-bootloader)
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
					    (openssh patched-ssh)
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
