
(use-modules
 (gnu)
 (gnu image)
 (gnu system image)
 (guix gexp)
 ((guix packages) #:select (package origin base32 modify-inputs package-native-inputs))
 ((guix git-download) #:select (git-fetch git-reference git-file-name))
 ((guix platform) #:select(platform))

 ((gnu bootloader) #:select(bootloader))
 ((gnu bootloader u-boot) #:select(u-boot-bootloader))

 ((gnu packages ssh) #:select(openssh))
 ((gnu packages autotools) #:select(automake autoconf))

 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))

 ((guix records) #:select(define-record-type*))
 ((ice-9 match) #:select(match-lambda))
 ((gnu services shepherd) #:select(shepherd-root-service-type shepherd-service))

 )



(define-record-type* <startupscript-configuration>
  startupscript-configuration make-startupscript-configuration
  startupscript-configuration?
  (settings startupscript-configuration-settings  ; alist of string pairs
            (default '())))

(define startupscript-shepherd-service
  (match-lambda
    (($ <startupscript-configuration> settings)
       (shepherd-service
        (documentation "turns on user led light at startup")
        (provision '(startupscript))
        (start #~(lambda _
                   (call-with-output-file "/sys/class/leds/omnia-led:user1/color"
		     (lambda (port)
		       (display "255 60 0" port)))
		   (call-with-output-file "/sys/class/leds/omnia-led:user1/brightness"
		     (lambda (port)
		       (display "255" port)))))
        (one-shot? #t)))))

(define startupscript-service-type
  (service-type
   (name 'startupscript)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list startupscript-shepherd-service))))
   ;; (compose concatenate)
   ;; (extend (lambda (config settings)
   ;;           (sysctl-configuration
   ;;            (inherit config)
   ;;            (settings (append (sysctl-configuration-settings config)
   ;;                              settings)))))
   (default-value (startupscript-configuration))
   (description "turns on the omnia led at startup to see if it is loading guix")))

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

	
;; u-boot-bootloader inherits the extlinux.conf from extlinux but we just rely on the u-boot in nor flash to try to load from the configuration file.
(define turris-omnia-u-boot-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (name 'omniaboot)
   (package #f)
   (installer #f)
   (disk-image-installer #f)))


(define HOSTNAME "omniaguix1")
(define DEVICENAME "/dev/mmcblk0p1")
(define PARTITION-UUID "bc1980eb-68ee-4ac7-b540-8dc3cebf5ab2")
(define my-system (operating-system
		    (host-name HOSTNAME)
		    (timezone "America/Toronto")
		    (bootloader (bootloader-configuration
				 (bootloader turris-omnia-u-boot-bootloader)
				 (timeout 0)
				 (targets '())))
		    (file-systems (cons (file-system
					  (mount-point "/")
					  (device (uuid PARTITION-UUID))
					  (type "ext4"))
					%base-file-systems))
		    (services
		     (cons*       ;;(service dhcp-client-service-type)
		      (service startupscript-service-type)
		      (service static-networking-service-type
			       (list (static-networking
				      (addresses
				       (list (network-address
					      (device "lan4")
					      (value "192.168.1.1/24"))))
				      (routes
				       (list (network-route
					      (destination "default")
					      (gateway "192.168.2.1"))))
				      (name-servers '()))))

		      (service openssh-service-type
			       (openssh-configuration
				(openssh patched-ssh)
				(permit-root-login #t)
				(allow-empty-passwords? #t)))
		      %base-services))))

(image
 (format 'disk-image)
 (platform (platform
	    (glibc-dynamic-linker "/lib/ld-linux-armhf.so.3")
	    (target "arm-linux-gnueabihf")
	    (system "armhf-linux")))
 (operating-system  my-system)
 (partitions
  (list
   (partition
    (size 'guess)
    (uuid PARTITION-UUID)
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
