
;;;;; to connect to device while it is plugged in
;; guix shell minicom -- sudo minicom -w -b 115200 -D /dev/ttyUSB0
;;;; to disable watchdog timer while in the uboot prompt
;; mw 0xf1020300 0x403
;;;;; to continue to normal boot sequence after disabling watchdog
;; run bootcmd

(use-modules
 
					;((gnu packages linux) #:select(customize-linux linux-libre linux-libre-arm64-generic))
 (gnu packages linux)
 (gnu)
 (gnu image)
 (gnu system image)
 (guix gexp)
 (guix transformations)
 ((guix packages) #:select (package origin base32 modify-inputs package-native-inputs))
 ((guix git-download) #:select (git-fetch git-reference git-file-name))
 ((guix platforms arm) #:select(armv7-linux))

 ((gnu bootloader) #:select(bootloader))
 ((gnu bootloader u-boot) #:select(u-boot-bootloader))

 ((gnu packages ssh) #:select(openssh))
 ((gnu packages autotools) #:select(automake autoconf))

 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))

 ((guix records) #:select(define-record-type*))
 ((ice-9 match) #:select(match-lambda))
 ((gnu services shepherd) #:select(shepherd-root-service-type shepherd-service))
 ((srfi srfi-1) #:select(lset-difference))
 ((nongnu packages linux) #:select(corrupt-linux linux-firmware))
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
(define transform1
  (options->transformation
    '((without-tests . "guile-ssh"))))
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
;; (define turris-omnia-u-boot-bootloader
;;   (bootloader
;;    (inherit u-boot-bootloader)
;;    (name 'omniaboot)
;;    (package #f)
;;    (installer #f)
;;    (disk-image-installer #f)))



(define HOSTNAME "omniaguix")
;;(define DEVICENAME "/dev/mmcblk0p1")
;;(define PARTITION-UUID "38af4c98-a457-ab59-caf5-b77b38af4c98")


;; kernel setting based on https://gitlab.com/Cynerd/nixturris/-/blob/master/pkgs/default.nix#L15
;; I can't access hard drives without it and that source claims this the turris omnia doesn't work with the PCIEASPM symbol
;; (define KERNEL_CONFIGS (list "CONFIG_PCIEASPM=n"))
(define make-linux-libre* (@@ (gnu packages linux) make-linux-libre*))
(define default-extra-linux-options (@@ (gnu packages linux) default-extra-linux-options))
(define kernel-config (@@ (gnu packages linux) kernel-config))

;; (define linux-libre-arm-omnia
;;   (make-linux-libre* linux-libre-version
;;                      linux-libre-gnu-revision
;;                      linux-libre-source
;;                      '("armhf-linux")
;;                      #:extra-version "arm-omnia"
;; 		     #:configuration-file kernel-config
;;                      #:extra-options
;;                      (append
;;                       `(
;;                         ("CONFIG_PCIEASPM" . #f))
;; 		      %default-extra-linux-options)))

(define KERNEL-VERSION "6.12.10")
(define linux-non-libre-source
  ((@@ (gnu packages linux) %upstream-linux-source) KERNEL-VERSION (base32 "15xjjn8ff7g9q0ljr2g8k098ppxnpvxlgv22rdrplls8sxg6wlaa")))
(define* (kernel-config-from-turris-os arch #:key variant)
  (local-file "turris-kernel.config"))
(define kernel-to-use
  (make-linux-libre* KERNEL-VERSION "IGNORED_VARIABLE" linux-non-libre-source 
                     '("armhf-linux")
                     #:defconfig "mvebu_v7_defconfig"
		     #:configuration-file kernel-config-from-turris-os
                     #:extra-version "nonlibre-arm"
                     #:extra-options
                     (append
                      `(
			;("CONFIG_BTRFS_FS" . #t)
			;("CONFIG_OF" . #t) ;; required for turris leds, Device Tree and Open Firmware support
			;;("CONFIG_MACH_ARMADA_38X" . #t) ;;; enabed by mvebu_v7_defconfig, needed by omnia leds
			;("CONFIG_LEDS_CLASS_MULTICOLOR" . #t) ; depends on LEDS_CLASS which is enabled by defconfig
			;("CONFIG_LEDS_TURRIS_OMNIA" . 'm) ;; also needs I2C which is in defconfig


			("CONFIG_RD_GZIP") . #t) ;; absolutely necessary, guix gzips its initrd
			("CONFIG_CZNIC_PLATFORMS" . #t)
			("CONFIG_TURRIS_OMNIA_MCU" . m)
                        ("CONFIG_TURRIS_OMNIA_MCU_GPIO" . #t)
			("CONFIG_TURRIS_OMNIA_MCU_SYSOFF_WAKEUP" . #t)
                        ("CONFIG_TURRIS_OMNIA_MCU_WATCHDOG" . #t)
                        ("CONFIG_TURRIS_OMNIA_MCU_TRNG" . #t))
                      (default-extra-linux-options KERNEL-VERSION))
		     ))

(define root-label "GuixRoot")
(define my-system (operating-system
		    (kernel kernel-to-use)
		    (firmware (cons* linux-firmware
				     %base-firmware))


		    (initrd-modules (lset-difference equal? %base-initrd-modules
						     '("usb-storage" ;; baked into kernel
						       "hid-apple" ;; we aren't going to use apple products as input device
						       "virtio_console" ;; we are not in a vm
						       "virtio-rng" ;; we are not a vm
						       "btrfs" ;; baked into kernel
						       )))
		    (kernel-arguments (list
				       "earlyprintk"
				       "console=ttyS0,115200"
				       "pcie_aspm=no"
				       "modprobe.blacklist=pcieaspm";;,usbmouse,usbkbd"
				       ))
		    ;; (initrd-modules (cons*
		    ;; 		     ;; these are both used in nixturris with the comment about led support
				     
		    ;; 		     ;;"ahci_mvebu" ;; ahci is about SATA support which might be important when loading from internal card but for now loading from USB probably doesn't need it
		    ;; 		     "rtc_armada38x" ;; something about real time clock, idk if it is important.
		    ;; 		     %base-initrd-modules))
		    (host-name HOSTNAME)
		    (timezone "America/Toronto")
		    (bootloader (bootloader-configuration
				 (bootloader u-boot-bootloader)
				 (timeout 1)
				 (targets '())))
		    (file-systems (cons (file-system
					  (mount-point "/")
					  (device (uuid "6e0391cd-f7f5-4d26-bff1-34bb52258812"));;(file-system-label root-label))
					  (type "btrfs"))
					%base-file-systems))
		    (services
		     (cons*       ;;(service dhcp-client-service-type)
		      ;; (service startupscript-service-type)
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

		      ;; (service openssh-service-type
		      ;; 	       (openssh-configuration
		      ;; 		(openssh patched-ssh)
		      ;; 		(permit-root-login #t)
		      ;; 		(allow-empty-passwords? #t)))
		      %base-services))))
;linux-non-libre-source
my-system

;; (image
;;  (format 'disk-image)
;;  (platform armv7-linux)
;;  (operating-system  my-system)
;;  (partitions
;;   (list
;;    (partition
;;     (size 'guess)
;;     (label root-label)
;;     (file-system "ext4")
;;     (flags '(boot))
;;     (initializer (gexp initialize-root-partition))))))


;; (list (machine
;;        (operating-system my-system)
;;        (environment managed-host-environment-type)
;;        (configuration (machine-ssh-configuration
;;                        (host-name HOSTNAME)
;;                        (system "armhf-linux")
;;                        (user "alice")
;;                        (identity "./id_rsa")
;;                        (port 2222)))))
