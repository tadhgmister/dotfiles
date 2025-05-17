
(define-module (system-info setup)
  #:use-module (guix)
  #:use-module (gnu system)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  ;#:use-module (gnu system keyboard)
  #:use-module ((system-info details) #:select (HOSTNAME RESUME-OFFSET ROOT-UUID EFI-UUID))
  #:export (wrap-os)
) 
;; directly relate to stuff from system-info
(define mapper-target (string-append HOSTNAME "_drive"))
(define root-drive (string-append "/dev/mapper/" mapper-target))
(define boot-config (bootloader-configuration
                (bootloader grub-efi-removable-bootloader)
                (targets (list "/boot/efi"))
                ;(keyboard-layout keyboard-layout)
		;(extra-initrd "/swap/keyfile.cpio")
		))

(define mapped-devices
  (list
   (mapped-device
    (source (uuid ROOT-UUID))
    (type luks-device-mapping)
	   ;(luks-device-mapping-with-options
	   ;; this keyfile is relative to the path when the cpio file containing the key was generated
	   ;; whcih is specifically done at the root directory as I don't want any change of using a folder name that
	   ;; is used by the initrd and this ends up visible after boot or overriding something else
	   ;#:key-file "/keyfile.bin"))
    (target mapper-target))))
(define rootfs (file-system
     (mount-point "/")
     (device root-drive)
     (dependencies mapped-devices)
     ;; lazy time indicates that when a file is accessed or changed the
     ;; timestamp for that file won't be immidiately written to disk
     ;; and will instead update the disk when background file IO has to happen anyway
     ;; which gives a performance boost at the cost of access timestamps may be incorrect if the system crashes.
     (flags '(lazy-time))
     ;; set compression to zstd with effort 3
     ;; effort 3 is the default but listing explicitly to allow ease of changing later.
     ;; effort ranges from 1 to 9 where higher gives more compression and more CPU overhead.
     (options
      (alist->file-system-options
       '(("compress" . "zstd:3"))))
     (type "btrfs")))
(define filesystems
  (cons*
   rootfs
   (file-system
     (mount-point "/boot/efi")
     (device (uuid EFI-UUID 'fat32))
     (type "vfat"))
   
   ;; (file-system
   ;;   ;; guix store, use compress-force to try harder to compress stuff and with higher effort
   ;;   (mount-point "/gnu")
   ;;   (options
   ;;    (alist->file-system-options
   ;;     '(("subvol" . "gnu")
   ;; 	 ("compress-force" . "zstd:5"))))
   ;;   (device root-drive)
   ;;   ;; guix/nix/libstore/gc.cc indicates that gc randomizes the order of garbage to collect
   ;;   ;; so there is no point storing any access times in the gnu store
   ;;   (flags '(no-atime no-diratime))
   ;;   (type "btrfs"))
   ;; (file-system
   ;;   ;; nix store, use compress-force to try even harder to compress stuff as it is updated very infrequently
   ;;   (mount-point "/nix")
   ;;   (options
   ;;    (alist->file-system-options
   ;;     '(("subvol" . "nix")
   ;; 	 ("compress-force" . "zstd:7"))))
   ;;   (device root-drive)
   ;;   ;; guix/nix/libstore/gc.cc indicates that gc randomizes the order of garbage to collect
   ;;   ;; so there is no point storing any access times in the gnu store
   ;;   (flags '(no-atime no-diratime))
   ;;   (type "btrfs"))
   
   ;; allow /home and /swap to be automatically loaded by path as they don't need special options
   %base-file-systems
   ))
;; note that the swap file implicitly depends on the root partition being mounted so no need to explicitly specify that
;; if it was on a partition that isn't made available by the partitions necessary for boot then a dependency would be needed here
(define swap-devices (list (swap-space (target "/swap/swapfile"))))

		     

(define* (wrap-os base-os-routine)
  (let ((base-os (base-os-routine #:hostname HOSTNAME
				  #:filesystems filesystems
				  #:boot-config boot-config)))
    
    (operating-system
      (inherit base-os)
      (mapped-devices (append mapped-devices
			      (operating-system-mapped-devices base-os)))
      (swap-devices (append swap-devices
			    (operating-system-swap-devices base-os)))
      (kernel-arguments (cons*
			 (string-append "resume=" root-drive)
			 (string-append "resume_offset=" RESUME-OFFSET)
			 (operating-system-user-kernel-arguments base-os))))))
      
      
