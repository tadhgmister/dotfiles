
(define-module (system-info setup)
  #:use-module (guix)
  #:use-module (gnu system)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module ((gnu packages linux) #:select(efibootmgr))
  ;#:use-module (gnu system keyboard)
  #:use-module ((system-info details) #:select (HOSTNAME RESUME-OFFSET ROOT-UUID EFI-UUID))
  #:export (wrap-os)
) 
;; directly relate to stuff from system-info
(define mapper-target (string-append HOSTNAME "_drive"))
(define root-drive (string-append "/dev/mapper/" mapper-target))

;; TODO figure out how the heck to derive these in the format that efibootmgr accepts from the bootloader config
(define HARDCODED-EFIDEVICE "/dev/nvme0n1")
(define HARDCODED-EFIPART "1")

(define custom-bootloader
  (bootloader
    (inherit grub-efi-removable-bootloader)
    
   (installer
    (with-imported-modules '((ice-9 popen) (ice-9 rdelim))
    #~(lambda (bootloader target mount-point)
	;; start by running the grub-efi-removable installer so if the rest fails our partially broken state should at least have grub accessible.		       
	(define efi-removable-value (#$(bootloader-installer grub-efi-removable-bootloader) bootloader target mount-point))
        (define sys-link (getenv "GUIX_NEW_SYSTEM"))
	(unless (string? sys-link)
	  (error "can only run this bootloader installer when GUIX_NEW_SYSTEM environment variable is set"))
	(define (extract-N str)
	  (let* ((prefix "/var/guix/profiles/system-")
		 (suffix "-link")
		 (len (string-length str))
		 (plen (string-length prefix))
		 (slen (string-length suffix)))
	    (and (>= len (+ plen slen))                      ; long enough?
		 (string-prefix? prefix str)                 ; starts correctly?
		 (string-suffix? suffix str)                 ; ends correctly?
		 (substring str plen (- len slen)))))        ; middle part

	(define generation-n (extract-N sys-link))
	(unless generation-n
	  (error (string-append "CANNOT IDENTIFY GENERATION NUMBER" sys-link)))
	
	(define (load-as-data filename)
	  (with-input-from-file filename
	    (lambda ()
	      (read))))
	(define boot-params (load-as-data (string-append sys-link "/parameters")))

	(define (get-param p)
	  (let ((x (assoc p (cdr boot-params))))
	    (unless x
	      (error (string-append "CANNOT GET PARAMETER " (symbol->string p))))
	    (cadr x)))
	(define kernel (canonicalize-path (get-param 'kernel)))
	(define initrd (canonicalize-path (get-param 'initrd)))
	(define root (get-param 'root-device))
	;; TODO submit patch to guix so if gnu.system and gnu.load are
	;; not specified it determines all system revisions with the
	;; same boot parameters and either offers them as choices or just loads the latest one.
	(define args-that-could-be-infered-by-early-init-if-guix-was-patched
	  "gnu.system=/var/guix/profiles/system gnu.load=/var/guix/profiles/system/boot")
	;; efi standard uses backslash relative to the esp partition root, initrd and loader need this substitution
	(define (slash->backslash str)
	  (string-map (lambda (c)
			(if (char=? c #\/) #\\ c))
		      str))
	(define loader (string-append (slash->backslash kernel) ".EFI"))
	(define args (string-join (cons* (string-append "initrd=" (slash->backslash initrd))
					 (string-append "root=" root)
					 args-that-could-be-infered-by-early-init-if-guix-was-patched
					 (get-param 'kernel-arguments))))
	;; this is how grub decides where to install the files, it
	;; seems absolutely absurd to me it just uses a string and not
	;; the filesystem object that we could directly measure the
	;; mount point of.
	(define mount-point/target (string-append mount-point target))
	(define bootloader-target (if (file-exists? mount-point/target)
                                      mount-point/target
                                      target))


	;;; now we actually get to the useful bootloader installing code!
	;; copy the kernel and initrd into the esp partition with the
	;; same folder structure for simplicity.  we also append .EFI
	;; to be sure that UEFI implementations accept it as a viable
	;; loader although it is possible this is not necessary.
	(mkdir-p (string-append bootloader-target (dirname kernel)))
	(mkdir-p (string-append bootloader-target (dirname initrd)))
	(copy-file kernel (string-append bootloader-target kernel ".EFI"))
	(copy-file initrd (string-append bootloader-target initrd))

	;; and now we need to run efibootmgr to put the boot entry.
	
	(use-modules (ice-9 popen) (ice-9 rdelim))
        (define (command-output . args)
	  ;; Run command with stdout captured, stderr merged into our stderr
	  (let* ((port (apply open-pipe* OPEN_READ args))
		 (out (read-string port))
		 (status (close-pipe port)))  ; closing waits for child, returns status
	    (if (zero? status)
		out
	        (error "command failed" (cons status args)))))
	;; first read the boot options list so we can detect if our boot entry is already there
	(define prev-boot-options (command-output
				   #+(file-append efibootmgr "/sbin/efibootmgr")
				   "--unicode"))
	;; output of efibootmgr has no space between the executing image and the arguments
	(define expected-boot-entry (string-append loader args))
	;; if there is a boot entry that has all the same arguments then leave it be
	(unless (string-contains prev-boot-options expected-boot-entry )
	  ;; if the loader or args has changed then make a new entry
	  (let ((new-output (command-output
			     #+(file-append efibootmgr "/sbin/efibootmgr")
			     "--create"
			     ;; TODO figure out how to infer disk and
			     ;; part in the format efibootmgr needs if
			     ;; we check that `bootloader-target` is
			     ;; the mount point for say /dev/nvme0n1p1
			     ;; then disk = /dev/nvme0n1 and part=1,
			     ;; the `p` vs no `p` in sda1 causes
			     ;; issues with using a direct string
			     ;; parsing, and that still assumes we
			     ;; already have the mount point.
			     "--disk" #$HARDCODED-EFIDEVICE
			     "--part" #$HARDCODED-EFIPART
			     "--label" (string-append "Guix" generation-n)
			     "--loader" loader
			     "--unicode"
			     args)))
	    ;; check the output now contains the expected string, this
	    ;; is particularly important since if this fails we may
	    ;; leave the machine in a broken state
	    (unless (string-contains new-output expected-boot-entry)
	      (error "efibootmgr create command was run but the expected entry was not present"))))
	
        ;; I don't think the return value is used but just in case ensure this gexp resolves to the same value that our backup grub bootloader resolves to.
	efi-removable-value)))))


(define boot-config (bootloader-configuration
                (bootloader custom-bootloader)
                (targets (list "/boot/efi"))
                ;(keyboard-layout keyboard-layout)
		;(extra-initrd "/swap/keyfile.cpio")
		))

(define mapped-devices
  (list
   (mapped-device
    (source (uuid ROOT-UUID))
    (type luks-device-mapping)
	   ;; this keyfile is relative to the path when the cpio file containing the key was generated
	   ;; whcih is specifically done at the root directory as I don't want any change of using a folder name that
	   ;; is used by the initrd and this ends up visible after boot or overriding something else
    ;(arguments '(#:key-file "/keyfile.bin"))
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
				  #:username USERNAME
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
      
      
