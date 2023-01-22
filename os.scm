;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules
 (gnu)
 
 ((nongnu packages linux) #:select(linux linux-firmware))
 ((nongnu system linux-initrd) #:select(microcode-initrd))
 ((guix packages) #:select (origin base32 modify-inputs package-source package-inputs package))
 ((guix download) #:select (url-fetch))
 ((guix gexp) #:select(file-append))
 ((gnu packages freedesktop) #:select(fprintd))
 ((gnu packages suckless) #:select(slock))
 ((gnu packages games) #:select (steam-devices-udev-rules))
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages linux) #:select (brightnessctl))
 ((gnu packages wm) #:select (swaylock))
 ((gnu packages cups) #:select (cups cups-filters epson-inkjet-printer-escpr hplip-minimal))
 ((gnu services cups) #:select (cups-service-type cups-configuration))
 ((gnu services desktop) #:select (bluetooth-service-type gnome-desktop-service-type %desktop-services elogind-service-type elogind-configuration))
 ((gnu services virtualization) #:select(qemu-binfmt-service-type qemu-binfmt-configuration lookup-qemu-platforms))
 ((gnu services nix) #:select (nix-service-type))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services xorg) #:select (xorg-server-service-type gdm-service-type screen-locker-service screen-locker-service-type xorg-configuration set-xorg-configuration))
 ((gnu services authentication) #:select (fprintd-service-type))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services pm) #:select (tlp-service-type tlp-configuration thermald-service-type))
)

;; (define ledtrigger-udev (udev-rule "90-ledtrigger.rules"
;; "ACTION==\"add\", SUBSYSTEM==\"leds\", RUN+=\"/run/current-system/profile/bin/chgrp input /sys/class/leds/%k/trigger\"
;; ACTION==\"add\", SUBSYSTEM==\"leds\", RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/%k/trigger\""))
				   
;; Note that the wifi connection to CU-Wireless was done with the nm-applet (and guix shell stalonetray) to configure wPA-Enterprise
;; idk where else to put this, wifi info isn't something I expect to ever save to the guix config but want to remember how to do it.
;; also eduroam has an install script for ottawa U, haven't tried it yet since I don't have ottawa U credentials at time of writing
(define slock-patched
  (let ((pam-patch (origin
   (method url-fetch)
   (uri "https://tools.suckless.org/slock/patches/pam_auth/slock-pam_auth-20190207-35633d4.diff")
   (file-name "slock-pam.diff")
   (sha256 (base32 "0dlhif9vlci9w3cs2wlb73j7s9w3nzxwf5h170ybcywwj7y9gjsc"))
   )))
  (package
   (inherit slock)
   (source (origin (inherit (package-source slock))
		   (patches (list pam-patch))))
   (inputs (modify-inputs (package-inputs slock)
              (append  (specification->package "linux-pam"))))
   )))
(define username "tadhg")
(define (nonguix-subs config) (guix-configuration
               (inherit config)
               (substitute-urls (cons*
		 "https://substitutes.nonguix.org"
                 %default-substitute-urls
	       ))
               (authorized-keys (cons*
		 (origin (method url-fetch)
			 (uri "https://substitutes.nonguix.org/signing-key.pub")
			 (sha256 (base32 "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
		 %default-authorized-guix-keys
	       ))
))

(operating-system
  (kernel linux)
  (kernel-arguments (cons* "i915.enable_psr=0" %default-kernel-arguments))
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_CA.utf8")
  (timezone "America/Toronto")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "framework")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name username)
                  (comment "Tadhg McDonald-Jensen")
                  (group "users")
                  (home-directory "/home/tadhg")
                  (supplementary-groups '("wheel" ;; for sudo access
					  "netdev" ;; NOT SURE
					  "audio" ;; to be able to use alsamixer etc
					  "video"  ;; think this is to control brightness
					  "input" ;; to control caps lock light
					  "lp" ;; for printing / scanning I THINK
					  "transmission" ;; for transmission (torrent)
					  )))
                %base-user-accounts))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot"))
                (keyboard-layout keyboard-layout)))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "c0010d06-0bd1-4ae2-93e6-f2f89a3a670b"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot")
                         (device (uuid "5190-E840" 'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "btrfs")
                         (flags '(lazy-time))
                         (options
                          (alist->file-system-options
                           '(("compress" . "lzo"))))
                         (dependencies mapped-devices)) 
                         %base-file-systems))
  (packages (append
	     (list
	      (specification->package "nss-certs")
	      (specification->package "alacritty")
	      slock-patched)
             %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (cons*
    (service xorg-server-service-type)
    (service cups-service-type
	     (cups-configuration
	      (web-interface? #t)
	      (extensions
	       (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))
    (service nix-service-type)
    (service qemu-binfmt-service-type
         (qemu-binfmt-configuration
           (platforms (lookup-qemu-platforms "arm" "aarch64"))))
    (service bluetooth-service-type)
    (service fprintd-service-type)
    (service tlp-service-type
	     (tlp-configuration
	      (wifi-pwr-on-bat? #f)))
    (service thermald-service-type)
    (service transmission-daemon-service-type
             (transmission-daemon-configuration
              (download-dir "/torrents")))
    (udev-rules-service 'brightnessctl brightnessctl)
    (screen-locker-service slock-patched)
    ;;(udev-rules-service 'ledtrigger ledtrigger-udev)
    (modify-services
        %desktop-services
	(guix-service-type config => (nonguix-subs config))
	(elogind-service-type
	 config =>
	 (elogind-configuration
          (inherit config)
          (handle-lid-switch 'ignore)))
	(delete gdm-service-type)
	(delete screen-locker-service))))
  ;; allow using .local with mdns resolution, used for printer in particular
  (name-service-switch %mdns-host-lookup-nss)
  (pam-services (map (lambda (pam)
		       (if (member (pam-service-name pam) (list "sudo" "login" "polkit-1"))
			   (pam-service
			    (inherit pam)
			    (auth (cons (pam-entry (module (file-append fprintd "/lib/security/pam_fprintd.so"))
						   (control "sufficient"))
					(pam-service-auth pam)))
			   ) pam
		      )) (base-pam-services)))
  )
                      
