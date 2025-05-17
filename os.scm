

;; Tadhg's personal OS definition for framework laptop

;; note about btrfs setup:
;; right now there is a small boot partition and the encrypted btrfs partition
;; I'd like to set it up so it has these subvolumes:
;; /keyfile.cpio (decrypt key for luks partition)
;; /swapfile (having a swapfile in a subvolume stops it from getting snapshotted so having it in root is desirable)
;; /guixroot -> /
;; /guixstore -> /gnu/store (mounted with very high compression, enables reusing store in dualboot)
;; /nixstore -> /nix/store (probably not strictly necessary but it is the principle.)
;; /tadhghome -> /home/tadhg (low compression for faster lookup)
;; /steamhome -> /home/tadhg/.local/share/.guix-sandbox-home (might be desirable to put in own subvolume)

;; there are 3 possible issues with this setup:
;; 1. grub might get confused about where things are and mess up (probably not but will need to check)
;; 2. keyfile may be problematic, I doubt this since grub will be mounting the default partition and will load the .cpio file from there
;; 3. swapfile can't be easily accesssed, might have to mount the root partition somewhere to make it visible.
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
 ((gnu packages linux) #:select (brightnessctl))
 ((gnu packages wm) #:select (swaylock))
 ((gnu packages cups) #:select (cups cups-filters epson-inkjet-printer-escpr hplip-minimal))
 ((gnu services cups) #:select (cups-service-type cups-configuration))
 ((gnu services nfs) #:select (nfs-service-type nfs-configuration))
 ((gnu services desktop) #:select (sane-service-type bluetooth-service-type %desktop-services elogind-service-type elogind-configuration))
 ;;((gnu services docker) #:select(docker-service-type))
 ((gnu services virtualization) #:select(qemu-binfmt-service-type qemu-binfmt-configuration lookup-qemu-platforms libvirt-service-type))
 ((gnu services nix) #:select (nix-service-type))
 ((gnu services networking) #:select (hostapd-service-type hostapd-configuration ipfs-service-type ipfs-configuration))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services sound) #:select (pulseaudio-service-type pulseaudio-configuration))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration))
 ((gnu services xorg) #:select (xorg-server-service-type gdm-service-type screen-locker-service screen-locker-service-type xorg-configuration set-xorg-configuration))
 ;;((gnu services authentication) #:select (fprintd-service-type))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services pm) #:select (tlp-service-type tlp-configuration thermald-service-type))
 (tadhg packagelist)
 ((tadhg channels-and-subs) #:prefix tadhg:)
 ((system-info setup) #:select(wrap-os))
 )

;; stuff for fprintd that I need to submit a patch
;; this adds support for pam modules
(use-modules
 (gnu services)
 (gnu services base)
 (gnu services configuration)
 (gnu services dbus)
 (gnu services shepherd)
 (gnu system pam)
 (gnu system shadow)
 (gnu packages admin)
 (gnu packages freedesktop)
 )

(define-configuration fprintd-configuration
  (fprintd      (file-like fprintd)
                "The fprintd package")
  (pam-modules (list '())
               "list of pam modules to use fprint for"
	       empty-serializer))

(define (fprintd-dbus-service config)
  (list (fprintd-configuration-fprintd config)))

(define (fprintd-pam-service config)
  ;; returns list of pam extension objects
  ;; TODO: extract variables with let
  (if (null? (fprintd-configuration-pam-modules config))
      '() ;; when there are no pam modules specified don't add any transforms
      ;; otherwise we have a transformer
      (list (pam-extension
             (transformer
              (lambda (pam)
                (if (member (pam-service-name pam) (fprintd-configuration-pam-modules config))
                    (pam-service
                     (inherit pam)
		     ;; add fingerprint as sufficient to gain access
		     (auth (cons (pam-entry (module (file-append (fprintd-configuration-fprintd config) "/lib/security/pam_fprintd.so"))
					    (control "sufficient"))
				 (pam-service-auth pam))))
                    pam)))
             (shepherd-requirements '(dbus-system))))))
                 

(define fprintd-service-type
  (service-type (name 'fprintd)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          fprintd-dbus-service)
                       (service-extension polkit-service-type
                                          fprintd-dbus-service)
		       (service-extension pam-root-service-type
					  fprintd-pam-service)))
                (default-value (fprintd-configuration))
                (description
                 "Run fprintd, a fingerprint management daemon.")))

;; (define ledtrigger-udev (udev-rule "90-ledtrigger.rules"
;; "ACTION==\"add\", SUBSYSTEM==\"leds\", RUN+=\"/run/current-system/profile/bin/chgrp input /sys/class/leds/%k/trigger\"
;; ACTION==\"add\", SUBSYSTEM==\"leds\", RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/%k/trigger\""))
				   
;; Note that the wifi connection to CU-Wireless was done with the nm-applet (and guix shell stalonetray) to configure wPA-Enterprise
;; idk where else to put this, wifi info isn't something I expect to ever save to the guix config but want to remember how to do it.
;; also eduroam has an install script for ottawa U, haven't tried it yet since I don't have ottawa U credentials at time of writing

(define hostname "tadhgfrmwrk")
(define root-uuid "")
(define efi-uuid "")
(define resume-offset "")

(define username "tadhg")

(define* (my-os #:key hostname filesystems boot-config)
(operating-system
  (host-name hostname)
  (bootloader boot-config)
  (file-systems filesystems)

  (locale "en_CA.utf8")
  (timezone "America/Toronto")
  (keyboard-layout (keyboard-layout "us"))
  
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments (cons*
		     ;; https://wiki.archlinux.org/title/Intel_graphics#Screen_flickering
		     "i915.enable_psr=0" ;; fixes screen lag / not updating
		     ;; https://wiki.archlinux.org/title/Framework_Laptop_13#12th_.26_13th_gen_brightness_and_airplane_mode_keys
		     "modprobe.blacklist=hid_sensor_hub" ;; makes brightness keys visible to X
		     ;; https://wiki.archlinux.org/title/Framework_Laptop#Intel_Wi-Fi_6E_AX210_reset/_low_throughput_/_%22Microcode_SW_error%22
		     "iwlwifi.disable_11ax=Y" ;; should fix wifi repeatedly dying on me issue
		     ;; morgan recommended this to ensure the power light turns off during suspend
		     "acpi_osi=\"!Windows 2020\"" ; framework laptop suspend issue
		     %default-kernel-arguments))

  ;; The list of user accounts ('root' is implicit).
  (users (cons* 
	  (user-account
                  (name username)
                  (comment "Tadhg McDonald-Jensen")
                  (group "users")
                  (home-directory (string-append "/home/" username))
                  (supplementary-groups '("wheel" ;; for sudo access
					  "netdev" ;; TODO: what is this for?
					  "audio" ;; to be able to use alsamixer etc
					  "video"  ;; think this is to control brightness
					  "scanner" ;; for scanning
					  "input" ;; to control caps lock light
					  "lp" ;; for printing / scanning TODO: confirm this is needed for printing/scanning
					  "transmission" ;; for transmission (torrent)
					  ;;"docker" ;; to use docker commands without sudo
					  "kvm" ;; kvm is a kernel thing for performance with virtualization, probably helps with optimization but wouldn't be strictly necessary, morgan recommended adding myself to this group to save myself hassle when figuring out VM stuff.
					  )))
                %base-user-accounts))

  (packages (append
	     os-packages
             %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (cons*
    
    (udev-rules-service 'steam-devices steam-devices-udev-rules) ;; needed for steam controller to work good.
    ;;(service mpd-service-type ;; TODO: figure out how to actually use this I just stole the config from morgan.
    ;;         (mpd-configuration
    ;;          (user username)
    ;;          (music-directory "~/Music")
    ;;          (playlist-directory "~/.config/mpd/playlists")
    ;;          (db-file "~/.config/mpd/database")
    ;;          (state-file "~/.config/mpd/state")
    ;;          (sticker-file "~/.config/mpd/sticker.sql")))
    (service xorg-server-service-type) ;; needed for display (kind of important)
    (service sane-service-type)
    (service cups-service-type ;; for printing
	     (cups-configuration
	      (web-interface? #t) ;; http://localhost:631
	      (extensions
	       (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))
    (service nix-service-type) ;; used for brave and possibly other packages I can't get on guix.
    ;;(service ipfs-service-type) ;; TODO: ipfs stopped working requiring a repo migration, figure out how to get it to work reliably. 
    (service libvirt-service-type) ;; TODO: figure out exactly what this is for, almost certainly for virtualization but might for qemu or guix cross compile or both
    ;;(service docker-service-type) ;; for docker, used once as a TA to show a student how to use it for a lab.
    (service qemu-binfmt-service-type ;; enables cross compiling for turris or android box
         (qemu-binfmt-configuration
          (platforms (lookup-qemu-platforms "arm" "aarch64"))))
   
    (service bluetooth-service-type) ;; allows bluetooth
    (service fprintd-service-type
	     (fprintd-configuration
	      (pam-modules '("sudo" "login" "polkit-1")))) ;; enable fingerprint for sudo
    (service tlp-service-type ;; enables power optimizations
	     (tlp-configuration
	      (wifi-pwr-on-bat? #f))) ;; tried this to fix wifi issue, don't think it fixed
    (service thermald-service-type) ;; helps with power, I think it does fan control.
    (service transmission-daemon-service-type ;; http://localhost:9091
             (transmission-daemon-configuration ;; guix shell transmission -- transmission-remote ...
              (download-dir "/torrents")))
    (service syncthing-service-type ;; http://localhost:8384
             (syncthing-configuration (user username)))
    (udev-rules-service 'brightnessctl brightnessctl)
    ;;(screen-locker-service slock-patched)
    ;;(udev-rules-service 'ledtrigger ledtrigger-udev)
    (modify-services
        %desktop-services
	(guix-service-type config => (tadhg:substitutes config))
	;; (pulseaudio-service-type config => (pulseaudio-configuration
;; 					    (inherit config)
;; 					    (extra-script-files
;; 					     (list (plain-file "audigy.pa"
;; 							       (string-append "\
;; load-module module-alsa-sink device=hw:0,3
;; load-module module-combine-sinks sink_name=combined\n"))))))
	
	(elogind-service-type
	 config =>
	 (elogind-configuration
          (inherit config)
	  (handle-power-key 'hibernate)
	  ;;(idle-action 'suspend)
          (handle-lid-switch 'hybrid-sleep)
	  ))
	(delete gdm-service-type)
	)))
  ;; allow using .local with mdns resolution, used for printer in particular
  (name-service-switch %mdns-host-lookup-nss)
  ))
                      

(wrap-os my-os)
