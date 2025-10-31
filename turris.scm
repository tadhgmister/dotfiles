;;;; to connect to device while it is plugged in with serial cable
;; guix shell minicom -- sudo minicom -w -b 115200 -D /dev/ttyUSB0
;;;; to disable watchdog timer while in the uboot prompt
;; mw 0xf1020300 0x403
;;;; to continue to normal boot sequence after disabling watchdog
;; run bootcmd
;;;; to pre-build the kernel use:
;; guix build -f turris.scm -L packages --target=arm-linux-gnueabihf --cores=8
;;;; to pre-build the system use:
;; guix system build turris.scm -L packages --target=arm-linux-gnueabihf --max-jobs=8 --cores=2
;;;; and to deploy use:
;; guix deploy turris.scm -L packages

;;;; datasheet for device this is designed for:
;; https://static.turris.com/docs/omnia/omnia2020-datasheet.pdf


;;;;;;; THIS FILE IS A MESS
;; More than half of this file is code that I need to submit as patches to guix to encorperate it into the mainline
;; the operating system declaration, the customization on the linux kernel, the managed machine details, and the bit of code
;; to resolve to either the system or machine list depending on which guix command is run truely belongs in this file
;; variants on packages and the horrible cludge of hacking together the remote-eval to do cross compiling should be contributed to the mainline.

(use-modules
 ((gnu packages linux) #:select(customize-linux))
 (gnu)
 ((gnu machine) #:select(machine))
 ((gnu machine ssh) #:select(managed-host-environment-type machine-ssh-configuration))
 (gnu image)
 ((guix platforms arm) #:select(armv7-linux))
 (gnu system image)
 (guix gexp)
 (guix transformations)
 ((guix packages) #:select (package origin base32 modify-inputs package-arguments package-native-inputs))
 ((guix git-download) #:select(git-reference git-fetch git-file-name))
 ((guix utils) #:select ( substitute-keyword-arguments))
 ;((guix git-download) #:select (git-fetch git-reference git-file-name))
 ;((guix platforms arm) #:select(armv7-linux))

 ((gnu bootloader) #:select(bootloader))
 ((gnu bootloader u-boot) #:select(u-boot-bootloader))

 ((gnu packages ssh) #:select(openssh))
 ((gnu packages scanner) #:select (sane-backends))
 ((gnu packages syncthing) #:select (syncthing))
 ((gnu packages web) #:select(quark))
 ((gnu packages bash) #:select(bash))

 ((gnu services networking)
  #:select(dhcpcd-service-type
	   dhcpcd-configuration
	   hostapd-service-type
	   hostapd-configuration
	   nftables-service-type
	   nftables-configuration
	   dhcpd-service-type
	   dhcpd-configuration))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))
 ((gnu services sysctl) #:select(sysctl-service-type sysctl-configuration sysctl-configuration-settings))
 ((gnu packages bittorrent) #:select(transmission))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration transmission-password-hash))
 ((gnu services desktop) #:select (sane-service-type))
 (gnu services syncthing)

 ((guix records) #:select(define-record-type*))
 ((ice-9 match) #:select(match-lambda))
 ((gnu services shepherd) #:select(shepherd-root-service-type shepherd-service))
 ((srfi srfi-1) #:select(lset-difference))
 ((nongnu packages linux) #:select(atheros-firmware))

 ;;((guix store) #:select(run-with-store with-store))
 ((guix packages) #:select(%current-system))

 ((tadhg quark) #:select(quark-configuration quark-service-type))
 ((tadhg webgallery) #:select(gallery-folder))

 (gnu machine)
 (guix monads)
 (guix modules)
 (gnu machine ssh)
 (guix remote)
 (guix store)
 (guix ssh)
 ((srfi srfi-1) #:select(append-map))
 (guix derivations)
 ((guix packages) #:select(default-guile-derivation))
 )
(define deploying-system (%current-system))
;; this takes something that has a suitable gexp compiler (package, operating-system, computed file etc)
;; and returns a derivation (which can be used in its place) that is natively compiled for arm
(define (force-native-compiled pkg)
  (with-store store (run-with-store store (lower-object pkg "armhf-linux"))))
;; see force-native-compiled, this is like that but forces it to be cross compiled.
;; this is used when running `guix system build` on this file so it doesn't matter if you specify --target or not it just cross compiles it.
(define (force-cross-compiled pkg)
  (with-store store (run-with-store store (lower-object pkg deploying-system #:target "arm-linux-gnueabihf"))))


(define syncthing-able-to-cross-compile
  (package
    (inherit syncthing)
    
    (arguments
     (substitute-keyword-arguments (package-arguments syncthing)
       ((#:phases phases)
        #~(modify-phases #$phases
           (replace 'build
             (lambda _
               (with-directory-excursion "src/github.com/syncthing/syncthing"
                 ; Build the primary Syncthing executable
                 ; Build utilities used to run an independent Syncthing network
                 (for-each (cut invoke "go" "run" "build.go" "build" <>)
                           '("syncthing" "stcrashreceiver" "strelaypoolsrv" "stupgrades"
                             "ursrv" "stdiscosrv" "strelaysrv")))))

           (replace 'install
             (lambda _
               ;; (with-directory-excursion "src/github.com/syncthing/syncthing/bin"
               ;;   (install-file "syncthing" (string-append #$output "/bin"))
               ;;   (for-each (cut install-file <> (string-append #$output:utils "/bin/"))
               ;;             '("stdiscosrv" "strelaysrv")))
               (with-directory-excursion "src/github.com/syncthing/syncthing"
                 (install-file "syncthing" (string-append #$output "/bin"))
                 (for-each (cut install-file <> (string-append #$output:utils "/bin/"))
                           '("ursrv" "stupgrades" "strelaypoolsrv" "stcrashreceiver" "stdiscosrv" "strelaysrv")))))))))))



;;;; Transmission headless

;; todo at least extract this to another file or preferably submit as patch to guix

(use-modules 
	     ;(guix packages)
             ((gnu packages bittorrent) #:select(transmission))
	     ((gnu packages bash) #:select(bash-minimal))
	     ((gnu packages curl) #:select(curl))
	     ((gnu packages libevent) #:select(libevent))
	     ((gnu packages tls) #:select(openssl))
	     ((gnu packages python) #:select(python))
	     ((gnu packages compression) #:select(zlib))
	     ;(guix utils)
					;(guix gexp)
)
(define transmission-headless
  (package
    (inherit transmission)
    (name "transmission-headless")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments transmission)
       
       ((#:configure-flags flags)
        #~(append
           #$flags
           '("-DENABLE_GTK=OFF" "-DENABLE_MAC=OFF" "-DENABLE_QT=OFF")))
	
       ((#:phases phases)
        #~(modify-phases #$phases
	    (replace 'move-gui
	      (lambda* (#:key outputs #:allow-other-keys)
               (mkdir-p (string-append #$output:gui "/bin"))))
	    (delete 'wrap-program)))))
    (inputs (list bash-minimal
                  curl
                  ;(list glib "bin")
                  ;gtkmm
                  ;libappindicator
                  libevent
                  openssl
                  python
                  zlib))
    ))



;; (define-record-type* <startupscript-configuration>
;;   startupscript-configuration make-startupscript-configuration
;;   startupscript-configuration?
;;   (settings startupscript-configuration-settings  ; alist of string pairs
;;             (default '())))

;; (define startupscript-shepherd-service
;;   (match-lambda
;;     (($ <startupscript-configuration> settings)
;;        (shepherd-service
;;         (documentation "turns on user led light at startup")
;;         (provision '(startupscript))
;;         (start #~(lambda _
;;                    (call-with-output-file "/sys/class/leds/omnia-led:user1/color"
;;                      (lambda (port)
;;                        (display "255 60 0" port)))
;;                    (call-with-output-file "/sys/class/leds/omnia-led:user1/brightness"
;;                      (lambda (port)
;;                        (display "255" port)))))
;;         (one-shot? #t)))))
;; (define startupscript-service-type
;;   (service-type
;;    (name 'startupscript)
;;    (extensions
;;     (list (service-extension shepherd-root-service-type
;;                              (compose list startupscript-shepherd-service))))
;;    ;; (compose concatenate)
;;    ;; (extend (lambda (config settings)
;;    ;;           (sysctl-configuration
;;    ;;            (inherit config)
;;    ;;            (settings (append (sysctl-configuration-settings config)
;;    ;;                              settings)))))
;;    (default-value (startupscript-configuration))
;;    (description "turns on the omnia led at startup to see if it is loading guix")))


(define HOSTNAME "omniaguix")
(define disk-uuid "95e78a3a-7f1d-47d3-96ef-c02d95e78a3a")
(define sshport 2222)

(define make-linux-libre* (@@ (gnu packages linux) make-linux-libre*))

(define KERNEL-VERSION "6.12.10")
(define linux-non-libre-source
  ((@@ (gnu packages linux) %upstream-linux-source) KERNEL-VERSION (base32 "15xjjn8ff7g9q0ljr2g8k098ppxnpvxlgv22rdrplls8sxg6wlaa")))


;; turris-kernel.config is pulled directly from turris os under /proc/config.gz
;; this has things like the gcc version that was used and such and we do not want to keep them
;; so what we want to do is run `make savedefconfig` to minify that to an effective defconfig
;; then add our options to that defconfig, then recompute the full config file
;; and ideally get it to yell at us if something we specified doesn't end up in the final config because linux
;; is happy to just drop settings if you didn't specify the right dependencies
;;
;; the officially supported workflow for this is that you use customize-linux where the base config file is specified
;; in the kernel being customized and the extra options are passed to customize-linux
;; this is because the base kernel will logically have a full configuration file and if you don't specify a defconfig to customize
;; then it needs to use the config from the base kernel and thus has to first minify it, thus getting the workflow we intend.
(define kernel-base
  (make-linux-libre* KERNEL-VERSION "IGNORED_VARIABLE" linux-non-libre-source '("armhf-linux")
                     #:configuration-file (lambda _ (local-file "turris-kernel.config"))))

(define kernel-to-use
  (customize-linux #:name "omnia-arm"
                   #:linux kernel-base
                   #:configs ((@@ (gnu packages linux) config->string)
                              '(
                                ;; is critically needed, guix gzips its initrd
                                ("CONFIG_RD_GZIP" . #t)
                                ;; wifi driver dependencies
                                ("CONFIG_CFG80211" . m)
                                ("CONFIG_MAC80211" . m)
                                ("CONFIG_WLAN_VENDOR_ATH" . #t)
				;; for WLE200N2   Qualcomm Atheros AR9287 802.11bgn
                                ("CONFIG_ATH9K" . m)
				;; for WLE900VX   Qualcomm Atheros QCA986x/988x 802.11ac
				;; TODO: this is loaded as a kernel module and lspci lists the chip but it doesn't show up as a network interface?
                                ("CONFIG_ATH10K" . m)
                                ("CONFIG_ATH10K_PCI" . m)

				;;; options enabled by guix normally but did not work when I tried to cross compile
                                ;;("CONFIG_HAVE_GCC_PLUGINS" . #t)
                                ;;("CONFIG_GCC_PLUGINS" . #t)
				;;("CONFIG_GCC_PLUGIN_LATENT_ENTROPY" . #t)
				
                                ;;; options that are not in the turris
				;;; os config but probably got
				;;; installed as seperate modules with
				;;; their package manager
				;;; TODO: investigate these options
                                ;; ("CONFIG_CZNIC_PLATFORMS" . #t)
                                ;; ("CONFIG_TURRIS_OMNIA_MCU" . m)
                                ;; ("CONFIG_TURRIS_OMNIA_MCU_GPIO" . #t)
                                ;; ("CONFIG_TURRIS_OMNIA_MCU_SYSOFF_WAKEUP" . #t)
                                ;; ("CONFIG_TURRIS_OMNIA_MCU_WATCHDOG" . #t)
                                ;; ("CONFIG_TURRIS_OMNIA_MCU_TRNG" . #t)
                                ))))


(define-public (substitutes guixconfig)
  (guix-configuration
   (inherit guixconfig)
   (authorized-keys
    (cons*
     (local-file "/etc/guix/signing-key.pub")
     %default-authorized-guix-keys))))



(define tadhg-laptop-syncthing-device
  (syncthing-device
   (name "tadhgfrmwrk")
   (id "26OJPES-FUQFPGY-3FACMAQ-LZLDUCI-JKKUX6J-QHKRK4W-7OXVBA4-E7TG6QF")))


(define QUARK_FILES (file-union "pictures"
			    (list
			     (list "randomHumour" (gallery-folder "/home/tadhg/Pictures/funny/"))
			     (list "huaweibackup" (gallery-folder "/home/tadhg/Pictures/huawaiBackup/"))
			     (list "ciaranBrunelleCapeCod" (gallery-folder "/home/tadhg/Pictures/ciaranTrip/")))))

(define my-hostapd-config (hostapd-configuration
		(interface "wlp3s0")
		(ssid "Tadhg's Router")
		(driver "nl80211")
		(channel 0);;"acs_survey") ;; supposedly 0 and "acs_survey" should do the same thing, I'd rather be more explicit about it but will have to patch guix to allow that symbol
		(extra-settings "
hw_mode=g
country_code=CA
macaddr_acl=0
auth_algs=1
wpa=2
wpa_psk=ac9ce402606b4f9281ee1801e3f6262803d105e4273803896d076a845f7a0ac8
wpa_key_mgmt=WPA-PSK
wpa_pairwise=TKIP
rsn_pairwise=CCMP")))
;; note the second domain name server is what shows up when i run
;; "nmcli device show wlp166s0" on my laptop (IP4.DNS[2] entry)
;; think it is decided by the bell modem, ideally I could specify to use whatever the local system sees as the viable domain name servers via dhcpcd (client) to the parent modem
(define dhcpd.conf (plain-file "dhcpd.conf" "
subnet 192.168.80.0 netmask 255.255.255.0 {
    range 192.168.80.10 192.168.80.100;
    option routers 192.168.80.1;
    option domain-name-servers 192.168.2.1, 207.164.234.193, 8.8.8.8;
}
subnet 192.168.81.0 netmask 255.255.255.0 {
    range 192.168.81.10 192.168.81.100;
    option routers 192.168.81.1;
    option domain-name-servers 192.168.2.1, 207.164.234.193, 8.8.8.8;
}
"))
;; network address translation (NAT) after deciding where to go if the packet is going to eth2 (the WAN port for external internet) 'masquerade' so it looks like the packet came from this device
;; filter to drop packets unless it is from WAN going to external or is reply that is established/related to a request that already went out.
(define nftables.conf (plain-file "nftables.conf" "
table ip nat {
    chain prerouting {
        type nat hook prerouting priority 0;
    }

    chain postrouting {
        type nat hook postrouting priority 100;
        oifname \"eth2\" masquerade
    }
}

table ip filter {
    chain forward {
        type filter hook forward priority 0;
        policy drop;

        # Allow established/related packets back in
        ct state established,related accept

        # Allow forwarding from anything to WAN
        oifname \"eth2\" accept
        # Except from WAN to WAN
        iifname \"eth2\" oifname \"eth2\" drop
    }
}
"))
(define my-system
  (operating-system
    (kernel kernel-to-use)
    (firmware (cons* atheros-firmware
                     %base-firmware))

    (initrd-modules (lset-difference equal? ((@@ (gnu system linux-initrd) default-initrd-modules) "armhf-linux")
                                     '("usb-storage" ;; baked into kernel
                                       "hid-apple" ;; we aren't going to use apple products as input device
                                       "virtio_console" ;; we are not in a vm
                                       "virtio-rng" ;; we are not a vm
                                       "btrfs" ;; baked into kernel
                                       )))
    (kernel-arguments (list
		       ;; TODO these are actually baked into the kernel according to the turris-kernel.config file, may be able to drop these
                       "earlyprintk"
                       "console=ttyS0,115200"
		       
                       ;;"pcie_aspm=no"
                       ;;"modprobe.blacklist=pcieaspm";;,usbmouse,usbkbd"
                       ))
    (host-name HOSTNAME)
    (timezone "America/Toronto")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (timeout 1)
                 ;; the uboot bootloader inherits from extlinux which the turris uboot can load
                 ;; but I don't want to have guix reflashing the uboot on every reconfigure so we
		 ;;just leave the targets empty
                 (targets '())))
    (file-systems (cons (file-system
                          (mount-point "/")
                          (device (uuid disk-uuid))
                          (type "btrfs"))
                        %base-file-systems))
    (users (cons*
	    ;;;; sftp still allows accessing root directory, and using chroot breaks all of guix symlinks
	    (user-account
	      (name "guest")
	      (comment "external access")
	      (group "sftpusers")
	      ;(shell (file-append bash "/bin/rbash"))
	      (password (crypt "sharing" "$6$abc"))
	      (home-directory "/home/share"))
	    (user-account
	     (name "tadhg")
	     (comment "Tadhg McDonald-Jensen")
	     (group "users")
	     (supplementary-groups '("wheel"))
	     (home-directory "/home/tadhg"))
	    %base-user-accounts))
    (groups (cons*
	     (user-group (name "sftpusers"))
	     %base-groups))

    ;; (sudoers-file
    ;;       (plain-file "sudoers"
    ;;                   (string-append (plain-file-content %sudoers-specification)
    ;;                                  "tadhg ALL = NOPASSWD: ALL\n")))
    ;; (packages
    ;;  (specifications->packages
    ;;   '("sane-backends")))
      
    ;; allow using .local with mdns resolution, used for printer in particular
    (name-service-switch %mdns-host-lookup-nss)
    (services
     (cons*
      ;;;;; all the networking related services needed to host a wfii network (except for sysctl to enable ipv4 port forwarding which is at the bottom in modify-services)
      
      ;; allow connecting to this device so guix deploy works
      (service openssh-service-type
               (openssh-configuration
                (permit-root-login #t)
		;;(password-authentication? #f)
		(public-key-authentication? #t)
		(extra-content "AllowUsers tadhg root@192.168.*.*")
                (authorized-keys
                 `(("root" ,(local-file "deploy_id.pub"))
                   ("tadhg" ,(local-file "deploy_id.pub"))))
                (port-number sshport)))
      
      ;; use wifi card for hosting a wifi network
      (service hostapd-service-type
	       my-hostapd-config)
      ;; setup filtering to make traffic for connected devices to appear as it comes from us and then forward it back
      (service nftables-service-type
	       (nftables-configuration
		 (ruleset nftables.conf)))
      ;; issue ip addresses to our connected clients
      (service dhcpd-service-type
	       (dhcpd-configuration
		 (interfaces '("lan1" "wlp3s0"))
		 (config-file dhcpd.conf)))
      ;; automatically aquire ip address over WAN ethernet port
      (service dhcpcd-service-type
	       (dhcpcd-configuration
		 (interfaces (list "eth2"))))
      ;; give ourselves an ip address in the address space of the connected devices
      ;; this address matching with the "routers" field of dhcpd is how it decides to allocate addresses I think?
      (service static-networking-service-type
               (list
		(static-networking
		  (provision '(self-wifi-address))
                  (addresses
                   (list (network-address
                           (device "wlp3s0")
                           (value "192.168.80.1/24")))
		   ))
		(static-networking
		  (provision '(lan1))
		  (addresses
		   (list (network-address
			   (device "lan1")
			   (value "192.168.81.1/24")))))
		;; (static-networking
		;;   (provision '(lan-bridge))
		;;   (addresses
		;;    (list (network-address
		;; 	   (device "br0")
		;; 	   (value "192.168.81.1/24"))))
		;;   (links
		;;    (cons*
		;;     (network-link
		;;       (name "br0")
		;;       (type 'bridge)
		;;       (arguments '((up . #t))))
		;;     (map (lambda (iname)
		;; 	   (network-link
		;; 	     (name iname)
		;; 	     (arguments '((master . "br0") (up . #t)))))
		;; 	 '("lan0" "lan1" "lan2" "lan3" "lan4")))))
		))


      ;;;;; syncing and sharing services
      
      (service quark-service-type
	       (quark-configuration
		;; TODO actually pack together the edits as a diff and commit to this repo
		;; Basically it is just edited to drop the chroot as that breaks symlinks to guix store
	        (quark ((options->transformation
			 '((with-source . "quark=/home/tadhg/src/quark")))
			quark))
		(port 8000)
		(directory QUARK_FILES)))
      
      (service syncthing-service-type
	    (syncthing-configuration
	     (syncthing syncthing-able-to-cross-compile)
	     (user "tadhg")
	     (config-file
	      (syncthing-config-file
	       (auto-upgrade-interval-hours 0) ;; disable auto upgrade as it can't modify the files in guix store anyway
	       (gui-address "0.0.0.0:8384")
	       (gui-user "torrenting")
	       ;; is the same username and password as transmission, probably not the most secure but better than nothing
	       (gui-password "$2y$04$Mqejsu/.hPC37YQTo0QB5.iy5mxcOacuu1LBNybb/oGQxyahxIx7O")
	       (folders
		(list
		 (syncthing-folder
		  (label "org")
		  (id "zjrm2-36cqv")
		  (path "/home/tadhg/org")
		  (devices (list tadhg-laptop-syncthing-device))
		  ) ; end org folder
		 (syncthing-folder
		  (label "huaweibackup")
		  (path "/home/tadhg/Pictures/huawaiBackups/")
		  (devices (list tadhg-laptop-syncthing-device))
		  (id "frwii-bbaeq"))
		 )) ;; end list of folders
		
	       )))) ;;end syncthing configuration and service
      (service transmission-daemon-service-type
               (transmission-daemon-configuration
		(transmission transmission-headless)
          ;; Restrict access to the RPC ("control") interface
          (rpc-authentication-required? #t)
          (rpc-username "torrenting")
          (rpc-password
           (transmission-password-hash
            "torrenting" ; desired password
            "uKd1uMs9"))   ; arbitrary salt value

          ;; Accept requests from this and other hosts on the
          ;; local network
          (rpc-whitelist-enabled? #t)
          (rpc-whitelist '("::1" "127.0.0.1" "192.168.2.*")) ;; allow local connections (if done while ssh'd into router) or on local network

	  (download-dir "/home/torrents")
	  (incomplete-dir-enabled? #t)
	  (incomplete-dir "/var/lib/transmission-daemon/downloads/")
	  (lpd-enabled? #t) ;; try to find peers on local network so copying torrent from a laptop to turris is fast
	  (ratio-limit-enabled? #t)
	  (ratio-limit 10.0)))


      
      (modify-services
          %base-services
	(sysctl-service-type config =>
                       (sysctl-configuration
                         (settings (append '(("net.ipv4.ip_forward" . "1"))
                                           (sysctl-configuration-settings config)))))
        (guix-service-type config => (substitutes config)))))))



;;;;;; Start of cludge to do cross compiling
;;; this is a copy of the remote-eval from (guix remote) and of
;;; managed-host-remote-eval from (gnu machine ssh) that enables cross
;;; compiling the relevant stuff. The target system and the current
;;; system are hard coded into here because more changes would be
;;; needed in (gnu machine ssh) to do it properly but specifying the
;;; #:system as the machine running deploy and #:target as the deploy
;;; target both passed to lower-gexp in the remote eval function that
;;; makes it all work good, however this gets wrapped in parameterize
;;; for the %current-system by the code in (gnu machine ssh) that
;;; maybe causes weird behaviour.

;; this reloads the guix remote module because for some reason trampoline and %remote-eval don't load without that
(reload-module (resolve-module '(guix remote)))
(define %remote-eval (@@ (guix remote) %remote-eval))
;(define trampoline (@@ (guix remote) trampoline))
(define* (edited-remote-eval exp session
                      #:key
                      (build-locally? #t)
                      target-system
                      target
                      (module-path %load-path)
                      (socket-name (%daemon-socket-uri))
                      (become-command #f))
  "Evaluate EXP, a gexp, on the host at SESSION, an SSH session.  Ensure that
all the elements EXP refers to are built and deployed to SESSION beforehand.
When BUILD-LOCALLY? is true, said dependencies are built locally and sent to
the remote store afterwards; otherwise, dependencies are built directly on the
remote store."
  (mlet* %store-monad ((guile-to-use (default-guile-derivation target-system))
		       (lowered (lower-gexp ((@@ (guix remote) trampoline) exp)
					    #:system deploying-system
                                            #:target target
                                            #:guile-for-build guile-to-use
                                            #:module-path %load-path))
                       (remote -> (connect-to-remote-daemon session
                                                            socket-name)))
    (define inputs
      (cons (lowered-gexp-guile lowered)
            (lowered-gexp-inputs lowered)))

    (define sources
      (lowered-gexp-sources lowered))

    (if build-locally?
        (let ((to-send (append (append-map derivation-input-output-paths
                                           inputs)
                               sources)))
          (mbegin %store-monad
            (built-derivations inputs)
            ((store-lift send-files) to-send remote #:recursive? #t)
            (return (close-connection remote))
            (return (%remote-eval lowered session become-command))))
        (let ((to-send (append (map (compose derivation-file-name
                                             derivation-input-derivation)
                                    inputs)
                               sources)))
          (mbegin %store-monad
            ((store-lift send-files) to-send remote #:recursive? #t)
            (return (build-derivations remote inputs))
            (return (close-connection remote))
            (return (%remote-eval lowered session become-command)))))))
(define (edited-managed-host-remote-eval machine exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'managed-host."
  ((@@ (gnu machine ssh) maybe-raise-unsupported-configuration-error) machine)
  (let ((config (machine-configuration machine)))
    (edited-remote-eval exp ((@@ (gnu machine ssh) machine-ssh-session) machine)
                 #:build-locally?
                 (machine-ssh-configuration-build-locally? config)
                 #:target-system
                 ((@@ (gnu machine ssh) machine-ssh-configuration-system) config)
                 #:target "arm-linux-gnueabihf"
                 #:become-command
                 ((@@ (gnu machine ssh) machine-become-command) machine))))

(define my-edited-managed-host-environment-type
  (environment-type
    (inherit managed-host-environment-type)
    (machine-remote-eval edited-managed-host-remote-eval)))
;;;; helper to debug configuration, when building this package it intentionally crashes after configuration so
;;;; with --keep-failed the confuration used can be inspected
;;;; probably not useful anymore now that I am using the customize linux script which fails if the configuration doesn't match
;;;; what is explicitly specified
;; (use-modules (guix utils) (guix packages))
;; (package
;;   (inherit kernel-to-use)
;;   (arguments
;;      (substitute-keyword-arguments
;;          (package-arguments kernel-to-use)
;;        ((#:phases phases)
;;         #~(modify-phases #$phases
;;             (add-after 'configure 'crash
;;            (lambda _ (error "this is intentional"))))))))


;; (image
;;  (format 'disk-image)
;;  (platform armv7-linux)
;;  (partition-table-type 'gpt)
;;  (operating-system  my-system)
;;  (partitions
;;   (list
;;    (partition
;;     (size 'guess)
;;     (label root-label)
;;     (uuid disk-uuid)
;;     (file-system "btrfs")
;;     (flags '(boot))
;;     (initializer (gexp initialize-root-partition))))))

;;; TODO: figure out what module makes 'machine' a thing and import it so I can leave this uncommented when using this file for non deploy commands
(define turris-to-deploy-to (list (machine
       (operating-system my-system)
       (environment my-edited-managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "192.168.80.1")
                       (system "armhf-linux")
                       ;(target "arm-linux-gnueabihf")
                       (user "root")
                       (identity "~/.ssh/id_ed25519")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBPLIU+lgpp4eOZOqDtm5t94DbuRODG/rWEmCSXVY9wa root@(none)")
                       (port sshport)
                       )))))
(define COMMAND_BEING_RUN (cadr (command-line)))

(define val (cond
	     ((string=? COMMAND_BEING_RUN "deploy") turris-to-deploy-to)
	     ((string=? COMMAND_BEING_RUN "system") my-system)
	     ((string=? COMMAND_BEING_RUN "build") kernel-to-use)
	     (#t turris-to-deploy-to)))

val
