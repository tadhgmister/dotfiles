;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;; TODO: clean up imports
(use-modules (gnu home)
	     ((gnu packages xorg) #:select (xrdb xinit))
	     ((gnu packages xdisorg) #:select(redshift))
	     ((gnu packages suckless) #:select (slstatus))
	     ((gnu packages python) #:select (python))
	     ((guix gexp) #:select (gexp))
	     ((guix build-system copy) #:select (copy-build-system))
	     ((guix build-system trivial) #:select (trivial-build-system))
	     ((guix transformations) #:select (options->transformation))
	     ((guix packages) #:select (package origin base32 package-version package-source))
	     ((guix download) #:select (url-fetch))
	     ((guix git-download) #:select(git-fetch git-reference git-file-name))
	     ((gnu packages suckless) #:select (dwm slock))
	     (gnu home services)
	     ((gnu home services guix) #:select (home-channels-service-type))
	     ((guix channels) #:select (channel make-channel-introduction openpgp-fingerprint %default-channels))
	     ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
             (gnu packages)
	     ((guix packages) #:select (modify-inputs package-inputs))
             (gnu services)
             (guix gexp)
             (gnu home services shells)
	     (gnu home services xdg))



;; imports just for minecraft, need to move to seperate module
(use-modules
 ((guix build-system cmake) #:select (cmake-build-system))
 ((guix licenses) :prefix license:)
 ((nonguix licenses) :prefix non-license:)
 ((gnu packages java) #:select(icedtea))
 ((gnu packages compression) #:select (zlib))
 ((gnu packages pulseaudio) #:select (pulseaudio))
 ((gnu packages qt) #:select(qtbase-5 qtwayland))
 ((gnu packages xorg) #:select (xrandr libxcursor libxrandr libxext libx11 libxxf86vm))
 ((gnu packages gl) #:select (mesa))
 )
;; key extracted from official multimc binary
;;(define MSA-key "c158633f-4040-414a-b5fa-6d47a62424fd")
;; my personal key
(define MSA-key "eeaba6f5-0911-4462-a61b-0e6a08911986")
(define multimc
  (package
    (name "multimc")
    (version "0.6.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MultiMC/MultiMC5.git")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kc28wx0rn8qgqdpfks0hwbq7mckad6p1ikqlgi7zigixg0zaa1y"))))
    (build-system cmake-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:tests? #f                      ; Tests require network access
       #:configure-flags '("-DMultiMC_LAYOUT=lin-system")
       #:phases
       (modify-phases %standard-phases
		      (add-after 'unpack 'add-api-key
				 (lambda* (#:key inputs #:allow-other-keys)
					  (substitute* "notsecrets/Secrets.cpp"
						       (("QString MSAClientID = \"\";")
							(string-append "QString MSAClientID = \"" ,MSA-key "\";")))))
							
         (add-after 'install 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin"))
                    (exe            (string-append bin "/DevLauncher"))
                    (qtwayland      (assoc-ref inputs "qtwayland"))
                    (xrandr         (assoc-ref inputs "xrandr"))
                    (jdk            (assoc-ref inputs "jdk")))
               (wrap-program exe
                 `("PATH" ":" prefix (,(string-append xrandr "/bin")
                                      ,(string-append jdk "/bin")))
                 `("QT_PLUGIN_PATH" ":" prefix (,(string-append
                                                  qtwayland "/lib/qt5/plugins")))
                 `("GAME_LIBRARY_PATH" ":" prefix
                   (,@(map (lambda (dep)
                             (string-append (assoc-ref inputs dep)
                                            "/lib"))
                           '("libx11" "libxext" "libxcursor"
                             "libxrandr" "libxxf86vm" "pulseaudio" "mesa")))))
               #t)))
        )))
    (inputs
     `(("jdk" ,icedtea "jdk")
       ("zlib" ,zlib)
       ("qtbase" ,qtbase-5)
       ("qtwayland" ,qtwayland)
       ("xrandr" ,xrandr)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxcursor" ,libxcursor)
       ("libxrandr" ,libxrandr)
       ("libxxf86vm" ,libxxf86vm)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)))
    (home-page "https://multimc.org/")
    (synopsis "Launcher for Minecraft")
    (description
     "This package allows you to have multiple, separate instances of
Minecraft and helps you manage them and their associated options with
a simple interface.")
    (license (list license:asl2.0        ; MultiMC
                   license:lgpl2.1       ; Qt 5
                   license:lgpl3+        ; libnbt++
                   license:gpl2+         ; rainbow (KGuiAddons), Quazip, Pack200
                   license:silofl1.1     ; Material Design Icons
                   license:expat         ; lionshead, MinGW runtime
                   license:public-domain ; xz-minidec
                   license:isc           ; Hoedown
                   license:bsd-3         ; ColumnResizer
                   ;; Batch icon set:
                   (non-license:nonfree "file://COPYING.md")))))


;; TODO: probably remove this, and then move single-script-package to somewhere other than middle of home config
;; (define* (mixed-text-file-executable name #:key guile #:rest text)
;;   "Return an object representing store file NAME containing TEXT.  TEXT is a
;; sequence of strings and file-like objects, as in:

;;   (mixed-text-file \"profile\"
;;                    \"export PATH=\" coreutils \"/bin:\" grep \"/bin\")

;; This is the declarative counterpart of 'text-file*'."
;;   (define build
;;     (let ((text (if guile (drop text 2) text)))
;;       (gexp (call-with-output-file (ungexp output "out")
;;               (lambda (port)
;;                 (set-port-encoding! port "UTF-8")
;;                 (display (string-append (ungexp-splicing text)) port)
;; 		(chmod port #o555))))))
;;   (computed-file name build #:guile guile))

;; (define brightness-script (mixed-text-file-executable "brctl"
;;    "#!/bin/sh\n"
;;    "echo $1 > /sys/class/backlight/intel_backlight/brightness"
;;    ))

;;;;; DevLauncher -d ~/Documents/minecraft -o -a lordtadhg -l trol -n lordtadhg
;; (define brightness-script (program-file "brctl"
;;    '(let ((outp (open-file "/sys/class/backlight/intel_backlight/brightness" "w")))
;;       (display (list-ref (command-line) 1) outp)
;;       (close outp)
;;     )))
(define* (single-script-package name #:rest text)
  "this takes gexp strings to form a plain text script (should start with #!/bin/sh)
   and compiles the gexp parts into a whole script and resolves to a package that puts
   that file in a /bin folder and is marked executable, this way it can be added to the profile
   to get the script executable."
  (package (name name)
	   (version "1.0")
	   (synopsis name)
	   (description "script made with single-script-package")
	   (license #f)
	   (home-page "")
	   (build-system copy-build-system)
	   (source (computed-file name #~(begin
	      (mkdir #$output)
	      (mkdir (string-append #$output "/bin"))
	      (call-with-output-file
	       (string-append #$output "/bin/" #$name)
	       (lambda (port)
		(display (string-append #$@text) port)
		(chmod port #o555)))
	   )))
))
(define mclauncher-package (single-script-package "multimc"
	"#!/bin/sh\n"
	multimc "/bin/DevLauncher -d ~/Documents/minecraft "
	"-o -a lordtadhg -l trol -n lordtadhg"))


(define guix-manager-package
  (let* ((STAGE "git -C ~/src/dotfiles add -u")
	 (CPDWM "git -C ~/src/dwm/ diff > ~/src/dotfiles/dwm_personal.diff")
	 (HOME "guix home reconfigure ~/src/dotfiles/home-config.scm")
	 (OS "sudo guix system reconfigure ~/src/dotfiles/os.scm")
	 (COMMIT "git -C ~/src/dotfiles/ commit")
	 (OSBU "guix system build ~/src/dotfiles/os.scm")
	 (PULL "guix pull")
	 (ICEDOVE "icedove -ProfileManager")
	 )
    (single-script-package "guixman"
  "#!/bin/sh\n"
  "case $1 in\n"
  "  \"home\" )\n"
  "    " CPDWM "; " HOME " && " STAGE ";;\n"
  "  \"os\" )\n"
  "    " OS " && " STAGE ";;\n"
  "  \"commit\" )\n"
  "    " COMMIT ";;\n"
  "  \"full\" )\n"
  "    " CPDWM "; " COMMIT "; " PULL "&&"OSBU "&&" HOME "&&" ICEDOVE ";;\n"
  "  *)\n"
  "   echo invalid command, see following;\n"
  "   cat $(which $0);;\n"
  "esac\n"
  )))


(define worktimer-package (single-script-package "worktimer"
    "#!/bin/sh\n"
    "echo 1 > /sys/class/leds/input2\\:\\:capslock/brightness\n"
    "sleep $1\n"
    "echo 0 > /sys/class/leds/input2\\:\\:capslock/brightness\n"))
(define alarm-noise-file (origin
   (method url-fetch)
   (uri "http://freesoundeffect.net/sites/default/files/kitchen-timer-616-sound-effect-92324830.mp3")
   (file-name "timer-ring.mp3")
   (sha256 (base32 "0fcvg881ibr0ys734zynsl3q6g6rsi1piig0vxb9yimsxz9hxha9"))
   ))
(define playtimer-package (single-script-package "playtimer"
    "#1/bin/sh\n"
    "worktimer $1\n"
    "echo timer is done, stop what you are doing. Look away from the screen. Set another timer | festival --tts\n"
    ;;"mpv --loop=3 " alarm-noise-file "\n"
    ))
(define brctl-package (single-script-package "brctl"
   "#!/bin/sh\n"
   "echo $1 > /sys/class/backlight/intel_backlight/brightness"
   ))
(define sims3-package (single-script-package "sims3"
   "#!/bin/sh\n"
   "wine ~/.wine/drive_c/Program\\ Files/Electronic\\ Arts/The\\ Sims\\ 3/Game/Bin/TS3W.exe\n"
   ))
(define headphones-package (single-script-package "headphones"
   "#!/bin/sh\n"
   "if [ $# -eq 0 ]; then\n"
   "  guix shell pavucontrol -- pavucontrol & \n"
   "  guix shell bluez -- bluetoothctl power on\n"
   "  guix shell bluez -- bluetoothctl connect 10:AC:DD:E6:97:45\n"
   "fi\n"
   "if [ $# -eq 1 ]; then\n"
   "  guix shell bluez -- bluetoothctl power off\n"
   "fi\n"
 ))
(define ciarancostume-package (single-script-package "ciarancostume"
   "#!/bin/sh\n"
   "cd ~/Documents/\n"
   "  guix shell bluez -- bluetoothctl power on\n"
   "  guix shell bluez -- bluetoothctl connect 75:D2:98:42:EA:A4\n"
   "python3 ciaransoundthing.py &\n"
   "while [ 1 -le 3 ];do python3 -m ble_serial -d E1:CF:11:21:DF:BE; done\n"
))
(define slstatus-patched
  (package
   (inherit slstatus)
   (source (origin
	    (inherit (package-source slstatus))
	    (patches (list (local-file "./slstatus_personal.diff")))
	))    
   ))

(define dwm-patched
  (package
   (inherit dwm)
   (version "6.4")
   (source (origin
             (method git-fetch)
             (uri (git-reference (url "https://git.suckless.org/dwm")
                                 (commit version)))
	     (file-name (git-file-name "dwm" version))
             (sha256 (base32 "025x6rbw61c8l3dsdlkb6wawp8236wy0314jlsxi1jyxnfbml4ds"))
	     (patches (list
		       (origin (method url-fetch)
	                (uri "https://dwm.suckless.org/patches/holdbar/dwm-holdbar-modkey-pertag-nobar-6.2.diff")
	                (sha256 (base32 "0hymhhp2w3rx3006dxwblf7lh4yq3bi958r0qj1x4aszkvdzx1f6")))
		       (origin (method url-fetch)
	                (uri "https://dwm.suckless.org/patches/actualfullscreen/dwm-actualfullscreen-20211013-cb3f58a.diff")
	                (sha256 (base32 "0882k8w6651c18ina0245b558f1bvqydcycw07lp711hpbg7f9gv")))
		       (origin (method url-fetch)
			(uri "https://dwm.suckless.org/patches/hide_vacant_tags/dwm-hide_vacant_tags-6.3.diff")
			(sha256 (base32 "0c8cf5lm95bbxcirf9hhzkwmc5a690albnxcrg363av32rf2yaa1")))
		       ;; TODO: is there a better way to store this?
		       (local-file "./dwm_personal.diff")
		       ))))))

;; TODO: probably clean up this definition slightly, is kind of unreadable as is.
(define profile-script
  (let* (
       (Xresources (plain-file "Xresources" "
Xft.dpi: 192
Xft.hinting: 1
Xft.autohint: 0
Xft.antialias: 1
Xcursor.size: 64
"))
       (xinitrc (mixed-text-file "xinitrc"
         ;; TODO: use the proper method to set these in a config file instead of doing it at initrc
	 "xinput set-prop \"PIXA3854:00 093A:0274 Touchpad\""
         "     \"libinput Natural Scrolling Enabled\""
	 "     1 & "
	 "xinput set-prop \"PIXA3854:00 093A:0274 Touchpad\""
         "     \"libinput Click Method Enabled\""
	 "     0 1 & "
	 "xinput set-prop \"PIXA3854:00 093A:0274 Touchpad\""
         "     \"libinput Disable While Typing Enabled\""
	 "     0 & "
	 python "/bin/python3 " (local-file "battery_script.py") " & "
	 slstatus-patched "/bin/slstatus & "
	 redshift "/bin/redshift  -l 45.421532:-75.697189 -b 1:0.7 -t 6500K:3000K & "
	 ;; TODO: as with xinput, find the correct way to do this
	 "setxkbmap -option caps:none && "
	 xrdb "/bin/xrdb -merge " Xresources " && "
	 "exec " dwm-patched "/bin/dwm"))
       (DIR "~/.guix-home/profile")
       (XORG (string-append DIR "/bin/Xorg"))
       (CONFIGDIR (string-append DIR "/share/X11/xorg.conf.d"))
       (MODULEPATH (string-append DIR "/lib/xorg/modules"))
       )
    (mixed-text-file "profile" ;; name of the file, not part of the script
	  ;; source nix profile to make brave available
"source /run/current-system/profile/etc/profile.d/nix.sh

if [ \"$(tty)\" = \"/dev/tty1\" ]; then
  " xinit "/bin/xinit " xinitrc " -- " XORG " :0 vt1 -dpi 192 -keeptty -configdir " CONFIGDIR " -modulepath " MODULEPATH "
fi")))

(define transform1
  (options->transformation
    '((with-commit
        .
        "libinput-minimal=e8732802b7a3a45194be242a02ead13027c7fd73")
      (with-git-url
        .
        "libinput-minimal=https://gitlab.freedesktop.org/libinput/libinput.git"))))
(define libinput-pack (transform1 (specification->package "xf86-input-libinput")))

(define xorg-packages (list 
		       "dmenu" ;; is like quicklook (from mac) for dwm
		       "xorg-server" ;; the server
		       ;;"xf86-input-libinput" ;; input drivers using patched one instead with my fix for the framework trackpad
		       "xf86-video-fbdev" ;; TODO is this needed?
		       "setxkbmap" ;; TODO: remove this once xinitrc is fixed to use config instead of this to disable caps
		       "xinput" ;; TODO: remove this once config is used to configure mouse instead of doing it in xinitrc
		       ;; "xrdb" "xinit" ;; can get away with both of these uninstalled and referenced directly in the profile
		       ))
(define productivity-packages (list
			       "vim" ;; mostly so guix edit and if I need to make a quick fix in the tty
			       "emacs"
			       "festival" ;; for speech synthesis:
			       "alsa-utils" ;; needed for volume controls used by dwm
			       "xclip" ;; used by dwm command to use festival
			       ;; TODO: write script that does the xclip and festival and then get dwm to reference that instead of installing both?
			       "git"
			       "icedove" ;; TODO: figure out how to make this stop breaking every update
			       ;;"pidgin"
			       ))
(define entertainment-packages (list
				"mpv"
				"dino" ; for communication
				))


(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages (cons* libinput-pack
		  guix-manager-package
		  brctl-package
		  headphones-package
		  worktimer-package
		  playtimer-package
		  mclauncher-package
		  ciarancostume-package
		  sims3-package
		  (specifications->packages
		   (append
		    xorg-packages
		    productivity-packages
		    entertainment-packages))))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list
    (simple-service 'profile home-shell-profile-service-type
		    (list profile-script))
    (simple-service 'lower-brightness home-run-on-first-login-service-type
     `(system "echo 1 > /sys/class/backlight/intel_backlight/brightness"))
    
    (simple-service 'channels home-channels-service-type
     (cons*
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       ;; Enable signature verification:
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix-gaming-games)
       (url "https://gitlab.com/guix-gaming-channels/games.git")
       ;; Enable signature verification:
       (introduction
        (make-channel-introduction
         "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
	 (openpgp-fingerprint
          "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
      %default-channels))
    (simple-service 'environment-variables home-environment-variables-service-type
		    `(("EDITOR" . "vim")
		      ("XCURSOR_SIZE" . "64")))

    (service home-xdg-mime-applications-service-type
    (home-xdg-mime-applications-configuration
     (default
       '((text/html  . brave.desktop)))
     (desktop-entries (list
       (xdg-desktop-entry
        (file "mpv")
        (name "mpv")
        (type 'application)
        (config '((exec . "mpv -- %U"))))
       (xdg-desktop-entry
        (file "brave")
        (name "brave")
        (type 'application)
        (config '((exec . "brave %U"))))
      ))))
       
    (simple-service 'dotfiles home-files-service-type `(
      ;;(".xinitrc" ,xinitrc)
      ;; TODO: the folder this is referring to no longer exists, figure out if brave has figured out fonts now?
      (".config/fontconfig/conf.d/10-guixfonts.conf" ,(plain-file "10-guixfonts.conf" 
"<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
  <dir>/run/current-system/profile/share/fonts/</dir>
</fontconfig>
"))
      (".gitconfig" ,(plain-file "gitconfig"
"[user]
	email = tadhgmister@gmail.com
	name = Tadhg McDonald-Jensen
"))
      (".bashrc" ,(plain-file "bashrc"
"# Adjust the prompt depending on whether we're in 'guix environment'.
# also use \\a to signal bel so when a command finishs it signals x in the same way as chat apps do
if [ -n \"$GUIX_ENVIRONMENT\" ]
then
    PS1='\\a\\w [env]\\$ '
else
    PS1='\\a\\w\\$ '
fi
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
"))
      
                                        )))))
