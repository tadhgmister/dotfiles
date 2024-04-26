;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.

;; TODO: clean up imports
(use-modules (gnu home)
	     ((gnu packages xorg) #:select (xrdb xrandr xinit))
	     ((gnu packages xdisorg) #:select(redshift))
	     ((gnu packages suckless) #:select (slstatus dmenu))
	     ((gnu packages gtk) #:select (pango))
	     ((gnu packages python) #:select (python))
	     ((gnu packages pkg-config) #:select (pkg-config))
	     ((gnu packages xdisorg) #:select(scrot))
	     ((guix gexp) #:select (gexp))
	     ((guix build-system copy) #:select (copy-build-system))
	     ((guix build-system trivial) #:select (trivial-build-system))
	     ((guix transformations) #:select (options->transformation))
	     ((guix packages) #:select (package origin base32 package-version package-source))
	     ((guix download) #:select (url-fetch))
	     ((guix git-download) #:select(git-fetch git-reference git-file-name))
	     ((gnu packages suckless) #:select (dwm slock dmenu))
	     (gnu home services)
	     ((gnu home services guix) #:select (home-channels-service-type))
	     ((guix channels) #:select (channel make-channel-introduction openpgp-fingerprint %default-channels))
	     ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
             (gnu packages)
	     ((guix packages) #:select (modify-inputs package-inputs package-native-inputs prepend))
             (gnu services)
             (guix gexp)
             (gnu home services shells)
	     (gnu home services xdg))


;; TODO: move the minecraft stuff to a separate module.
;; imports just for minecraft, need to move to seperate module
(use-modules
 ((guix build-system cmake) #:select (cmake-build-system))
 ((guix licenses) :prefix license:)
 ((nonguix licenses) :prefix non-license:)
 ((gnu packages java) #:select(icedtea openjdk17))
 ((gnu packages compression) #:select (zlib))
 ((gnu packages pulseaudio) #:select (pulseaudio))
 ((gnu packages qt) #:select(qtbase-5 qtwayland))
 ((gnu packages xorg) #:select (xrandr libxcursor libxrandr libxext libx11 libxxf86vm))
 ((gnu packages gl) #:select (mesa))
 )
;; key extracted from official multimc binary (didn't work, is probably run through a cipher of some form in code before being used)
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
     ;;`(("jdk" ,icedtea "jdk")
     `(("jdk" ,openjdk17 "jdk")
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

;;;;;;; END OF MINECRAFT STUFF

;;;;;;; START OF PACKAGE DEFINITIONS

;; TODO: move single-script-package to separate module
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
	;;"-o -a lordtadhg -l trol -n lordtadhg"
	))

(define guix-man-pull-command (single-script-package "guixmanpullcmd"
"#!/bin/sh
nix-channel --update & 
ICEDOVEPATH=$(realpath $(which icedove))
guix pull
guix home reconfigure ~/src/dotfiles/home-config.scm
if [ \"$ICEDOVEPATH\" != \"$(realpath $(which icedove))\" ]; then
   killall .icedove-real && icedove -ProfileManager > /dev/null 2>/dev/null &
   disown -h %1
fi
guix system build ~/src/dotfiles/os.scm


guix pull --news
wait #wait for nix-channel
echo
echo !!! REMEMBER !!!
echo nix channels were updated, run 'nix-env -r -i' to update nix packages
echo os config was build, run 'guixman os' to reconfigure the system
"))	 
(define guix-manager-package
  (let* ((STAGEHOME "git -C ~/src/dotfiles/ add -u -- :!os.scm")
	 (STAGEOS "git -C ~/src/dotfiles/ add os.scm")
	 ;; for diff: -up gives more contxt, -N will display new files contents
	 ;; for build -q is quiet, means only output is the build directory and not the build log and -f specifies the source file, no substitutes speeds it up since it won't find a viable substitute.
	 (CPDWM "diff -up -N $(guix build -q -f ~/src/dotfiles/dwmsource.scm --no-substitutes) ~/src/dwm > ~/src/dotfiles/dwm_personal.diff")
	 ;; nix-env will load .nix-defexpr/default.nix which lists packages to install with nix (is specified near bottom of this file)
	 ;; -r removes all previously installed packages and -i installs 'the default' packages, meaning the list of packages listed in the default.nix I guess.
	 ;; nix has to be updated after reconfiguring home since updates to the default.nix file in this config will change which packages are updated and that is the whole point of doing it there, it doesn't print anything when nothing needs updating so most of the time it won't matter
	 (HOME "guix home reconfigure ~/src/dotfiles/home-config.scm && nix-env -r -i")
	 (OS "sudo guix system reconfigure ~/src/dotfiles/os.scm")
	 (COMMIT "git -C ~/src/dotfiles/ commit")
	 (GITPUSH "git -C ~/src/dotfiles/ push")
	 (OSBU "guix system build ~/src/dotfiles/os.scm")
	 (PULL "guix pull")
	 (ICEDOVE "icedove -ProfileManager")
	 )
    (single-script-package "guixman"
  "#!/bin/sh\n"
  "case $1 in\n"
  "\n  \"home\" )\n"
  "    " CPDWM "; " HOME " && " STAGEHOME ";;\n"
  "\n  \"os\" )\n"
  "    " OS " && " STAGEOS ";;\n"
  "\n  \"commit\" )\n"
  "    " COMMIT " && " GITPUSH ";;\n"
  "\n  \"pull\" )\n"
  "    " guix-man-pull-command "/bin/guixmanpullcmd;;\n"
  "\n  \"brave\" )\n"
  ;; this is absolutely nothing to do with guix, but it makes more sense to put here than anywhere else, especially since I often want to do it at the same time as guixman pull
  "    sudo herd restart nix-daemon && nix-channel --update && nix-env -iA nixpkgs.brave;;\n"
  "  *)\n"
  "   echo invalid command, see following;\n"
  "   cat $(which $0);;\n"
  "esac\n"
  )))

;; no longer needed but want to keep a reference to it :D
(define transform-libinput-my-trackpad-fix (options->transformation '(
      (with-commit . "libinput-minimal=e8732802b7a3a45194be242a02ead13027c7fd73")
      (with-git-url . "libinput-minimal=https://gitlab.freedesktop.org/libinput/libinput.git"))))
;;(define libinput-pack (transform1 (specification->package "xf86-input-libinput")))
(define transform2
  ;; this selects the latest version of dino that still supports the x alarm thingy that gets dwm to do notifications
  (options->transformation
   `((with-commit
      .
      "dino=c5cb4a7406c8ed5f18d0580c5edcc3b600ded78d")
     (with-git-url
      .
      "dino=https://github.com/dino/dino.git")
     )))
(define dino-with-dwm-notifications (transform2 (specification->package "dino")))

(define xorg-packages (list 
		       ;;"dmenu" ;; is like quicklook (from mac) for dwm
		       "xorg-server" ;; the server
		       ;;"xf86-input-libinput" ;; input drivers using patched one instead with my fix for the framework trackpad
		       "xf86-video-fbdev" ;; TODO is this needed?
		       "setxkbmap" ;; TODO: remove this once xinitrc is fixed to use config instead of this to disable caps
		       "xinput" ;; TODO: remove this once config is used to configure mouse instead of doing it in xinitrc
		       ;; "xrdb" "xinit" ;; can get away with both of these uninstalled and referenced directly in the profile
		       "alacritty" ;; terminal, not exactly needed just for xorg but ties in with keybindings in dwm so putting it here
		       "xdg-utils" ;; not sure exactly what this provides, might just be command line tools or could be necssary for the xdg default applications stuff to work properly.
		       "xf86-input-libinput" ;; drivers, mainly the one for the trackpad with the quirk patch I submitted
		       ))
(define productivity-packages (list
			       ;;"vim" ;; leave vim installed os wide so if things go wrong it is still there to use in tty
			       "emacs"
			       "festival" ;; for speech synthesis:
			       "alsa-utils" ;; needed for volume controls used by dwm
			       "xclip" ;; used by dwm command to use festival
			       ;; TODO: write script that does the xclip and festival and then get dwm to reference that instead of installing both?
			       "git"
			       "tup" ;; build system
			       "icedove" ;; email
			       ))
(define entertainment-packages (list
				"mpv"
				"steam"
				"wine"
				"dolphin-emu"
				;;"dino" ; removed and used transformed version for v0.3 so it has dwm notifications
				))
(define utility-packages (list
			  "tree" ;; showing directory structures
			  "pavucontrol" ;; audio control, used by headphone script
			  "bluez" ;; bluetooth control, used by headphone script
			  "simplescreenrecorder"
			  ))
			  
(define packages-for-home
  (append
   ;; standard packages from above lists
   (specifications->packages xorg-packages)
   (specifications->packages productivity-packages)
   (specifications->packages entertainment-packages)
   (specifications->packages utility-packages)
   (list
   ;; few complicated packages that are defined above
    mclauncher-package
    dino-with-dwm-notifications
    guix-manager-package
   ;; and other scripts that I want to use
   (single-script-package "screenshot"
    "#!/bin/sh\n"
    "mkdir -p ~/Pictures/screenshots/\n"					  
    scrot "/bin/scrot ~/Pictures/screenshots/%Y-%m-%d_%H:%M:%S.%f$1.png")
   
   (single-script-package "webcam"
   "#!/bin/sh\n"
   "mpv --cache=no --demuxer-lavf-format=video4linux2 --demuxer-lavf-o=video_size=1920x1080,input_format=mjpeg av://v4l2:/dev/video0")

   (single-script-package "worktimer"
    "#!/bin/sh\n"
    "echo 1 > /sys/class/leds/input2\\:\\:capslock/brightness\n"
    "sleep $1\n"
    "echo 0 > /sys/class/leds/input2\\:\\:capslock/brightness\n")

   (single-script-package "playtimer"
    "#!/bin/sh\n"
    "worktimer $1\n"
    "if [ $# -gt 1 ]; then\n"
    "    echo \"timer is done,\" ${@:2} | festival --tts\n"
    "else\n"
    "    echo timer is done, stop what you are doing. Look away from the screen. Set another timer | festival --tts\n"
    "fi\n")

   (single-script-package "dmenuwithbangs"
    "#!/bin/sh\n"
    "thing_to_run=`" dmenu "/bin/dmenu_path | cat " (local-file "./list_of_bangs.txt") " - | " dmenu "/bin/dmenu \"$@\"`\n"
    "if [ \"${thing_to_run:0:1}\" = \"!\" ]; then\n"
    "    brave \"? $thing_to_run\" &\n"
    "else\n"
    "    echo \"$thing_to_run\" | ${SHELL:-\"/bin/sh\"} &\n"
    "fi\n")

   (single-script-package "brctl"
    "#!/bin/sh\n"
    "echo $1 > /sys/class/backlight/intel_backlight/brightness")
   
;; TODO: figure out how wine is installed and whether there is any configs given to wine to make this work
   (single-script-package "sims3"
    "#!/bin/sh\n"
    "wine ~/.wine/drive_c/Program\\ Files/Electronic\\ Arts/The\\ Sims\\ 3/Game/Bin/TS3W.exe\n")
   
;; note that this script relies on having pavucontrol and bluez packages installed, both are occasionally needed outside this script
;; so having them installed makes the most sense.
   (single-script-package "headphones"
    "#!/bin/sh\n"
    "if [ $# -eq 0 ]; then\n"
    "  pavucontrol & \n"
    "  bluetoothctl power on\n"
    "  bluetoothctl connect 10:AC:DD:E6:97:45\n"
    "fi\n"
    "if [ $# -eq 1 ]; then\n"
    "  bluetoothctl power off\n"
    "fi\n")

   (single-script-package "ciarancostume"
    "#!/bin/sh\n"
    "cd ~/Documents/\n"
    "  guix shell bluez -- bluetoothctl power on\n"
    "  guix shell bluez -- bluetoothctl connect 75:D2:98:42:EA:A4\n"
    "python3 ciaransoundthing.py &\n"
    "while [ 1 -le 3 ];do python3 -m ble_serial -d E1:CF:11:21:DF:BE; done\n")
   (single-script-package "connecttotv"
    "#!/bin/sh\n"
    ;; set output profile to load the hdmi audio, this can also be done with pavucontrol					  
    "pacmd set-card-profile 0 'output:hdmi-stereo+input:analog-stereo'\n"
    ;; the profile annoyingly removes the builtin speaker as a viable output, so we can reload it
    ;; (if I wanted the built in speaker to stay the default I should probably just figure out what to set the 'device' to to add the hdmi as a new sink)
    "pacmd load-module module-alsa-sink device=hw:0,0\n"
    ;; and then use xrandr to load the display above the built in one.
    ;; TODO: when hdmi is in front left port it is DP-3, check that the '3' is determiend by the port and if I may change it set it correctly in the script.
    xrandr "/bin/xrandr --output DP-3 --above eDP-1\n")

)))

;;; END OF PACKAGES

;;; START OF XORG (dwm, xinit profile script etc.)

(define dwm-patched
  (package
   (inherit dwm)
   (version "6.4")
   ;; add pango dependency
   (inputs (modify-inputs (package-inputs dwm)
             (prepend pango)))
   ;; pango patch adds pkg-config as a dependency to grab necessary compiler flags.
   (native-inputs (modify-inputs (package-native-inputs dwm) (append pkg-config)))
   (source (origin
             (method git-fetch)
             (uri (git-reference (url "https://git.suckless.org/dwm")
                                 (commit version)))
	     (file-name (git-file-name "dwm" version))
             (sha256 (base32 "025x6rbw61c8l3dsdlkb6wawp8236wy0314jlsxi1jyxnfbml4ds"))
	     ;; remove default -p1, all files are in root folder and our personal diff is much easier to create when we use absolute paths referencing the guix store so just use filenames and no subdirectory info.
	     (patch-flags '())
	     (patches (list
		       (origin (method url-fetch)
			(uri "https://dwm.suckless.org/patches/pango/dwm-pango-20230520-e81f17d.diff")
			(sha256 (base32 "0921063c631y770xnfn7dxdb6g3b579r0x3a369amcymf6qb755n")))
		       ;(origin (method url-fetch)
		       ; (uri "https://dwm.suckless.org/patches/hide_vacant_tags/dwm-hide_vacant_tags-6.4.diff")
		       ; (sha256 (base32 "1avzp0mg7f77ifzg6h05f8z6fpx6wly8c018sxn2l7vw8avfj42p")))
		       (origin (method url-fetch)
	                (uri "https://dwm.suckless.org/patches/holdbar/dwm-holdbar-modkey-pertag-nobar-6.2.diff")
	                (sha256 (base32 "0hymhhp2w3rx3006dxwblf7lh4yq3bi958r0qj1x4aszkvdzx1f6")))
		       (origin (method url-fetch)
		        (uri "https://dwm.suckless.org/patches/autostarttags/dwm-autostarttags-6.4.diff")
		        (sha256 (base32 "0rc75hip9kayh62mwhrfp0jjrf1z1l0617mviy5qaqyvxi4g994z")))
		       (origin (method url-fetch)
	                (uri "https://dwm.suckless.org/patches/actualfullscreen/dwm-actualfullscreen-20211013-cb3f58a.diff")
	                (sha256 (base32 "0882k8w6651c18ina0245b558f1bvqydcycw07lp711hpbg7f9gv")))
		       ;; and finally apply the local dwm patch which is updated by the guixman command
		       ;; the command relies on having the dwm source code with git set to the result of the above patches to work correctly
		       ;; and there is not really a good way to store that as I want the diff to be saved as the cononical edits but need a place to make those edits to generate the diff.
		       (local-file "./dwm_personal.diff")
		       ))))))

;; TODO: probably clean up this definition slightly, is kind of unreadable as is.
(define profile-script
  (let* (
	 (LAT_LONG_FOR_REDSHIFT "45.421532:-75.697189")
	 (REDSHIFT_OPTIONS (string-append "-l " LAT_LONG_FOR_REDSHIFT " -b 1:0.9 -t 6500K:3000K"))
       (Xresources (plain-file "Xresources" "
Xft.dpi: 192
Xft.hinting: 1
Xft.autohint: 0
Xft.antialias: 1
Xcursor.theme: default
Xcursor.size: 64
"))
       (xinitrc (mixed-text-file "xinitrc"
	 ;; one off operations to set configurations, done in parallel then waited on before starting other processes
	 xrdb "/bin/xrdb -merge " Xresources " & "
	 "setxkbmap -option caps:none & "
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
	 ;;; wait for above operations before starting main applications
	 "wait; "
	 ;;; then start tasks in parallel
	 python "/bin/python3 " (local-file "battery_script.py") " & "
	 ;;slstatus-patched "/bin/slstatus & "
	 redshift "/bin/redshift " REDSHIFT_OPTIONS " & "
	 ;;; finally, execute dwm to open window manager.
	 "exec " dwm-patched "/bin/dwm"))
       (DIR "~/.guix-home/profile")
       (XORG (string-append DIR "/bin/Xorg"))
       (CONFIGDIR (string-append DIR "/share/X11/xorg.conf.d"))
       (MODULEPATH (string-append DIR "/lib/xorg/modules"))
       )
    (mixed-text-file "profile" ;; name of the file, not part of the script
"
if [ \"$(tty)\" = \"/dev/tty1\" ]; then
  " xinit "/bin/xinit " xinitrc " -- " XORG " :0 vt1 -dpi 192 -keeptty -configdir " CONFIGDIR " -modulepath " MODULEPATH "
fi")))


;;;;;;; END OF XORG

;;;;;;; START OF HOME ENVIRONMENT AND SERVICES


(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages packages-for-home)

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list
    (simple-service 'profile home-shell-profile-service-type
		    (list
		     ;; source nix profile to make brave (and any other applications maybe installed with nix) available
		     (plain-file "nixprofile" "source /run/current-system/profile/etc/profile.d/nix.sh")
		     profile-script)) 
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
		      ;;("XCURSOR_SIZE" . "64") ;; TODO: don't think this has any affect, larger cursor would be really nice.
		      ("CC" . "gcc")
		      ("CFLAGS" . "-Wall")))

    (service home-xdg-mime-applications-service-type
	     (home-xdg-mime-applications-configuration
	      ;; this is the mimetype list supported by icecat
	      ;; MimeType=text/html;text/xml;application/xhtml+xml;application/xml;application/rss+xml;application/rdf+xml;image/gif;image/jpeg;image/png;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/ftp;x-scheme-handler/chrome;video/webm;application/x-xpinstall;
	      (default '(
			 (text/html  . brave.desktop)
			 (text/xml   . brave.desktop)
			 (application/xhtml+xml . brave.desktop)
			 (application/xml . brave.desktop)
			 (image/gif . brave.desktop)
			 (image/png . brave.desktop)
			 (image/jpeg . brave.desktop)
			 (x-scheme-handler/http . brave.desktop)
			 (x-scheme-handler/https . brave.desktop)
			 (x-scheme-handler/chrome . brave.desktop)
			 (application/pdf . brave.desktop)

			 (x-scheme-handler/mailto . icedove.desktop)
			  ))
     (desktop-entries (list
       ;;(xdg-desktop-entry
       ;; (file "mpv")
       ;; (name "mpv")
       ;; (type 'application)
       ;; (config '((exec . "mpv -- %U"))))
       (xdg-desktop-entry
        (file "brave")
        (name "brave")
        (type 'application)
        (config '((exec . "brave %U"))))
      ))))
       
    (simple-service 'dotfiles home-files-service-type `(
      (".gitconfig" ,(plain-file "gitconfig"
"[user]
	email = tadhgmister@gmail.com
	name = Tadhg McDonald-Jensen
"))
      (".nix-defexpr/default.nix" ,(plain-file "default.nix"
"let
   nixpkgs = import ./channels/nixpkgs { config.allowUnfree = true; };
in
[
    nixpkgs.discord
    nixpkgs.brave
]"))
      (".bashrc" ,(plain-file "bashrc"
"# Adjust the prompt depending on whether we're in 'guix environment'.
# also use \\a to signal bel so when a command finishs it signals x in the same way as chat apps do
if [ -n \"$GUIX_ENVIRONMENT\" ]
then
    PS1='\\a\\n\\w [env]\\$ '
else
    PS1='\\a\\n\\w\\$ '
fi
alias ls='ls -p --color=auto'
alias grep='grep --color=auto'
"))
      
                                        )))))

;;;;;;; END OF HOME ENVIRONMENT AND SERVICES
