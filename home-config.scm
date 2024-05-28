;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.

;; TODO: clean up imports
(use-modules (gnu home)
	     ((gnu packages xorg) #:select (xrdb xrandr xinit xsetroot))
	     ((gnu packages xdisorg) #:select(redshift xdo))
	     ((gnu packages suckless) #:select (slstatus dmenu))
	     ((gnu packages gtk) #:select (pango))
	     ((gnu packages python) #:select (python))
	     ((gnu packages pkg-config) #:select (pkg-config))
	     ((gnu packages xdisorg) #:select(scrot))
	     ((gnu packages wm) #:select(dunst))
	     ((guix build-system copy) #:select (copy-build-system))
	     ((guix build-system trivial) #:select (trivial-build-system))
	     ((guix transformations) #:select (options->transformation))
	     ((guix packages) #:select (package origin base32 package-version package-source))
	     ((guix download) #:select (url-fetch))
	     ((guix git-download) #:select(git-fetch git-reference git-file-name))
	     ((gnu packages suckless) #:select (dwm slock dmenu))
	     (gnu home services)
	     ((gnu home services guix) #:select (home-channels-service-type))
	     ((guix channels) #:select(%default-channels))
	     
	     ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
             (gnu packages)
	     ((guix packages) #:select (modify-inputs package-inputs package-native-inputs prepend))
             (gnu services)
             (guix gexp)
             (gnu home services shells)
	     (gnu home services xdg)
	     (gnu home services ssh)
	     (gnu home services desktop)
	     (tadhg dwm)
	     (tadhg packagelist)
	     ((tadhg dunst) #:select(home-dunst-service-type home-dunst-configuration))
	     (tadhg dozfont)
	     ((tadhg channels-and-subs) #:select(channels) #:prefix tadhgs: )
)


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
;; (define mclauncher-package (single-script-package "multimc"
;; 	"#!/bin/sh\n"
;; 	multimc "/bin/DevLauncher -d ~/Documents/minecraft "
;; 	;;"-o -a lordtadhg -l trol -n lordtadhg"
;; 	))

(define guix-man-pull-command (single-script-package "guixmanpullcmd"
"#!/bin/sh
nix-channel --update & 
ICEDOVEPATH=$(realpath $(which icedove))
guix pull
guix home reconfigure  -L ~/src/dotfiles/packages/ ~/src/dotfiles/home-config.scm
if [ \"$ICEDOVEPATH\" != \"$(realpath $(which icedove))\" ]; then
   killall .icedove-real && icedove -ProfileManager > /dev/null 2>/dev/null &
   disown -h %1
fi
guix system build -L ~/src/dotfiles/packages/ ~/src/dotfiles/os.scm


guix pull --news
wait #wait for nix-channel
echo
echo !!! REMEMBER !!!
echo nix channels were updated, run 'guixman home' to update nix packages (or 'nix-env -r -i')
echo os config was build, run 'guixman os' to reconfigure the system
"))	 
(define guix-manager-package
  (let* ((STAGEHOME "git -C ~/src/dotfiles/ add -u -- :!os.scm")
	 (STAGEOS "git -C ~/src/dotfiles/ add os.scm")
	 ;; nix-env will load .nix-defexpr/default.nix which lists packages to install with nix (is specified near bottom of this file)
	 ;; -r removes all previously installed packages and -i installs 'the default' packages, meaning the list of packages listed in the default.nix I guess.
	 ;; nix has to be updated after reconfiguring home since updates to the default.nix file in this config will change which packages are updated and that is the whole point of doing it there, it doesn't print anything when nothing needs updating so most of the time it won't matter
	 (NIX "nix-env -r -i")
	 ;; for diff command -up gives more context, -N displays new files contents.
	 ;; we specifically use guix build to grab the most up to date version of the package definitions
	 (DWM_DIFF "diff -up -N $(guix build -e \"(@ (tadhg dwm) dwm-checkout-without-personal)\" -q) ~/src/dwm > ~/src/dotfiles/dwm_personal.diff")
	 (HOME "guix home reconfigure ~/src/dotfiles/home-config.scm")
	 (OS "sudo guix system reconfigure ~/src/dotfiles/os.scm")
	 (COMMIT "git -C ~/src/dotfiles/ commit")
	 (GITPUSH "git -C ~/src/dotfiles/ push")
	 )
    (single-script-package "guixman"
			   "#!/bin/sh
GUILE_LOAD_PATH=$GUILE_LOAD_PATH:/home/tadhg/src/dotfiles/packages
case $1 in
  \"home\" )
    " DWM_DIFF ";
    " HOME " && " STAGEHOME " && " NIX ";;
  \"os\" )
    " OS " && " STAGEOS ";;
  \"commit\" )
    " COMMIT " && " GITPUSH ";;
  \"pull\" )
    " guix-man-pull-command "/bin/guixmanpullcmd;;
  *)
   echo invalid command, see following;
   cat $(which $0);;
esac
")))

(define packages-for-home
  (append
   ;; standard packages from module
   home-packages
   (list
   ;; few complicated packages that are defined above
    ;;mclauncher-package
    guix-manager-package
    dozfont
   ;; and other scripts that I want to use
   (single-script-package "screenshot"
    "#!/bin/sh
mkdir -p ~/Pictures/screenshots/
"  scrot "/bin/scrot ~/Pictures/screenshots/%Y-%m-%d_%H:%M:%S.%f$1.png
")
   (single-script-package "webcam"
   "#!/bin/sh
mpv --cache=no --demuxer-lavf-format=video4linux2 --demuxer-lavf-o=video_size=1920x1080,input_format=mjpeg av://v4l2:/dev/video0
")
   (single-script-package "worktimer"
    "#!/bin/sh
echo 1 > /sys/class/leds/input2\\:\\:capslock/brightness
sleep $1
echo 0 > /sys/class/leds/input2\\:\\:capslock/brightness
")
   (single-script-package "playtimer"
    "#!/bin/sh
worktimer $1
if [ $# -gt 1 ]; then
    echo \"timer is done,\" ${@:2} | festival --tts
else
    echo timer is done, stop what you are doing. Look away from the screen. Set another timer | festival --tts
fi
")
   (single-script-package "dmenuwithbangs"
			  ;; NOTE: dmenu needs to be installed for this to work properly since dmenu_path references stest directly.
    "#!/bin/sh
thing_to_run=`dmenu_path | cat " (local-file "./list_of_bangs.txt") " - | dmenu \"$@\"`
if [ \"${thing_to_run:0:1}\" = \"!\" ]; then
    brave \"? $thing_to_run\" &
else
    echo \"$thing_to_run\" | ${SHELL:-\"/bin/sh\"} &
fi
")
   (single-script-package "brctl" ;;TODO: don't make this hardcoded path to backlight?
    "#!/bin/sh
if [ $1 -ge 2 ]; then
    number=$(( ($1 - 1) * 1000))
else
    number=$1
fi
echo $number > /sys/class/backlight/intel_backlight/brightness
")
;; TODO: figure out how wine is installed and whether there is any configs given to wine to make this work
   (single-script-package "sims3"
    "#!/bin/sh\n"
    "wine ~/.wine/drive_c/Program\\ Files/Electronic\\ Arts/The\\ Sims\\ 3/Game/Bin/TS3W.exe\n")
   
;; note that this script relies on having pavucontrol and bluez packages installed, both are occasionally needed outside this script
;; so having them installed makes the most sense.
   (single-script-package "headphones"
			  "#!/bin/sh
if [ $# -eq 0 ]; then
  pavucontrol &
  bluetoothctl power on
  bluetoothctl connect 10:AC:DD:E6:97:45
fi
if [ $# -eq 1 ]; then
  bluetoothctl power off
fi
")
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


;; TODO: probably clean up this definition slightly, is kind of unreadable as is.
(define profile-script
  (let* (
       (Xresources (plain-file "Xresources" "
Xft.dpi: 192
Xft.hinting: 1
Xft.autohint: 0
Xft.antialias: 1
Xcursor.theme: redglass
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
	 ;;; finally, execute dwm to open window manager.
	 "exec " dwm-tadhg "/bin/dwm"))
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
    (service home-dbus-service-type)
    (service home-dunst-service-type
	     (home-dunst-configuration
	      (log-file "/home/tadhg/.local/state/log/dunst.log")
       (global `(
	;;(dmenu . "/gnu/store/ppbhrgv67g3rzkdxdksw5sqgn8p55cxs-dmenu-5.2/bin/dmenu -p dunst")
	(dmenu ,#~(string-append #$dmenu "/bin/dmenu -p dunst"))
	(script ,#~(lambda* (#:key app-name summary body icon urgency id progress
			       category stack-tag urls timeout timestamp desktop-entry #:allow-other-keys)
			      (begin
				(use-modules (ice-9 format))
				(display "TEST TEXT HERE MAYBE?\n")
				(define port (open-output-file (string-append "/home/tadhg/src/dotfiles/logs/" timestamp ".dump")))
				(format port "
app-name: ~s
summary: ~s
body: ~s
icon: ~s
urgency: ~s
id: ~s
progress: ~s
category ~s
stack-tag: ~s
urls: ~s
timeout: ~s
timestamp: ~s
desktop-entry: ~s
" app-name summary body icon urgency id progress category stack-tag urls timeout timestamp desktop-entry )
				(close-port port)
				)))
	;;(mouse_left_click . "context, close_current")
	;;(mouse_right_click . "close_current")
	;;(timeout . "10m")
	))
       (rules `(
		(dino-trigger-immidiate
		 (appname "Dino")
		 (skip_display #t)
		 (script ,#~(lambda* (#:key app-name #:allow-other-keys) (system* (string-append #$xdo "/bin/xdo") "activate" "-N" "dino")))
		 )
		(discord-trigger-immidiate
		 (appname "discord")
		 (skip_display  #t)
		 (script ,#~(lambda* (#:key app-name #:allow-other-keys) (system* (string-append #$xdo "/bin/xdo") "activate" "-N" "discord")))
		 )
		))))
    (simple-service 'profile home-shell-profile-service-type
		    (list
		     ;; source nix profile to make brave (and any other applications maybe installed with nix) available
		     (plain-file "nixprofile" "source /run/current-system/profile/etc/profile.d/nix.sh")
		     profile-script)) 
    (simple-service 'lower-brightness home-run-on-first-login-service-type
		    `(system "echo 1 > /sys/class/backlight/intel_backlight/brightness"))
    (service home-redshift-service-type
         (home-redshift-configuration
          (location-provider 'manual)
          (latitude 45.421532)
          (longitude -75.697189)
	  (nighttime-temperature 2500)
	  (nighttime-brightness 0.9)))
    ;; (service  home-batsignal-service-type
    ;; 		    (home-batsignal-configuration
    ;; 		     (
    
    (simple-service 'channels home-channels-service-type tadhgs:channels)
    (simple-service 'environment-variables home-environment-variables-service-type
		    `(("EDITOR" . "vim")
		      ;;("GTK_THEME" . "Adwaita-dark")
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
    (service home-openssh-service-type
	     (home-openssh-configuration
	      (hosts (list
		      (openssh-host
		       (name "turris")
		       (host-name "192.168.1.1")
		       (user "root")
		       (host-key-algorithms '("+ssh-rsa"))
		       (extra-content "  StrictHostKeyChecking no"))))))
    (simple-service 'configfiles home-xdg-configuration-files-service-type `(	   
      ("gtk-3.0/settings.ini" ,(plain-file "settings.ini"
"[Settings]
gtk-application-prefer-dark-theme = true
"))
      ("gtk-4.0/gtk.css" ,(plain-file "settings.ini"
"window.dino-main {
        font-size: 30px;
}

window.dino-main .dino-conversation {
        font-size: 40px;
}
"))
      ("foot/foot.ini" ,(plain-file "foot.ini" "
[ bell ]
urgent=yes
"))
       ))
    (simple-service 'dotfiles home-files-service-type `(
      (".emacs.d/init.el" ,(local-file "./init.el"))
      (".gitconfig" ,(plain-file "gitconfig"
"[user]
	email = tadhgmister@gmail.com
	name = Tadhg McDonald-Jensen
[sendemail]
	smtpserver = smtp.gmail.com
	smtpuser = tadhgmister@gmail.com
	smtpencryption = ssl
	smtpserverport = 465
"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (".nix-channels" ,(plain-file "nix-channels" "https://nixos.org/channels/nixpkgs-unstable nixpkgs\n"))
      (".nix-defexpr/default.nix" ,(plain-file "default.nix"
"let
   nixpkgs = import ./channels/nixpkgs { config.allowUnfree = true; };
in
[
    nixpkgs.discord
    nixpkgs.brave
    nixpkgs.vscode
]"))
      ;;;;;;;;;;;;;;;;;;;;;;;;;
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
