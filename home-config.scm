;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.

;; TODO: clean up imports
(use-modules (gnu home)
	     ((gnu packages xorg) #:select (xrdb xrandr xinit xsetroot xmodmap))
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
	     ((gnu home services fontutils) #:select (home-fontconfig-service-type))
	     (tadhg dwm)
	     (tadhg packagelist)
	     ((tadhg dunst) #:select(home-dunst-service-type home-dunst-configuration))
	     (tadhg dozfont)
	     ((tadhg channels-and-subs) #:select(channels) #:prefix tadhgs: )
	     (tadhg cyrus-sasl-xoauth)
	     (tadhg emacs)
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
	      (mkdir #$output) ;make the output a directory with a /bin subdirectory
	      (mkdir (string-append #$output "/bin"))
	      (call-with-output-file ; write to a file in /bin with the name of the script
	       (string-append #$output "/bin/" #$name)
	       (lambda (port)
		 ;; paste all the text ungexped into the port
		 (display (string-append #$@text) port)
		 ;; change permissions to give it execute and read permissions
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
	 (DWM_DIFF "diff -up -N $(guix build -e \"(@ (tadhg dwm) dwm-checkout-without-personal)\" -q -L ~/src/dotfiles/packages/) ~/src/dwm > ~/src/dotfiles/packages/tadhg/aux-files/dwm_personal.diff")
	 (HOME "guix home reconfigure ~/src/dotfiles/home-config.scm -L ~/src/dotfiles/packages/")
	 (OS "sudo guix system reconfigure ~/src/dotfiles/os.scm -L ~/src/dotfiles/packages/")
	 (COMMIT "git -C ~/src/dotfiles/ commit")
	 (GITPUSH "git -C ~/src/dotfiles/ push")
	 )
    (single-script-package "guixman"
			   "#!/bin/sh
GUILE_LOAD_PATH=$GUILE_LOAD_PATH:/home/tadhg/src/dotfiles/packages
case $1 in
  \"home\" )
    " DWM_DIFF ";
    " HOME " && " STAGEHOME ";;
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
    ;;cyrus-sasl-xoauth2
    ;; few packages for invoking emacsclient with a few arguments
    (single-script-package "editor"
			   "#!/bin/sh
emacsclient --no-wait --reuse-frame -a vim $@")
    (single-script-package "browser"
			   "#!/bin/sh
emacsclient --no-wait --reuse-frame -a brave $@")
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
" dunst "/bin/dunstify playtimer \"${@:2}\" --timeout=100000
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
	 ;; TODO: there is a xmodmap home service, see if it is viable to replace xmodmap manual invocation
	 (Xmodmap (plain-file "Xmodmap" "
clear control
clear mod3
clear lock

keycode 37 = Control_L
keycode 105 = Hyper_R

add Control = Control_L
add Mod3 = Hyper_R

keycode 66 = BackSpace
"))
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
	 ;; TODO: these settings are directly duplicated in the os.scm keyboard layout
	 ;; figure out some way to co-locate them
	 xmodmap "/bin/xmodmap " Xmodmap " & "
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
	 ;;; TODO: turn this into a home shepherd service
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
;; 	(script ,#~(lambda* (#:key app-name summary body icon urgency id progress
;; 			       category stack-tag urls timeout timestamp desktop-entry #:allow-other-keys)
;; 			      (begin
;; 				(use-modules (ice-9 format))
;; 				(display "TEST TEXT HERE MAYBE?\n")
;; 				(define port (open-output-file (string-append "/home/tadhg/src/dotfiles/logs/" timestamp ".dump")))
;; 				(format port "
;; app-name: ~s
;; summary: ~s
;; body: ~s
;; icon: ~s
;; urgency: ~s
;; id: ~s
;; progress: ~s
;; category ~s
;; stack-tag: ~s
;; urls: ~s
;; timeout: ~s
;; timestamp: ~s
;; desktop-entry: ~s
;; " app-name summary body icon urgency id progress category stack-tag urls timeout timestamp desktop-entry )
;; 				(close-port port)
;; 				)))
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
		 (skip_display  #f)
		 (script ,#~(lambda* (#:key app-name #:allow-other-keys) (system* (string-append #$xdo "/bin/xdo") "activate" "-N" "discord")))
		 )
		))))
    (simple-service 'profile home-shell-profile-service-type
		    (list
		     profile-script))
    ;; run nix-env to reinstall nix packages when the channels change
    ;; TODO: how do I ensure this runs after symlink manager? doesn't that also just use activation service type?
    ;; TODO: pack nix channels, nix package list, sourcing nix profile, and this activation into a single service.
    (simple-service 'nix-env-update home-activation-service-type #~(begin
								     (display "\nSetting up nix-env\n")
								     (system* "nix-env" "-i" "-r")
								     (display "\nFinished nix-env\n")))
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
    ;; 		     ()))
    (service home-xdg-mime-applications-service-type
	     (home-xdg-mime-applications-configuration
	      (default
		'((text/plain . emacsclient.desktop)
		  (x-scheme-handler/org-protocol . emacsclient.desktop)
		  (inode/directory . emacsclient.desktop)
		  ))))
    
    (simple-service 'channels home-channels-service-type tadhgs:channels)
    (simple-service 'environment-variables home-environment-variables-service-type
		    `(;; edit stuff in emacs, mainly used for guix edit
		      ("EDITOR" . "editor")
		      ;; git waits on emacs and opens a new frame so don't need to switch tabs in dwm
		      ("GIT_EDITOR" . "emacsclient --create-frame -a vim")
		      ;; used by xdg-open to open stuff in emacs if nothing else seems viable
		      ;; -a brave is used to launch stuff in brave if emacs server is not running
		      ("BROWSER" . "browser")
		       ;; short circuits a bunch of logic in xdg-open that tries to detect the desktop environment, probably unnecessary
		      ("XDG_CURRENT_DESKTOP" . "X-Generic")
		      ;("SASL_PATH" . ,(file-append cyrus-sasl-xoauth2 "/lib/sasl2/"))
		      ;;("GUIX_BUILD_OPTIONS" . "--max-jobs=6")
		      ;;("GTK_THEME" . "Adwaita-dark")
		      ;;("XCURSOR_SIZE" . "64") ;; TODO: don't think this has any affect, larger cursor would be really nice.
		      ("CC" . "gcc")
		      ("CFLAGS" . "-Wall")
		      ;; Terrible hack to get nix variables to source
		      ;; everywhere internal code says if you bind a
		      ;; key to #t it just writes 'export #$key'
		      ;; without quoting anything so use a newline to
		      ;; abuse this to shove a 'source' command into
		      ;; there.  this puts brave on the PATH for home
		      ;; shepherd services which I rely on for emacs
		      ;; daemon to be able to open urls in brave.
		      ("XDG_DATA_DIRS=${XDG_DATA_DIRS}:$HOME/.nix-profile/share
source /run/current-system/profile/etc/profile.d/nix.sh" . #t)
		      ))
    (service home-openssh-service-type
	     (home-openssh-configuration
	      (hosts (list
		      (openssh-host
		       (name "turris")
		       (host-name "192.168.1.1")
		       (user "root")
		       (host-key-algorithms '("+ssh-rsa"))
		       (extra-content "  StrictHostKeyChecking no"))))))
    (service home-emacs-service-type
	     (home-emacs-configuration
	      (extra-packages (list
			       "geiser"
			       "geiser-guile"
			       "guix"
			       "typescript-mode"))
	      (init.el (org-tangle-file "init.el" (local-file "./emacsconfig.org")))))
    (simple-service 'configfiles home-xdg-configuration-files-service-type `(
      ("mpv/mpv.conf" ,(plain-file "mpv.conf"
"#align video to top of window so if there is extra room subtitles will use black space
video-align-y=-1
save-position-on-quit=yes
"))
      ("mpv/script-opts/osc.conf" ,(plain-file "mpv-osc.conf"
"title=${!playlist-count==1:[${playlist-pos-1}/${playlist-count}] }${media-title} ${chapter}
"))
      ;; TODO: figure out how to get dino to go back to dark theme
      ("gtk-3.0/settings.ini" ,(plain-file "settings.ini"
"[Settings]
gtk-application-prefer-dark-theme = true
"))	   
      ("gtk-4.0/settings.ini" ,(plain-file "settings.ini"
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
      ;; TODO: remove this if I can confirm that I rather alacritty over kitty.
      ("kitty/kitty.conf" ,(plain-file "kitty.conf" "
focus_follows_mouse yes

window_alert_on_bell yes
enable_audio_bell no

# Hyper+C sends Ctrl+C (0x03) and same for Hyper+V
map hyper+c send_text all \\x03
map hyper+v send_text all \\x16
map ctrl+shift+c send_text all \\x03
map ctrl+shift+v send_text all \\x16
# Ctrl+C and Ctrl+V does clipboard copy/paste
map ctrl+c copy_to_clipboard
map ctrl+v paste_from_clipboard
"))
      ;; foot is terminal for wayland, may use this if I ever switch
      ("foot/foot.ini" ,(plain-file "foot.ini" "
[ bell ]
urgent=yes
"))
       ))
    (simple-service 'dotfiles home-files-service-type `(
      ;;(".emacs.d/init.el" ,(local-file "./init.el"))
      (".gitconfig" ,(plain-file "gitconfig"
"[user]
	email = tadhgmister@gmail.com
	name = Tadhg McDonald-Jensen
[filter \"lfs\"]
 	clean = git-lfs clean -- %f
 	smudge = git-lfs smudge -- %f
 	process = git-lfs filter-process
 	required = true
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

;; Local Variables:
;; compile-command: "guix home build home-config.scm -L packages"
;; End:
