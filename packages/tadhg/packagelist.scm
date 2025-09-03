;; this lists packages that should be installed unaltered or with only standard transformations
;; at OS level (os-packages) and in the home config (home-packages)
;;
;; which are put together so that if I ever want to move packages between home and OS it only touches this file. 

(define-module (tadhg packagelist)
  #:use-module (gnu)
  #:use-module ((gnu packages) #:select(specification->package specifications->packages))
  #:use-module ((guix transformations) #:select (options->transformation))
)


(define os-level
  (list
   "openssh"
   "vim" ;; terminal text editor is critical if home config is not available for any reason.
   "glibc-locales"
   ))

(define xorg ;; packages needed for dwm or xorg config
  (list
   "xorg-server" ;; the server, probably important
   "xf86-video-fbdev" ;; TODO is this needed?
   "xf86-video-intel" ;; intel video driver for x11
   "intel-vaapi-driver" ;; intel video acceleration driver
   "intel-media-driver" ;; unclear what this offers more than the vaapi driver
   "xinput" ;; TODO: remove this once config is used to configure mouse instead of doing it in xinitrc
   ;; "xrdb" "xinit" ;; can get away with both of these uninstalled and referenced directly in the profile
   "xdg-utils" ;; provides xdg-open and xdg-mime commands 
   "xf86-input-libinput" ;; drivers, mainly the one for the trackpad with the quirk patch I submitted
   "dmenu" ;; since dmenu has a script dmenu_path which references it's stest utility by name this needs to be installed for the list of autocomplete programs to work properly.
   "font-apple-color-emoji" ;; because I finally got tired of not knowing what emojis people were sending me
   "font-gnu-freefont" "fontconfig" ;; these are needed to make brave show fonts out of the box, may need to run fc-cache manually once
   ))
(define productivity
  (list
   "alacritty" ;; terminal, note that this is hard coded into dwm as the terminal command
   "emacs"
   "emacs-oauth2" ;; for emacs to log into oauth servers
   "emacs-org-caldav" ;; for syncing emacs calendars
   ;;"mu" ;; for reading emails, comes with mu4e to use emacs to view emails
    "festival" ;; for speech synthesis:
    "alsa-utils" ;; needed for volume controls used by dwm
    "xclip" ;; used by dwm command to use festival
    ;; TODO: write script that does the xclip and festival and then get dwm to reference that instead of installing both?
    "git"
    "tup" ;; build system
    ;; TODO remove vscodium once I get LSP working in emacs and am comfortable with it
    "vscodium" ;; community driven clone of vscode, available on nonguix because compiling is hard
    ))
(define communication
  (list
   "icedove" ;; email
   "dino" ;; jabber
   "signal-desktop" ;; signal
   )) 
(define entertainment
  (list
   "mpv" ;; video / audio
   "steam" ;; most of games
   "wine64" ;; sims3
   "dolphin-emu" ;; TODO: maybe remove this?
   ))

(define utility
  (list
   "tree" ;; showing directory structures
   "pavucontrol" ;; audio controls, used by headphone script
   "bluez" ;; bluetooth controls, used by headphone script
   "simplescreenrecorder"
   "qemu" ;; for VM stuff
   "isync" ;; for mbsync mail syncing
    "zip" "unzip" ;; zip and unzip are used enough I'd like to have them always present
   ))
;; (define dino-with-x-alarm-transform
;;   ;; this selects the latest version of dino that still supports the x alarm thingy that gets dwm to do notifications
;;   (options->transformation
;;    `((with-commit
;;       .
;;       "dino=c5cb4a7406c8ed5f18d0580c5edcc3b600ded78d")
;;      (with-git-url
;;       .
;;       "dino=https://github.com/dino/dino.git")
;;      )))
(define-public home-packages
  (cons*
   ;;(dino-with-x-alarm-transform (specification->package "dino"))
   (specifications->packages
    (append xorg productivity communication entertainment utility))))

(define-public os-packages
  (specifications->packages (append os-level)))

