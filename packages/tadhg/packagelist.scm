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
   "xorg-server" ;; the server, probably 
   "alacritty" ;; terminal, not exactly needed just for xorg but ties in with keybindings in dwm so putting it here
   "xf86-video-fbdev" ;; TODO is this needed?
   "setxkbmap" ;; TODO: remove this once xinitrc is fixed to use config instead of this to disable caps
   "xinput" ;; TODO: remove this once config is used to configure mouse instead of doing it in xinitrc
   ;; "xrdb" "xinit" ;; can get away with both of these uninstalled and referenced directly in the profile
   "xdg-utils" ;; not sure exactly what this provides, might just be command line tools or could be necssary for the xdg default applications stuff to work properly.
   "xf86-input-libinput" ;; drivers, mainly the one for the trackpad with the quirk patch I submitted
   "font-apple-color-emoji" ;; because I finally got tired of not knowing what emojis people were sending me
   "dmenu" ;; since dmenu has a script dmenu_path which references it's stest utility by name this needs to be installed for the list of autocomplete programs to work properly.
   ))
(define productivity
  (list
    "emacs"
    "festival" ;; for speech synthesis:
    "alsa-utils" ;; needed for volume controls used by dwm
    "xclip" ;; used by dwm command to use festival
    ;; TODO: write script that does the xclip and festival and then get dwm to reference that instead of installing both?
    "git"
    "tup" ;; build system
    ))
(define communication
  (list
   "icedove" ;; email
   "dino" ;; jabber
   )) 
(define entertainment
  (list
   "mpv" ;; video / audio
   "steam" ;; most of games
   "wine" ;; sims3
   "dolphin-emu" ;; TODO: maybe remove this?
   ))

(define utility
  (list
   "tree" ;; showing directory structures
   "pavucontrol" ;; audio controls, used by headphone script
   "bluez" ;; bluetooth controls, used by headphone script
   "simplescreenrecorder"
   "qemu" ;; for VM stuff
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

