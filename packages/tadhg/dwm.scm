(define-module (tadhg dwm)
  #:use-module ((guix utils) #:select(current-source-directory))
  #:use-module ((gnu packages suckless) #:select(dwm))
  #:use-module ((gnu packages gtk) #:select(pango))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  
  #:use-module (guix packages)
  #:use-module ((guix gexp) #:select(local-file))
  #:use-module ((guix download) #:select(url-fetch))
  #:use-module ((guix git-download) #:select(git-fetch git-reference git-file-name))
  )

;; patches used from official list of patches
(define official-patches
  (list
   ;; needed to render my dozonal font in the status area, but also apparently can crash if a coloured emoji appears in the window title
   ;; so I have to cover for that in my personal diff.
   (origin (method url-fetch)
	   (uri "https://dwm.suckless.org/patches/pango/dwm-pango-20230520-e81f17d.diff")
	   (sha256 (base32 "0921063c631y770xnfn7dxdb6g3b579r0x3a369amcymf6qb755n")))
  
   ;;; because pango patch changes call signature of a function called right near vacant tags
   ;;; this patch doesn't work correctly, the changes have been merged into personal diff file.
   ;;; TODO: actually I have changed my mind about hiding vacant tags and have effectively reverted this entirely in my personal diff
   ;;;  but commented out code or changes to spacing still linger.
   ;; (origin (method url-fetch)
   ;; 	   (uri "https://dwm.suckless.org/patches/hide_vacant_tags/dwm-hide_vacant_tags-6.4.diff")
   ;; 	   (sha256 (base32 "1avzp0mg7f77ifzg6h05f8z6fpx6wly8c018sxn2l7vw8avfj42p")))
   (origin (method url-fetch)
	   (uri "https://dwm.suckless.org/patches/holdbar/dwm-holdbar-modkey-pertag-nobar-6.2.diff")
	   (sha256 (base32 "0hymhhp2w3rx3006dxwblf7lh4yq3bi958r0qj1x4aszkvdzx1f6")))
   ;; TODO: the autostart logic is awful, if a command opens 0 windows
   ;; it halts the rest of the startups until you manually open any
   ;; window and if a command opens 2 windows it messes up which tag
   ;; the rest of the windows go to
   (origin (method url-fetch)
	   (uri "https://dwm.suckless.org/patches/autostarttags/dwm-autostarttags-6.4.diff")
	   (sha256 (base32 "0rc75hip9kayh62mwhrfp0jjrf1z1l0617mviy5qaqyvxi4g994z")))
   ;; TODO: remove this it didn't solve the issue of getting mpv to
   ;; take up a good amount of space and I now just use my own tiling
   ;; function exclusively
   (origin (method url-fetch)
   	   (uri "https://dwm.suckless.org/patches/bottomstack/dwm-bottomstack-20160719-56a31dc.diff")
   	   (sha256 (base32 "0lq7r887hby7mf8y3ksw28zwi4zzh0wzlwaj9viczir7a4fwlbk4")))
   ;; I am peeved this isn't in the default, apps setting themselves to be fullscreen but dwm not having a keybind to undo it is silly
   (origin (method url-fetch)
	   (uri "https://dwm.suckless.org/patches/actualfullscreen/dwm-actualfullscreen-20211013-cb3f58a.diff")
	   (sha256 (base32 "0882k8w6651c18ina0245b558f1bvqydcycw07lp711hpbg7f9gv")))))


;;; 'list' of personal patches, in theory the vacant tags and config changes could be split into seperate patches so this is kept as a list.
(define personal-patches (list (local-file "aux-files/dwm_personal.diff" )))


(define VERSION "6.4")
(define (get-origin include_personal)
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://git.suckless.org/dwm")
			(commit VERSION)))
    (file-name (git-file-name "dwm" VERSION))
    (sha256 (base32 "025x6rbw61c8l3dsdlkb6wawp8236wy0314jlsxi1jyxnfbml4ds"))
    ;; TODO: figure out a way to change the way the leading paths are generated in the personal diff so we can keep default P1 flag.
    (patch-flags '())
    (patches (append official-patches
		     (if include_personal personal-patches '())))))


(define-public dwm-checkout-without-personal (get-origin #f))

(define-public dwm-tadhg
  (package
    (inherit dwm)
    (name "dwm-tadhg")
    (version VERSION)
   ;; add pango dependency
   (inputs (modify-inputs (package-inputs dwm)
             (prepend pango)))
   ;; pango patch adds pkg-config as a dependency to grab necessary compiler flags.
   (native-inputs (modify-inputs (package-native-inputs dwm) (append pkg-config)))
   (source (get-origin #t))))
   
