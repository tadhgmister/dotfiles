(define-module (tadhg channels-and-subs)
  #:use-module(gnu)
  #:use-module((guix gexp) #:select(plain-file))
  #:use-module((guix download) #:select(url-fetch))
  #:use-module((guix packages) #:select(origin base32))
  #:use-module((guix channels) #:select (channel make-channel-introduction openpgp-fingerprint %default-channels))
  )

(define-public nix-channels (plain-file "nix-channels" "https://nixos.org/channels/nixpkgs-unstable nixpkgs\n"))

(define-public channels
  (list
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
      ))

(define-public (substitutes guixconfig)
  (guix-configuration
   (inherit guixconfig)
   ;(substitute-urls (cons*
   ;"https://substitutes.nonguix.org"
   ;%default-substitute-urls))
   (authorized-keys
    (cons*
     (origin (method url-fetch)
	     (uri "https://substitutes.nonguix.org/signing-key.pub")
	     (sha256 (base32 "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
     %default-authorized-guix-keys))))
   
