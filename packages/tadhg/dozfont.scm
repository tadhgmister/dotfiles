
(define-module (tadhg dozfont)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((gnu packages python) #:select(python))
  #:use-module ((gnu packages fontutils) #:select(fontforge))
  #:use-module ((guix build-system font) #:select(font-build-system))
  #:use-module (guix gexp)
  #:use-module (guix utils)
)

(define-public dozfont
  (package
    (name "dozfont")
    (version "1")
    (synopsis "dozonal font")
    (description "tadhg's dozonal font")
    (source (local-file "./makefont.py"))
    (build-system font-build-system)
    (native-inputs (list python fontforge))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda* (#:key inputs #:allow-other-keys)
	     (invoke (which "python3") (assoc-ref inputs "source")))))))
    (home-page #f)
    (license license:gpl3)))
