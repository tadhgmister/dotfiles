(use-modules (gnu)
  ((gnu packages gtk) #:select(pango))
  (guix packages)
  ((guix gexp) #:select (gexp)))
	     

(define pango-with-copied-pkgconfig
  (package
    (inherit pango)
    (arguments
     '(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:phases (modify-phases %standard-phases
		  ;;; TODO: instead of copying this verbatim from the original, figure out how to get the list of phases frfom pango instead of standard-phases above.
                  (add-after 'unpack 'disable-cantarell-tests
                    (lambda _
                      (substitute* "tests/meson.build"
                        ;; XXX FIXME: These tests require "font-abattis-cantarell", but
                        ;; adding it here would introduce a circular dependency.
                        (("\\[ 'test-layout'.*") "")
                        (("\\[ 'test-itemize'.*") "")
                        (("\\[ 'test-font'.*") "")
                        (("\\[ 'test-harfbuzz'.*") ""))))
		  ;; pkg-config expects these to be under /share/pkgconfig instead of /lib/pkgconfig
                  (add-after 'install 'link-ac-files
                    (lambda* (#:key outputs #:allow-other-keys)
                        (symlink "../lib/pkgconfig" (string-append (assoc-ref outputs "out") "/share/pkgconfig")))))))))

pango-with-copied-pkgconfig
