(define-module (tadhg webgallery)
  #:use-module (guix gexp)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video))

(define (jpg-path? path)
  (or (string-suffix? ".jpg" path)
      (string-suffix? ".jpeg" path)))
(define (gif-path? path)
  (string-suffix? ".gif" path))
(define (video-path? path)
  (string-suffix? ".mp4" path))
(define (supported-path? path)
  (or (jpg-path? path)
      (video-path? path)))
(define (video-thumbnail name orig-file)
  (computed-file (string-append name "-thumbnail.gif")
		 #~(system* #+(file-append ffmpeg "/bin/ffmpeg")
			    ;; most of this came from here: https://superuser.com/a/556031/550312
			    "-v" "error"
			    "-i" #+orig-file "-ss" "1" "-t" "5" 
			    "-vf" "fps=10,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" 
			    "-loop" "0"  #$output)
		 #:options '(#:substitutable? #f)))

(define (thumbnail name orig-file)
  (computed-file (string-append name "-thumbnail")
		 #~(system* #+(file-append imagemagick "/bin/convert")
			    #+orig-file
			    "-quality" "40%"
			    "-resize" "320x320"
			    #$output)
		 #:options '(#:substitutable? #f)))
(define (thumbnail-entry pair)
  (let ((name (car pair))
	(file (cadr pair)))
    (cond
     ((gif-path? name) 
      pair) ;; leave gifs as is, usually they are small enough to be ok
     ((video-path? name)
      (list (string-append name ".gif") (video-thumbnail name file)))
     (else (list name (thumbnail name file))))))
(define (vid-snippet filename file)
  (computed-file (string-append filename ".htmlsnippet")
   (with-imported-modules '((sxml simple)
			    (ice-9 popen)
			    (ice-9 textual-ports)
			    )
     #~(begin
         (use-modules (sxml simple)
		      (ice-9 popen)
                      (ice-9 textual-ports))
	 (define (read-output . args)
	   (let* ((p (apply open-pipe* OPEN_READ args))
		  (data (get-string-all p)))
	     (when (not (zero? (status:exit-val (close-pipe p))))
	       (throw 'error args))
	     data))
	 (define filename #+filename)
	 ;; (define caption (read-output #+(file-append libiptcdata "/bin/iptc")
	 ;; 			      "-p" "Caption" filename))
	 ;; (define headline (read-output #+(file-append libiptcdata "/bin/iptc")
	 ;; 			      "-p" "Headline" filename))
	 ;; (define keywords (string-split (read-output #+(file-append libiptcdata "/bin/iptc")
	 ;; 					     "-p" "Keywords:all" filename) #\newline))
	 (define img-fields (append
			     `((loading "lazy"))
			 ;; (if (string-null? caption) '() `((alt ,caption)))
			 ;; (if (string-null? headline) '() `((title ,headline)))
			 ))
	 (define a-fields (append
			   `((target "viewer") (id ,filename)  (href ,filename))
			   ;; (if (null? keywords) '() `((class ,(string-join keywords " "))))
			   ))
	 (define dims (string-split (read-output #+(file-append ffmpeg "/bin/ffprobe")
							      "-v" "error"
							      "-select_streams" "v:0"
							      "-show_entries" "stream=width,height"
							      "-of" "csv=p=0" #+file)
				    #\,))
	 (define thumb-src (string-append "thumbnails/" filename ".gif"))
	 (define width (car dims))
	 (define height (cadr dims))
	 (define style (string-append "--w:" width ";--h:" height ";"))
	 (define entry `(a (@ ,@a-fields (style ,style))
			   (img (@ ,@img-fields (src ,thumb-src)
				   (width ,width) (height ,height)))))
	 (call-with-output-file #$output
	   (lambda (port)
	     (sxml->xml entry port)))))
   #:options '(#:substitutable? #f)))
(define (img-snippet filename file)
  (computed-file (string-append filename ".htmlsnippet")
   (with-imported-modules '((sxml simple)
		            (ice-9 popen)
                            (ice-9 textual-ports))
     #~(begin
         (use-modules (sxml simple)
		      (ice-9 popen)
                      (ice-9 textual-ports))
	 (define (read-output . args)
	   (let* ((p (apply open-pipe* OPEN_READ args))
		  (data (get-string-all p)))
	     (when (not (zero? (status:exit-val (close-pipe p))))
	       (throw 'error args))
	     data))
	 (define (read-output-maybe default . args)
	   (let* ((p (apply open-pipe* OPEN_READ args))
		  (data (get-string-all p)))
	     (if (zero? (status:exit-val (close-pipe p)))
		 data
		 default)))
	 (define filename #+filename)
	 (define caption (read-output-maybe "" #+(file-append libiptcdata "/bin/iptc")
				      "-p" "Caption" #+file))
	 (define headline (read-output-maybe "" #+(file-append libiptcdata "/bin/iptc")
				      "-p" "Headline" #+file))
	 (define keywords (read-output-maybe "" #+(file-append libiptcdata "/bin/iptc")
						     "-p" "Keywords:all" #+file))
	 (define img-fields (append
			     `((loading "lazy"))
			 (if (string-null? caption) '() `((alt ,caption)))
			 (if (string-null? headline) '() `((title ,headline)))))
	 (define a-fields (append
			   `((target "viewer") (id ,filename)  (href ,filename))
			   (if (string-null? keywords) '() `((class ,(string-join (string-split keywords #\newline) " "))))))
	 (define dims (string-split (read-output #+(file-append imagemagick "/bin/identify")
						         "-auto-orient" "-format" "%w,%h"
							 #+file)
				    #\,))
	 (define thumb-src (string-append "thumbnails/" filename #+@(if (video-path? filename) '(".gif") '())))
	 (define width (car dims))
	 (define height (cadr dims))
	 (define style (string-append "--w:" width ";--h:" height ";"))
	 (define entry `(a (@ ,@a-fields (style ,style))
			   (img (@ ,@img-fields (src ,thumb-src)
				   (width ,width) (height ,height)))))
	 (call-with-output-file #$output
	   (lambda (port)
	     (sxml->xml entry port)))))
   #:options '(#:substitutable? #f)))

(define (snippet filename orig-file)
  (if (video-path? filename)
      (vid-snippet filename orig-file)
      (img-snippet filename orig-file)))
(define gallery-prefix "<!DOCTYPE html>
<html>
<head>
 <meta charset=\"utf-8\">
 <title>Image Gallery</title>
 <link rel=\"stylesheet\" href=\"./styles.css\">
 <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
 <meta name=\"color-scheme\" content=\"light dark\">
</head>
<body class=\"gallery\">
")
(define gallery-suffix "</body></html>")

(define* (gallery-file img-snippets #:optional (name-prefix ""))
  (computed-file (string-append name-prefix "index.html")
   (with-imported-modules '((ice-9 textual-ports))
     #~(call-with-output-file #$output
	   (lambda (port)
	     (define (write-snippet img-file)
	       (display (call-with-input-file img-file (@ (ice-9 textual-ports) get-string-all)) port)
	       (newline port))
	     (display #+gallery-prefix port)
	     (for-each write-snippet  (list #+@img-snippets))
	     (display #+gallery-suffix port)
	     )))
   #:options '(#:substitutable? #f)))




(define-public (gallery-folder folder)
  (let* ((imgs (map (lambda (x) (list x (local-file (string-append folder x))))
		    (scandir folder supported-path?)))
	 (img-snips (map (lambda (pair) (snippet (car pair) (cadr pair))) imgs)))
    (file-union "gallery" (cons* (list "index.html" (gallery-file img-snips))
				 (list "thumbnails" (file-union "thumbnails" (map thumbnail-entry imgs)))
				 (list "styles.css" (local-file "./aux-files/gallery.css"))
				 imgs))))

;; (gallery-folder "/home/tadhg/Pictures/huawaiBackup/")
;;(gallery-folder "/home/tadhg/Pictures/")
