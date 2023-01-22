;; This is a Guix deployment of a "bare bones" setup, with
;; no X11 display server, to a machine with an SSH daemon
;; listening on localhost:2222. A configuration such as this
;; may be appropriate for virtual machine with ports
;; forwarded to the host's loopback interface.

(use-modules
 (gnu)
 ((gnu system install) #:select(os-with-u-boot installation-os)) 
 ;;((gnu packages bootloaders) #:select(make-u-boot-package))
 ;;((gnu bootloader u-boot) #:select(u-boot-bootloader))
 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type openssh-configuration))
 ((gnu services desktop) #:select (%desktop-services))
)


;; (define X96_TRIPLET "arm-linux-muslgnueabi")
(define DEVICENAME "/dev/vda")
(define DEVICEPARTITION "/dev/sda2")
;; (define bootloader-package
;;   (make-u-boot-package "odroid-c2" "aarch-linux-gnu"))

(define %system
  (operating-system
   (host-name "x96testguix")
   (timezone "America/Toronto")
   (file-systems (cons (file-system
                        (mount-point "/")
                        (device DEVICEPARTITION)
                        (type "ext4"))
                       %base-file-systems))
   (bootloader #f)
   (services
    (append (list (service dhcp-client-service-type)
                  (service openssh-service-type
                           (openssh-configuration
                            (permit-root-login #t)
                            (allow-empty-passwords? #t))))
            %base-services))))
(os-with-u-boot %system "p212" #:bootloader-target "/dev/sda" #:triplet "aarch64-linux-gnu")


;; (%system)
;; ;;(bootloader-package)
;; ;; (list (machine
;; ;;        (operating-system %system)
;; ;;        (environment managed-host-environment-type)
;; ;;        (configuration (machine-ssh-configuration
;; ;;                        (host-name "localhost")
;; ;;                        (system "x86_64-linux")
;; ;;                        (user "alice")
;; ;;                        (identity "./id_rsa")
;; ;;                        (port 2222)))))
