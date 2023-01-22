;; This is a Guix deployment of a "bare bones" setup, with
;; no X11 display server, to a machine with an SSH daemon
;; listening on localhost:2222. A configuration such as this
;; may be appropriate for virtual machine with ports
;; forwarded to the host's loopback interface.

(use-service-modules networking ssh)
(use-modules
 ((gnu packages bootloader) #:select(make-u-boot-package))
 ((gnu services networking) #:select(dhcp-client-service-type))
 ((gnu services ssh) #:select(openssh-service-type))
)

(define u-boot-amlogic-s905w
  (make-u-boot-package "amlogic s905w" "arm-linux-")
  )
(define TURRIS_TRIPLET "arm-linux-muslgnueabi")
(define DEVICENAME "/dev/vda")
(define DEVICEPARTITION "/dev/vda1")
(define %system
  (operating-system
   (host-name "x96testguix")
   (timezone "America/Toronto")
   (bootloader (bootloader-configuration
                (bootloader u-boot-bootloader)
                (targets (list DEVICENAME))
                (terminal-outputs '(console))))
   (file-systems (cons (file-system
                        (mount-point "/")
                        (device DEVICEPARTITION)
                        (type "ext4"))
                       %base-file-systems))
   (services
    (append (list (service dhcp-client-service-type)
                  (service openssh-service-type
                           (openssh-configuration
                            (permit-root-login #t)
                            (allow-empty-passwords? #t))))
            %desktop-services))))

(%system)
;; (list (machine
;;        (operating-system %system)
;;        (environment managed-host-environment-type)
;;        (configuration (machine-ssh-configuration
;;                        (host-name "localhost")
;;                        (system "x86_64-linux")
;;                        (user "alice")
;;                        (identity "./id_rsa")
;;                        (port 2222)))))
