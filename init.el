;; emacs dark theme, one specifically designed to have high contrast.
(load-theme 'modus-vivendi t)
; cache the gpg passphrase so it doesn't prompt me every single time for it.
(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; we override right control to send the hyper modifier
;; most programs will not make any use of it but I will get emacs to swap hyper and ctrl
;; so the left control that I typically use will be bound to H- keybindings 
(setq x-hyper-keysym 'ctrl)
(setq x-ctrl-keysym 'hyper)

(defun my/org-agenda-n-in-frame ()
  "Open the Org Agenda in 'n' mode in a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (org-agenda nil "n")))
(defun my/org-agenda-in-current-window ()
  "Open Org agenda in the current window, replacing the current buffer."
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window))))
    (org-agenda nil "n")))

(defun my/scold ()
  "flashes the screen"
  (interactive)
  (invert-face 'default)
  (run-at-time 0.1 nil #'invert-face 'default))

;; replace startup screen with org agenda
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook #'my/org-agenda-in-current-window)

;; keybindings, comment in brackets indicates their original binding

					;(keymap-global-set "H-x" 'kill-region)    ;cut (C-w)

(keymap-global-set "H-c" 'kill-ring-save) ;copy (M-w)
(keymap-global-set "H-v" 'yank)           ;paste (C-y)
(keymap-global-set "H-s" 'save-buffer)    ;save (C-x C-s)
(keymap-global-set "H-n" #'my/org-agenda-n-in-frame) ;new frame (C-x 5 2)
(keymap-global-set "H-o" 'find-file)          ;open file (C-x C-f)
(keymap-global-set "H-f" 'isearch-forward)    ;search (C-s)
(keymap-global-set "H-z" 'undo)    ;undo (C-x u)
(keymap-global-set "H-S-z" 'undo-redo) ;redo (C-M-_ (control alt shift plus))

;; non typical keys that I use for my own purposes
(keymap-global-set "H-x" #'my/scold)
(keymap-global-set "H-S-f" 'find-file) ;; occasionally I only have one hand and H-o uses opposite ends of the keyboard


;; prefer poping out a "frame" (application window) instead of a
;; "window" (split buffer view in same app window) TODO read the docs
;; about these variables, the decision process for opening new
;; window/frames is complicated and worth taking some time to
;; understand
;(setq pop-up-windows nil)
;(setq pop-up-frames t)


(require 'org-caldav)
(setq org-caldav-url "http://localhost:8080/user/calendars")
(setq org-caldav-calendar-id "calendar")
;; Org filename where new entries from calendar stored
(setq org-caldav-inbox "~/Sync/cal.org")

;; Additional Org files to check for calendar events
(setq org-caldav-files '("~/Sync/work.org"))
;; and set the org agenda to also have those files
(setq org-agenda-files '("~/Sync/work.org"
                         "~/Sync/cal.org"))

;; Usually a good idea to set the timezone manually
(setq org-icalendar-timezone "America/Toronto")

;; sync todos as well.
(setq org-icalendar-include-todo 'all
      org-caldav-sync-todo t)
;; creates SCHEDULED timestamp from DEADLINE
;; (setq org-caldav-todo-deadline-schedule-warning-days t)

;; guix uses .dir-local.el with some functions that emacs doesn't recognize as safe,
;; the parts that are of questionable safety are copied here to mark them as safe
;; note that the indentation within this list can vary since it is copied from the source code
;; and not all the parts that need to be labeled safe are called from the top level so their indentation varies.
(setopt
 safe-local-variable-values
 '(
 ;;;;;;; SAFE VARIABLES FROM GUIX
     (eval . (add-to-list 'completion-ignored-extensions ".go"))
     ;; Emacs-Guix
     (eval . (setq-local guix-directory
                         (locate-dominating-file default-directory
                                                 ".dir-locals.el")))
     ;; YASnippet
     (eval . (with-eval-after-load
                 'yasnippet
               (let ((guix-yasnippets
                      (expand-file-name
                       "etc/snippets/yas"
                       (locate-dominating-file default-directory
                                               ".dir-locals.el"))))
                 (unless (member guix-yasnippets yas-snippet-dirs)
                   (add-to-list 'yas-snippet-dirs guix-yasnippets)
                   (yas-reload-all)))))

     ;; Geiser
     ;; This allows automatically setting the `geiser-guile-load-path'
     ;; variable when using various Guix checkouts (e.g., via git worktrees).
     (geiser-repl-per-project-p . t)
	  
   ;; This notably allows '(' in Paredit to not insert a space when the
   ;; preceding symbol is one of these.
   (eval . (modify-syntax-entry ?~ "'"))
   (eval . (modify-syntax-entry ?$ "'"))
   (eval . (modify-syntax-entry ?+ "'"))

   ;; Emacs 28 changed the behavior of 'lisp-fill-paragraph', which causes the
   ;; first line of package descriptions to extrude past 'fill-column', and
   ;; somehow that is deemed more correct upstream (see:
   ;; https://issues.guix.gnu.org/56197).
   (eval . (progn
             (require 'lisp-mode)
             (defun emacs27-lisp-fill-paragraph (&optional justify)
               (interactive "P")
               (or (fill-comment-paragraph justify)
                   (let ((paragraph-start
                          (concat paragraph-start
                                  "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                         (paragraph-separate
                          (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                         (fill-column (if (and (integerp emacs-lisp-docstring-fill-column)
                                               (derived-mode-p 'emacs-lisp-mode))
                                          emacs-lisp-docstring-fill-column
                                        fill-column)))
                     (fill-paragraph justify))
                   ;; Never return nil.
                   t))
             (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph)))
   ;;;;;; END SAFE VARIABLES FROM GUIX
         ))


