;; emacs dark theme, one specifically designed to have high contrast.
(load-theme 'modus-vivendi t)
; cache the gpg passphrase so it doesn't prompt me every single time for it.
(setq plstore-cache-passphrase-for-symmetric-encryption t)

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


