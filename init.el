;; emacs dark theme, one specifically designed to have high contrast.
(load-theme 'modus-vivendi t)

;; TODO: i don't remember if I ran into a case where this was needed or blindly copied from morgans config,
;;  once I'm using gpg to encrypt files I should test what this specifically does.
; cache the gpg passphrase so it doesn't prompt me every single time for it.
(setq plstore-cache-passphrase-for-symmetric-encryption t)


;; do not create backup (ending in ~) files when it is in a version control system.
;; as well as the normal backup logic to ignore /tmp or other temporary locations
(setq backup-enable-predicate
      (lambda (filename)
        (and (normal-backup-enable-predicate filename)
	     (not (vc-backend filename)))))

;; try to prevent actions from poping up new windows or frames if there are existing ones to use
(setq display-buffer-base-action
      ;; alist of (condition . ACTIONS)
      ;; where ACTIONS is cons cell of (functions . ALIST)
      ;; and ALIST contains keys that are called "action alist entries"
      ;; which because of chaining means each element is '(CONDITION FUNCTIONS (
      
      `(nil ;; function
	 ;; alist entries
	 (inhibit-same-window . nil) ;; override other things that would prevent reusing the same window
	 (reusable-frames . visible) ;; try to use a visible frame, unsure if this counts ones on other x spaces
	 ))


;;;;;  stuff for automatically recovering previous session
;; it is rediculous this amount of code is needed to perform this function non interactively
;; cl-lib is for mock/monkeypatching the prompt function
(require 'cl-lib)

(defun my/try-recover-file (file)
  "Attempt to recover FILE from its autosave non-interactively.
Returns t if recovery succeeds,
Returns nil if recover-file signals an error, I.E. the file doesn't exist,
  the auto-save doesn't exist, or the auto-save is older than the file
Note that if the file is currently open and modified any current changes to the file are unrecoverably lost."
  (condition-case nil
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (recover-file file))
    (:success
     (message "recovered file: %s" file)
     t)
    (error nil)
    (user-error nil)))

(defun my/recover-from-autosave-list (list-file)
  "Recover all real files listed in LIST-FILE.
Skips entries that look like autosave files themselves.  Returns
t if any file was recovered (opened and buffer set to auto-save
content), or returns nil if none of the files had valid autosave
data.

Note that autosave lists always contain pairs of the original
file and the auto-save to be robust against changing the handling
of auto-save files this function *should* try to recover from the
explicitly listed auto-save files instead of determining where
the autosave should be based on the current configuration. This
function *doesn't* do that because the underlying recover-file
function doesn't support that."
  (let ((recovered nil))
    (when (file-readable-p list-file)
      (with-temp-buffer
        (insert-file-contents list-file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((path (string-trim line)))
            (unless (auto-save-file-name-p (file-name-nondirectory path))
              (when (my/try-recover-file path)
                (setq recovered t)))))))
    recovered))

(defun my/process-all-autosave-lists ()
  "Process all autosave list files in the auto-save-list directory.
all files found with auto-save data (that is newer than file) are
opened and recovered. All auto-save lists that do not reference
any file that requires recovery are deleted.

returns t if there was at least one autosave-list that was processed, nil otherwise.

A message is generated for each file recovered and each stale
autosave list deleted so it may be desirable to show the messages
buffer if this returns true to show the user the list of
operations."
  (let* ((prefix auto-save-list-file-prefix)
         (dir (file-name-directory prefix))
         (file-prefix (file-name-nondirectory prefix))
         (pattern (concat "^" (regexp-quote file-prefix)))
         (matches (directory-files dir t pattern)))
    (when matches
      (dolist (file matches)
        (unless (my/recover-from-autosave-list file)
          (delete-file file)
          (message "Deleted stale autosave list: %s" file)))
      t)))

;;;;;;; end of recovery logic, start of startup logic that does call
;;;;;;; the recovery function.

;; when opening org agenda replace any windows in the frame so it is
;; the only thing open useful for opening new frames calling
;; org-agenda to make every new window open in a fresh view of the
;; agenda.
(setq org-agenda-window-setup 'only-window)
;; since we want to see the agenda when we start up and we don't need
;; the warning about autosave recovery no point in running startup
;; screen.
(setq inhibit-startup-screen t)

(defun my/recover-and-agenda ()
  "runs the code to recover all files, opens the org agenda, then if
there was files deleted/opened from the recovery function opens
the messages buffer"
  (let ((did-some-recovery (my/process-all-autosave-lists)))
    (org-agenda nil "n")
    (when did-some-recovery
      (switch-to-buffer "*Messages*"))))

(add-hook 'emacs-startup-hook #'my/recover-and-agenda)

;;;;;;;;; End of startup,
;;;;;;;;; start of keybindings

;; we override right control to send the hyper modifier system wide
;; most programs will not make any use of it but I will get emacs to swap hyper and ctrl
;; so the left control that I typically use will be bound to H- keybindings
;; while normal emacs bindings are still available using right control.
(setq x-hyper-keysym 'ctrl)
(setq x-ctrl-keysym 'hyper)

(defun my/org-agenda-n-in-frame ()
  "Open the Org Agenda in 'n' mode in a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    ;; we rely on the setting that opening the agenda takes the current window for this to work as intended
    (org-agenda nil "n")))



;; keybindings, comment in brackets indicates their original binding

(keymap-global-set "H-g" #'keyboard-quit) ;emacs cancel command (C-g)
;; cua standards
(keymap-global-set "H-c" #'kill-ring-save) ;copy (M-w)
(keymap-global-set "H-v" #'yank)           ;paste (C-y)
(keymap-global-set "H-x" #'kill-region) ;cut (M-w)
(keymap-global-set "H-s" #'save-buffer)    ;save (C-x C-s)
(keymap-global-set "H-a" #'mark-whole-buffer)
(keymap-global-set "H-z" #'undo)    ;undo (C-x u)
(keymap-global-set "H-S-z" #'undo-redo) ;redo (C-M-_ (control alt shift plus))
(keymap-global-set "H-f" #'isearch-forward)    ;search (C-s)
;; opening frames and files
(keymap-global-set "H-n" #'my/org-agenda-n-in-frame) ;new frame (C-x 5 2)
(keymap-global-set "H-o" #'find-file)          ;open file (C-x C-f)
;; programming
;; TODO identify other functions that are available for programming and put them here
(keymap-global-set "H-/" #'comment-or-uncomment-region)

;; non typical keys that I use for my own purposes
(keymap-global-set "H-S-f" 'find-file) ;; occasionally I only have one hand and H-o uses opposite ends of the keyboard

;;;;; end of key bindings, start of calendar stuff

(require 'org-caldav)
(setq org-caldav-url "http://localhost:8080/user/calendars")
(setq org-caldav-calendar-id "calendar")

;;; TODO: set the inbox and files based on agenda files, it may
;;; honestly default to read the first/last value from agenda files if
;;; not set

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


