;; Starts in maximum frame

;; Helm buffer
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; M-x smex by default
(global-set-key (kbd "M-x") 'helm-M-x)

;; This is old M-x 
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Starts eshell
(global-set-key (kbd "C-x m") 'eshell)


;; Company mode tooltip bar
(company-quickhelp-mode 1)


;; No bell sound
(setq ring-bell-function 'ignore)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; Comments with C-;
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
;; (setq-default sh-basic-offset 2)
;; (setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;
(winner-mode 1)

;; Move windows using Shift + arrow keys
(windmove-default-keybindings)

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")
