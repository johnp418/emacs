(require 'package)

;; Adds emacs repositories
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(defvar my-packages
  '(
    ;; Helm
    ;; helm

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;;
    ido-ubiquitous

    ;; Project navigation
    projectile

    ;; Autocomplete functionality
    company
    company-quickhelp

    ;;Clojure
    clojure-mode
    cider
    rainbow-delimiters
    paredit
    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; elpy

    
    ;; Better search tool
    swiper
    which-key

    ;; A collection of paredit-like functions for editing in html-mode.
    ;; https://github.com/magnars/tagedit

    ;; Zenburn theme
    zenburn-theme

    ;; Multiple Cursors
    multiple-cursors

    ;;
    switch-window
    ace-jump-mode

    ;; Visualization
    undo-tree
    minimap

    ;; Git integration
    magit))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Sets up exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Starts company-mode
;; (add-hook 'after-init-hook 'global-company-mode)

;; Set company completion delay
;; (setq company-idle-delay 0)
;; (setq company-dabbrev-downcase nil)


;;;;;;;
;; Starts ido-mode
(ido-mode t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
;;;;;;;


;; (desktop-save-mode 1)
;; (setq initial-buffer-choice nil)
;; Undo tree mode
;; (global-undo-tree-mode)
;; (global-set-key (kbd "M-/") 'undo-tree-visualize)

;; Better C-x o
(global-set-key (kbd "C-x o") 'switch-window)

;; Moving caret with C->
(global-set-key (kbd "C->") 'ace-jump-mode)

;; Better search
;; (ivy-mode 1)
;; (global-set-key (kbd "C-s") 'swiper)
;; (global-rainbow-delimiters-mode t)
;; (smartparens-global-mode 1)

;; Tree dir
;; (neotree)

;; Go hook
;; (add-hook 'go-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'before-save-hook 'gofmt-before-save)
;; 	    (setq tab-width 4)
;; 	    (setq indent-tabs-mode 1)))


;; (setq tab-width 2)
;; multiple cursors mode


;;(setq highlight-indent-guides-method 'character)

;; Enabling highlight indentation mode for most programming
;; languages
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)



;;;;
;; Customization
;;;;



;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Editor settings
(load "editing.el")

;; UI related settings
(load "ui.el")

;; Elisp
(load "elisp-editing.el")

;; Clojure Settings
(load "setup-clojure.el")

;; JavaScript Settings
(load "setup-js.el")
;; (load "javascript.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-preview-mode cider prettier-js yaml-mode flycheck js2-mode web-mode guess-style auctex web-beautify elpy minimap markdown-mode pdf-tools highlight-indent-guides highlight-indentation multiple-cursors smartparens go-mode typescript-mode erlang neotree swiper company-quickhelp company zenburn-theme smex rainbow-delimiters projectile paredit magit helm exec-path-from-shell auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
