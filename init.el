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

(defvar my-packages
  '(
    ;; Helm
    helm

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;;smex

    ;; Project navigation
    projectile

    ;; Autocomplete functionality
    company
    company-quickhelp
    
    ;;
    paredit

    ;; Better search tool
    swiper

    ;;
    which-key
    
    ;;
    rainbow-delimiters

    ;; A collection of paredit-like functions for editing in html-mode.
    ;; https://github.com/magnars/tagedit

    ;; Zenburn theme
    zenburn-theme

    ;; 
    switch-window
    ace-jump-mode

    ;;
    undo-tree
    
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
(add-hook 'after-init-hook 'global-company-mode)

;; Set company completion delay
(setq company-idle-delay 0)

;; Starts ido-mode
(ido-mode)

;; Undo tree mode
(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-visualize)

;; Better C-x o 
(global-set-key (kbd "C-x o") 'switch-window)

;; Moving caret with C->
(global-set-key (kbd "C->") 'ace-jump-mode)

;; Better search
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)



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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (swiper company-quickhelp company zenburn-theme smex rainbow-delimiters projectile paredit magit helm exec-path-from-shell auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
