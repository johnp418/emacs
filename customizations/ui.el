;; Load theme
(load-theme 'zenburn t)

;; Show line numbers
(global-linum-mode 1)

;; Rainbow !
(rainbow-delimiters-mode 1)

;; Show column number
(column-number-mode)

;; Hide menubar
(menu-bar-mode -1)

;; Hide toolbar
(tool-bar-mode -1)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlights current line
(global-hl-line-mode 1)

;; Cursor box
(set-default 'cursor-type 'box)

;; Region box
(set-face-attribute 'region nil :background "#000")

;; No cursor blinking, it's distracting
(blink-cursor-mode -1)


;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))
