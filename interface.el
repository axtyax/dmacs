;; Fix system clipboard interoperability
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; Copy to and Paste from system clipboard
(global-set-key (kbd "C-x c") 'clipboard-kill-region)
(global-set-key (kbd "C-x y") 'clipboard-yank)

;; Remove GUI clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Frames

;; Windows

;; Text Appearance
(setq tab-width 4)
(setq frame-resize-pixelwise t)
(set-face-attribute 'default nil :height 120)

;; Cursor movement


;; Text Selection
