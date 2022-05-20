;; Fix system clipboard interoperability
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; Remove GUI clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Frames

;; Windows
(window-divider-mode)

;; Text Appearance
(setq tab-width 4)
(setq frame-resize-pixelwise t)
(set-face-attribute 'default nil :height 120)

;; Syntax Tools
