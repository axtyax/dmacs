;; File prefix
(define-prefix-command 'file-prefix-map)
(global-set-key (kbd "M-3") 'file-prefix-map)

;; Focus treemacs
(define-key file-prefix-map (kbd "f") 'treemacs-select-window)

;; Get rid of all buffers
(define-key file-prefix-map (kbd "k") 'clear-buffer-list)
