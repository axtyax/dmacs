(awesome-tab-mode t)

(define-prefix-command 'tab-prefix-map)
(global-set-key (kbd "M-2") 'tab-prefix-map)

;; Move between tabs
(define-key tab-prefix-map (kbd "<left>") 'awesome-tab-backward)
(define-key tab-prefix-map (kbd "<right>") 'awesome-tab-forward)
