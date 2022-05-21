(setq last-split-direction 'vertical)
(defun insert-tiled-window ()
  "Insert a new window"
  (interactive)
  (progn
	(if (eq last-split-direction 'vertical)
	 (progn
	   (split-window-right)
	   (windmove-right)
	   (setq last-split-direction 'horizontal))
	  (progn
		(split-window-below)
		(windmove-down)
		(setq last-split-direction 'vertical)))
    (balance-windows)
	(dired)
  )
  )

(defun remove-tiled-window ()
  "Remove the current window"
  (interactive)
  (delete-window)
)

(define-prefix-command 'cursor-prefix-map)
(global-set-key (kbd "C-c") 'cursor-prefix-map)

;; Copy to and Paste from system clipboard
(define-key cursor-prefix-map (kbd "c") 'clipboard-kill-region)
(define-key cursor-prefix-map (kbd "y") 'clipboard-yank)


(define-prefix-command 'window-prefix-map)
(global-set-key (kbd "M-1") 'window-prefix-map)

;; Create and remove windows
(define-key window-prefix-map (kbd "=") 'insert-tiled-window)
(define-key window-prefix-map (kbd "-") 'remove-tiled-window)

;; Move across windows
(define-key window-prefix-map (kbd "<left>") 'windmove-left)
(define-key window-prefix-map (kbd "<down>") 'windmove-down)
(define-key window-prefix-map (kbd "<up>") 'windmove-up)
(define-key window-prefix-map (kbd "<right>") 'windmove-right)
