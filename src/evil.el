(evil-mode 1)

(setq evil-default-state 'normal)

(defun kill-all-buffers ()
  "Kills all buffers."
  (interactive)
  (mapc (lambda (buffer)
	  (progn
	    (set-buffer buffer)
	    (kill-buffer buffer))) (buffer-list)))


;; Set the evil leader key to <SPC>
(evil-set-leader 'normal (kbd "<SPC>") 'nil)

(evil-global-set-key 'normal (kbd "<leader> <left>") 'awesome-tab-backward)
(evil-global-set-key 'normal (kbd "<leader> <right>") 'awesome-tab-forward)

(evil-global-set-key 'normal (kbd "<leader> q <left>") 'awesome-tab-move-current-tab-to-left)
(evil-global-set-key 'normal (kbd "<leader> q <right>") 'awesome-tab-move-current-tab-to-right)

(evil-global-set-key 'normal (kbd "<leader> b k") 'kill-buffer)
(evil-global-set-key 'normal (kbd "<leader> b s") 'save-buffer)
(evil-global-set-key 'normal (kbd "<leader> b f") 'find-file)

(evil-global-set-key 'normal (kbd "<leader> t o") 'treemacs)
(evil-global-set-key 'normal (kbd "<leader> t d") 'treemacs-select-directory)

(evil-global-set-key 'normal (kbd "<leader> w <left>") 'windmove-left)
(evil-global-set-key 'normal (kbd "<leader> w <right>") 'windmove-right)
(evil-global-set-key 'normal (kbd "<leader> w k") 'delete-window)
