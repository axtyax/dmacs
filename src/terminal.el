(defun toggle-terminal ()
  "Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*ansi-term*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (progn
	;; Use the default shell specified in the SHELL env variable
        (ansi-term (getenv "SHELL"))
        (setq show-trailing-whitespace nil)))))

(global-set-key (kbd "C-x t") 'toggle-terminal)
