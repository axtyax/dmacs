;; Install this file as being in the emacs config directory
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Install 'straight.el' for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-pull-recipe-repositories)

(prin1 "Starting tilemacs initialization...")

;; Require all dependencies

(defun gassoc (key alist) (cdr (assoc key alist)))

(defun load-tilemacs-modules (tilemacs-modules)
 (dolist (mod tilemacs-modules)
  (progn
   (print (gassoc 'symbol mod))
   (straight-use-package (gassoc 'symbol mod))
   (print "Loaded module...")
   (print (gassoc 'symbol mod))
  )
 )
 "Load each module, using straight.el to install it (if necessary)."
)

(defvar dependency-modules
  (list
   ;;; Evil mode
   '((symbol . evil))

   ;;; External Modules
   '((symbol . doom-themes))
   '((symbol . all-the-icons))
   '((symbol . f))
   '((symbol . lsp-mode))
   '((symbol . lsp-ui))
   '((symbol . company))
   '((symbol . which-key))
   '((symbol . awesome-tab))
   '((symbol . flycheck))
   '((symbol . flycheck-pos-tip))
   '((symbol . treemacs))
    
    ;; Syntax visuals packages
   '((symbol . rust-mode))
   '((symbol . typescript-mode))
   '((symbol . rjsx-mode))
  )
  "A list of alists corresponding to module installations."
)

(load-tilemacs-modules dependency-modules)

;; Initialize lsp and various language-specific modes
(defun hook-lsp (hook)
 (add-hook hook #'lsp))

(defun hook-language-mode-lsp (filename-regex mode-hook mode-callback)
    (add-to-list 'auto-mode-alist '(filename-regex . mode-callback))
    (hook-lsp mode-hook))

(hook-language-mode-lsp "\\.rs\\'" 'rust-mode-hook 'rust-mode)
(hook-language-mode-lsp "\\.tsx?\\'" 'typescript-mode-hook 'typescript-mode)

;; Configure aesthetics

;; Fix system clipboard interoperability
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; Remove GUI clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode t)
(column-number-mode t)

;; Windows
(window-divider-mode)

;; Text Appearance
(setq tab-width 4)
(setq frame-resize-pixelwise t)
(set-face-attribute 'default nil :height 120)


;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-dark+ t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Configure windows, tabs, and posframes



;; Configure syntax highlighting



;; Configure file expoloration



;; Configure local and global regex, symbol, and file search



;; Normal emacs bindings



;; Evil mode bindings
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
(evil-global-set-key 'normal (kbd "<leader> w <up>") 'windmove-up)
(evil-global-set-key 'normal (kbd "<leader> w <down>") 'windmove-down)
(evil-global-set-key 'normal (kbd "<leader> w k") 'delete-window)


;; ;; --- Initialize Tilemacs ---
;; (defvar tilemacs-modules
;;   (list
;;    ;;; Evil mode
;;     '((symbol . evil)
;;      (is-external . true)
;;      (hook . "evil.el"))   

;;    ;;; External Modules
;;     '((symbol . f)
;;      (is-external . true)
;;      (hook . nil))

;;     '((symbol . lsp-mode)
;;      (is-external . true)
;;      (hook . "lsp-mode.el"))
    
;;     '((symbol . lsp-ui)
;;      (is-external . true)
;;      (hook . nil))

;;     '((symbol . company)
;;      (is-external . true)
;;      (hook . "company.el"))

;;     '((symbol . which-key)
;;      (is-external . true)
;;      (hook . "which-key.el"))
    
;;     '((symbol . doom-themes)
;;      (is-external . true)
;;      (hook . "theme.el"))
    
;;     '((symbol . awesome-tab)
;;      (is-external . true)
;;      (hook . "tabs.el"))
    
;;     '((symbol . flycheck)
;;      (is-external . true)
;;      (hook . nil))
    
;;     '((symbol . flycheck-pos-tip)
;;      (is-external . true)
;;      (hook . nil))
    
;;     '((symbol . treemacs)
;;      (is-external . true)
;;      (hook . "treemacs.el"))
    
;;     ;; Syntax visuals packages
;;     '((symbol . rainbow-identifiers)
;;      (is-external . true)
;;      (hook . "rainbow-identifiers.el"))

;;     '((symbol . rust-mode)
;;      (is-external . true)
;;      (hook . "rust-mode.el"))

;;      '((symbol . typescript-mode)
;;      (is-external . true)
;;      (hook . "typescript-mode.el"))

;;      '((symbol . rjsx-mode)
;;      (is-external . true)
;;      (hook . "rjsx-mode.el"))
     
;;     ;;; Internal Modules
;;     '((symbol . cursor-movement)
;;      (is-external . false)
;;      (hook . "cursor-movement.el"))

;;     '((symbol . interface)
;;       (is-external . false)
;;       (hook . "interface.el"))
        
;;     '((symbol . terminal)
;;      (is-external . false)
;;      (hook . "terminal.el"))

;;   )
;;   "A list of alists corresponding to module installations."
;; )
    
;; (load-tilemacs-modules)

;; (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
;;   backup-by-copying t    ; Don't delink hardlinks
;;   version-control t      ; Use version numbers on backups
;;   delete-old-versions t  ; Automatically delete excess backups
;;   kept-new-versions 20   ; how many of the newest versions to keep
;;   kept-old-versions 5    ; and how many of the old
;;   )

;; (prin1 "Emacs initialization complete.")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
