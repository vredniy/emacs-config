(setq framt-title-format "emacs")

(setq inhibit-startup-message t)        ; Disable startup message

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

					;(set-default 'cursor-type 'hbar)




(column-number-mode)

(show-paren-mode)

(global-hl-line-mode)

(winner-mode t)

(windmove-default-keybindings)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")   t)



;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

;(unless package-archive-contents    ;; Refresh the packages descriptions
;  (package-refresh-contents))
;(setq package-load-list '(all))     ;; List of packages to load
;(unless (package-installed-p 'org)  ;; Make sure the Org package is
;  (package-install 'org))           ;; installed, install it if not
;(package-initialize)                ;; Initialize & Install Package
;; (setq org-...)



(package-initialize)


(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))

(setq alchemist-key-command-prefix (kbd "C-c a"))


(ido-mode)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ;ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)



(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(nlinum-mode)

(require 'autopair)
(autopair-global-mode)

(global-undo-tree-mode)

(global-set-key (kbd "M-/") 'undo-tree-visualize)


(global-set-key (kbd "C-M-z") 'switch-window)

(global-set-key (kbd "C->") 'ace-jump-mode)

(load-theme 'solarized-light t)


(require 'evil)
; TODO: you should definetly read evil-mode documentation
(evil-mode 1)

;; {{ change mode-line color by evil state
;(lexical-let ((default-color (cons (face-background 'mode-line)
;				   (face-foreground 'mode-line))))
;  (add-hook 'post-command-hook
;	    (lambda ()
;	      (let ((color (cond ((minibufferp) default-color)
;				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;				 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;				 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;				 (t default-color))))
;		(set-face-background 'mode-line (car color))
;		(set-face-foreground 'mode-line (cdr color))))))
;; }}

;An example of defining a Key Translation: You could translate ch to C-h and cx to C-x. To do this, make c a Prefix Key so as Key Lookup does not end on the c input alone.
;
;   (define-key evil-normal-state-map "c" nil)
;   (define-key evil-motion-state-map "cu" 'universal-argument)
;The second line automatically made c a Prefix Key, and its binding was somewhat arbitrary. Now we define the Key Translations:
;
;   (define-key key-translation-map (kbd "ch") (kbd "C-h"))
;   (define-key key-translation-map (kbd "cx") (kbd "C-x"))

(require 'powerline)
(powerline-default-theme)
(powerline-vim-theme)

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

; C-c p f = helm-projectile-find-file (fuzzy find file in current project)

; -------------------

(require 'anzu)
(global-anzu-mode +1)

(require 'recentf)			;
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(init-open-recentf)


					; appearence
;(set-face-attribute 'default nil :font "Hack")
(set-frame-font "Hack-15")

(rainbow-identifiers-mode)


					; for ruby
(setq ruby-insert-encoding-magic-comment nil) ; prevent adding comment with encoding to *.rb files
