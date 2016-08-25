(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(setq frame-title-format '("%b"))


(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(scroll-bar-mode -1)
(column-number-mode)

(show-paren-mode)

(winner-mode t)



(windmove-default-keybindings)

(package-initialize)

(use-package package
  :ensure t
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")   t))

;; (use-package evil-smartparens
;;   :config
;;   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
;;   (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)))

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")   t)

;; (package-initialize)

(use-package alchemist
  :ensure t
  :config
  (setq alchemist-key-command-prefix (kbd "C-c a"))
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
  )

;; (use-package tabbar-ruler
;;   :ensure t
;;   :init
;;   (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;;   ;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
;;   ;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;;   (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;;   ;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move

;;   ;; The default behavior for tabbar-ruler is to group the tabs by frame. You can change this back to the old-behavior by
;;   ;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;;   :config
;;   ;; you can also group by projectile project easily by
;;   ;; (tabbar-ruler-group-by-projectile-project)
;;   )

(use-package evil-search-highlight-persist
  :ensure t
  :config
  (global-evil-search-highlight-persist t)
  (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)
  )


(ido-mode)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ;; ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;; smarter fuzzy matching for ido
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode +1)
;; ;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)



;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(projectile-globally-ignored-file-suffixes (quote ("png" "jpeg" "jpg")))
 '(projectile-sort-order (quote recentf)))
;; start maximized


(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package desktop+
  :ensure t
  )

;; {{{ auto-complete
(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  ;; AC does not complete when you are in face specified by ac-disable-faces. It is '(font-lock-comment-face font-lock-string-face font-lock-doc-face) by default
  (setq ac-disable-faces nil)

  (setq-default ac-sources '(ac-source-words-in-all-buffer
                             ac-source-words-in-buffer
                             ac-source-files-in-current-dir))

  (defun auto-complete-mode-maybe ()
    "Overwrite auto-complete-mode-maybe which by defaults turns autocomplete only on for buffers listed in ac-modes."
    (unless (minibufferp (current-buffer))
      (auto-complete-mode 1)))
  (setq  ac-use-fuzzy t)
  (setq ac-ignore-case (quote smart))
  (global-auto-complete-mode t)
  )
;; (defun ac-common-setup ()
;; (setq ac-sources (append ac-sources '(ac-source-dictionary))))
;; }}} / auto-complete

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode)
  )

;; (global-undo-tree-mode)
;; (setq undo-tree-auto-save-history "~/.emacs.d/.undo-tree-history")
;; (setq undo-tree-auto-save-history t)
;; (global-set-key (kbd "M-/") 'undo-tree-visualize)

;; (global-set-key (kbd "C-M-z") 'switch-window)
;; (global-set-key (kbd "C->") 'ace-jump-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-light t)

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode)
  )

;; {{{ volatile-highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)

  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )
;; }}} /volatile-highlights


(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode)

(global-linum-mode t)
(setq linum-format "%3d \u2502 ")

;; {{ change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((evil-visual-state-p) '("#00ff00" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))
;; }}


;; (require 'powerline)
;; (powerline-default-theme)
;; (powerline-vim-theme)
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(helm-autoresize-mode 1) ; experimental

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

;; C-c p f = helm-projectile-find-file (fuzzy find file in current project)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'helm-projectile-ag)



;; backup folder in ~/.emacs
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
;; -------------------

(require 'anzu)
(global-anzu-mode +1)

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250)
  (global-set-key (kbd "s-r") 'helm-recentf)
  )


;; appearence
(set-frame-font "Hack-14")

(rainbow-identifiers-mode)

(require 'color-identifiers-mode)
(global-color-identifiers-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; {{{ for ruby
(setq ruby-insert-encoding-magic-comment nil) ; prevents adding comment with encoding to *.rb files

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
;; Both of the above work only when the connection to the Ruby subprocess has been established. To do that, either use one of the core Robe commands, or type M-x robe-start.
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; }}} /ruby

;; (with-eval-after-load 'robe-mode
;;             (modify-syntax-entry ?- "_")
;;             (modify-syntax-entry ?_ "_"))

;; (with-eval-after-load 'robe
;;             (modify-syntax-entry ?- "_")
;;             (modify-syntax-entry ?_ "_"))

;; hyphen is a part of word
;; (modify-syntax-entry ?- "_")
;; (modify-syntax-entry ?_ "_")

;; This makes * and # use emacs-symbols instead of words
;; (e.g.) mamb_ctrl_scaleConversion
(setq-default evil-symbol-word-search t)

;; {{{ comments
(setq ess-fancy-comments nil) ;; Comments are also handled specially by ESS, using an idea borrowed from the Emacs-Lisp indentation style (http://stackoverflow.com/questions/26312317/wrong-indentation-of-comments-in-emacs)

;; gcc and gc
(unless (package-installed-p 'evil-commentary)
  (package-install 'evil-commentary))

(require 'evil-commentary)
(evil-commentary-mode)


;; }}} /comments

;; {{{ tuning evil-mode
(define-key evil-normal-state-map (kbd "[ b") 'previous-buffer) ;; Go to previous buffer
(define-key evil-normal-state-map (kbd "] b") 'next-buffer) ;; Go to next buffer

;; browsing with j/k long wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(require 'evil-exchange)
;; change default key bindings (if you want) HERE
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

(setq evil-want-fine-undo 'fine) ;; http://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
;; }}} /tuning evil-mode




;; {{{ mouse scroll
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; do accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; {{{ /mouse scroll


;; {{{ indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(global-set-key (kbd "s-M-<right>") 'hs-show-block)
(global-set-key (kbd "s-M-<left>") 'hs-hide-block)

(use-package editorconfig
  :init
  ;; (add-hook 'sh-mode-hook
  ;;           (lambda ()
  ;;             (editorconfig-mode 0)))
  (editorconfig-mode 1))

;; }}} /indentation



;; TODO: remove me please
;; (require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-local-mode)
;; ;; (global-set-key (kbd "<escape>") 'god-mode-all)

;; (defun my-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;; { key binding
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
;; } // key binding



(unless (package-installed-p 'inf-ruby)
  (package-install 'inf-ruby))

(unless (package-installed-p 'ac-inf-ruby)
  (package-install 'ac-inf-ruby))

(use-package elixir-mode
  :ensure t
  :config
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1)))
  )

;; {{{ matchit
(unless (package-installed-p 'evil-matchit)
  (package-install 'evil-matchit))
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(setq evilmi-ignore-comments nil)
;; }}} / matchit

;; copying important environment variables from the user's shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; {{{ History
;; save history
(savehist-mode 1)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; It's also worth pointing out that you can persist other variables across sessions by adding them to savehist-additional-variables
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(setq savehist-file "~/.emacs.d/savehist")
;; }}} History

;; {{{ org-mode
(require 'org)
;; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; }}} /org-mode




(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-disable-autocompletion t)
(local-set-key (kbd "RET") 'newline-and-indent)

(add-to-list 'ac-modes 'web-mode)
(setq web-mode-extra-snippets
      '(("erb" . (("=" . "<%= | %>")))))
(put 'scroll-left 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
