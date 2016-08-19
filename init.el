(setq framt-title-format "emacs")
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(scroll-bar-mode -1)
(column-number-mode)

(show-paren-mode)

(winner-mode t)

(windmove-default-keybindings)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")   t)

(package-initialize)

(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))

(require 'alchemist)

(setq alchemist-key-command-prefix (kbd "C-c a"))
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

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
(flx-ido-mode +1)



(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'desktop+)

;; {{{ auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (defun ac-common-setup ()
;; (setq ac-sources (append ac-sources '(ac-source-dictionary))))

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
;; }}} / auto-complete

(require 'autopair)
(autopair-global-mode)

(global-undo-tree-mode)
(setq undo-tree-auto-save-history "~/.emacs.d/.undo-tree-history")
;; (setq undo-tree-auto-save-history t)
;; (global-set-key (kbd "M-/") 'undo-tree-visualize)

(global-set-key (kbd "C-M-z") 'switch-window)
(global-set-key (kbd "C->") 'ace-jump-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-light t)

(require 'evil-visualstar)
(global-evil-visualstar-mode)

;; {{{ volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

(vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                      'evil-paste-pop 'evil-move)
(vhl/install-extension 'evil)

(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)
;; }}} /volatile-highlights


(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(require 'evil)
;; TODO: you should definetly read evil-mode documentation
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode)


(global-linum-mode t)
(setq linum-format "%4d \u2502 ")


;; {{ change mode-line color by evil state
(require 'cl)

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
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
;; -------------------

(require 'anzu)
(global-anzu-mode +1)

(require 'recentf)			;
(recentf-mode 1)
(setq recentf-max-menu-items 250)
(global-set-key (kbd "s-r") 'helm-recentf)

;; (init-open-recentf)


;; appearence
(set-frame-font "Hack-15")

(rainbow-identifiers-mode)


;; {{{ for ruby
(setq ruby-insert-encoding-magic-comment nil) ; prevents adding comment with encoding to *.rb files

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
;; Both of the above work only when the connection to the Ruby subprocess has been established. To do that, either use one of the core Robe commands, or type M-x robe-start.
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; }}} /ruby

;; hyphen is a part of word
(modify-syntax-entry ?- "_")
(modify-syntax-entry ?_ "_")


;; {{{ comments
(setq ess-fancy-comments nil) ;; Comments are also handled specially by ESS, using an idea borrowed from the Emacs-Lisp indentation style (http://stackoverflow.com/questions/26312317/wrong-indentation-of-comments-in-emacs)
;; }}} /comments




;; {{{ mouse scroll
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; do accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; {{{ /mouse scroll


;; {{{ indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(global-set-key (kbd "s-M-<right>") 'hs-show-block)
(global-set-key (kbd "s-M-<left>") 'hs-hide-block)
;; }}} /indentation



;; TODO: remove me please
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
;; (global-set-key (kbd "<escape>") 'god-mode-all)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;; { key binding
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
;; } // key binding



(unless (package-installed-p 'inf-ruby)
  (package-install 'inf-ruby))

(unless (package-installed-p 'ac-inf-ruby)
  (package-install 'ac-inf-ruby))

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))


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

;; It's also worth pointing out that you can persist other variables across sessions by adding them to savehist-additional-variables
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(setq savehist-file "~/.emacs.d/savehist")
;; }}} History
