;; Custom Functions
(defun cust-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun cust-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun cust-neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           ; (projectile-project-root)
           (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

;; Package Repos
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"          . "http://orgmode.org/elpa/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/") 
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; use-package makes it easy to maintain a list of current packages
;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; refresh keyring
;; if signature is failing when downloading a package, you can silence the check and then turn it on again
(setq package-check-signature nil)
(use-package gnu-elpa-keyring-update
  :ensure t)
(setq package-check-signature "allow-unsigned")

;; VIM modal editing
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Adds a nice undo mode
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)
;; need to set this or else evil mode doesn't know how to undo!
(evil-set-undo-system 'undo-tree)

;; Check out the themes here:
;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-peacock t))

;; Ivy overrights the search buffers in emacs with fuzzy completion
;; It's comperable to helm
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Counsel makes ivy completions more common
(use-package counsel
  :ensure t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq projectile-completion-system 'ivy)

;; Projectile encapsulates a project and provides helpful commands
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package neotree
  :ensure t)

;; Not sure what this does yet
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Git config
(use-package git-link
  :ensure t
  :init
  (progn
    ;; default is to open the generated link
    (setq git-link-open-in-browser t)))

;; Remove trailing whitespace
(use-package ws-butler
  :ensure t)

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
  "TAB" '(cust-switch-to-previous-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  "ff"  '(counsel-find-file :which-key "find files")
  ;; Buffers
  "bb"  '(ivy-switch-buffer :which-key "switch buffer")
  "bd"  '(kill-this-buffer :which-key "kill this buffer")
  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wd"  '(delete-window :which-key "delete window")
  ;; Others
  "at"  '(ansi-term :which-key "open terminal")
  "fd"  '(cust-find-user-init-file :which-key "open init.el")
  ;; projectile
  "pp"  '(projectile-switch-project :which-key "find project") 
  "pf"  '(projectile-find-file :which-key "fuzzy search find file")
  "ps"  '(counsel-ag :which-key "fuzzy search in project")
  "pb"  '(projectile-switch-to-buffer :which-key "find buffer")
  "pk"  '(projectile-kill-buffers :which-key "kill buffers")
  "pt"  '(cust-neotree-project-dir-toggle :which-key "show tree")
  ;; search
  "ss"  '(swiper-isearch :which-key "search")
  ;; clojure
  "ee"  '(cider-eval-last-sexp :which-key "eval expr")
  "eb"  '(cider-load-buffer :which-key "eval buffer")
  "ef"  '(cider-eval-defun-at-point :which-key "eval form")
  "c;"  '(cider-jack-in :which-key "create cider repl")
  "cc"  '(cider-connect :which-key "connect to repl")
  "cs"  '(cider-switch-to-repl-buffer :which-key "switch to repl")
  "ch"  '(cider-doc :which-key "get doc of clojure var")
  "cg"  '(cider-find-var :which-key "go to definition of clojure var")
  ;; lisp state - consider adding: https://github.com/syl20bnr/evil-lisp-state
  "ks"  '(paredit-forward-slurp-sexp :which-key "slurp next")
  "kb"  '(paredit-forward-barf-sexp :which-key "barf next")
  "kw"  '(paredit-wrap-sexp :which-key "wrap form")
  ;; git link
  "gl"  '(git-link :which-key "open code in github")
))

;; TODO: how to create new file from neotree?
;; TODO: also turn off evil mode in neotree

;; TODO: add line numbers
;; TODO: auto remove whitespace on save

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(use-package smartparens
  :ensure t
  :init
  (setq smartparens-strict-mode t))

(use-package evil-cleverparens
  :ensure t)

;; terraform specific configuration
(use-package terraform-mode
  :ensure t)

;; Clojure specific configuration
(use-package clojure-mode
  :ensure t)

;; Remove clojure specific matched quotes
;; See: https://smartparens.readthedocs.io/en/latest/pair-management.html#local-pair-definitions
(sp-with-modes 'clojure-mode
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" nil :actions nil)
  )

(use-package cider
  :ensure t
  :init
  (setq cider-font-lock-reader-conditionals nil))

;; Adds options to all cider-jack-in commands
(setq cider-clojure-cli-global-options "-A:dev-defaults:dev -Sforce")
(setq cider-clojure-cli-command "~/.direnv-clojure.sh")

(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)


;; better default ui
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq-default indent-tabs-mode nil)

;; Questions:
;; - how do I get autocomplete in the minibuffer?
;;   - when I am searching for commands
;;   - when I find files and such?
;;   - you can evaluate a function with `C-x C-e`
;; - Can I replicate the search for things and edit accross multiple files?

;; How to use:

;; How do I reload the init file without closing emacs?
;; `(eval-buffer)`

;;   How do I fuzzy search for text in a file?
;;   How do I fuzzay search for filenames?
;;   How do I look up keybindings?
;; C-h k keybinding
;;   How do I add a new mode?


;; Auto Generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-mode nil)
 '(package-selected-packages
   '(ws-butler git-link terraform-mode gnu-elpa-keyring-update undo-tree evil-cleverparens smartparens cider neotree neo-tree counsel clojure-mode swiper-helm evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
