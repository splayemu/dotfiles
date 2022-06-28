;;; init.el ---

;; ********
;; BEGIN Custom Functions
;; ********

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

(defun camdez/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

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

;; ********
;; END Custom Functions
;; ********

;; ********
;; BEGIN Package Repos
;; ********
;; Packages are modules that you can use to modify emacs.
;; `use-package` is a very convienient way to install emacs packages.
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
  ;; run this if you can't install packages
  (package-refresh-contents)

  (package-install 'use-package))
(require 'use-package)

;; refresh keyring
;; if signature is failing when downloading a package, you can silence the check and then turn it on again
(setq package-check-signature nil)

(use-package gnu-elpa-keyring-update
  :ensure t)
;;(setq package-check-signature "allow-unsigned")

;; ********
;; END Package Repos
;; ********

(set-exec-path-from-shell-PATH)

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

;; use line numbers
(global-display-line-numbers-mode)
(set-default 'truncate-lines t)
;; (add-hook 'diff-mode-hook (lambda () (setq truncate-lines t)))

(use-package wgrep
  :ensure t)

;; Ivy overrights the search buffers in emacs with fuzzy completion
;; It's comperable to helm
(use-package ivy
  :ensure t
  :init
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-hydra
  :ensure t)

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

(setq neo-window-width 40)

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

(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Parens configuration
(use-package smartparens
  :ensure t
  :init
  (setq smartparens-strict-mode t))

;; Delete up to but not the parenthesis
(use-package evil-cleverparens
  :ensure t)

;; terraform specific configuration
(use-package terraform-mode
  :ensure t)

;; markdown mode configuration
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; yaml specific configuration
(use-package yaml-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; dart time
(use-package lsp-mode
  :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))
(use-package lsp-ui
  :ensure t)
(use-package hover
  :ensure t)

;; emacs lisp specific
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(sp-with-modes 'emacs-lisp-mode
  (sp-local-pair "'" nil :actions nil)
  )

;; Scheme specific
(use-package geiser-mit
  :ensure t)

(add-hook 'scheme-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(sp-with-modes 'scheme-mode
  (sp-local-pair "'" nil :actions nil)
  )

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
  (setq cider-font-lock-reader-conditionals nil)
  (setq cider-test-defining-forms '("deftest" "defspec" "defflow" "defflow-i18n")))

;; Adds options to all cider-jack-in commands
(setq cider-lein-global-options "with-profile dev")

(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; company adds the auto complete in clojure code
(use-package company
  :ensure t)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; better default ui
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq-default indent-tabs-mode nil)

;; ********
;; BEGIN org-mode
;; ********
;; https://systemcrafters.net/build-a-second-brain-in-emacs/getting-started-with-org-roam/
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam-notes")
  (org-roam-completion-everywhere 't)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ;; used for a new person
     ("p" "person" plain
      (file "~/org-roam-notes/templates/person-template.org")
      :if-new (file+head "person-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Person: ${title}\n#+filetags: Person\n")
      :unnarrowed t)
     ("f" "feedback" plain
      (file "~/org-roam-notes/templates/feedback-template.org")
      :if-new (file+head "feedback-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Feedback: ${title}\n#+filetags: feedback\n")
      :unnarrowed t)
     ;; for documenting a service
     ("s" "service" plain
      (file "~/org-roam-notes/templates/service-template.org")
      :if-new (file+head "service-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Service: ${title}\n#+filetags: Service\n")
      :unnarrowed t)
     ;; used for a 1:1
     ("c" "conversation" plain
      "%?"
      :if-new (file+head "conversation-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Conversation: ${title} %U\n#+date: %U\n#+filetags: Conversation\n")
      :unnarrowed t)
     ("m" "conversation2" plain
      "%?"
      :if-new (file+head "conversation2-%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Conversation: ${person} %U\n#+date: %U\n#+filetags: Conversation\n")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ;; When typing something, you can run this to autocomplete to org roam nodes
         ("C-c o c"   . completion-at-point))
  :config
  (org-roam-setup))

(use-package org-download
  :ensure t
  :after org)

(setq-default org-download-image-dir "~/Pictures/org")
(setq-default org-download-screenshot-method "screencapture -i %s")

;; ******
;; END org-mode
;; ******

;; ******
;; BEGIN NUBANK
;; ******
(use-package isa
	     :load-path "~/dev/nu/isa.el"
	     :config
	     ;; if you use vim keys
	     ;;(map! :leader :desc "isa" "N i" #'isa)
	     ;; if you use emacs keys
	     ;;(define-key global-map (kbd "C-c i") #'isa)
             )

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)
    (require 'nu-datomic-query nil t)))

;; ******
;; END NUBANK
;; ******

;; Custom keybinding
(use-package general
  :ensure t)

(general-define-key
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
  "wL"  '(windmove-swap-states-right :which-key "swap right")
  "wH"  '(windmove-swap-states-left :which-key "swap left")
  "wK"  '(windmove-swap-states-up :which-key "swap up")
  "wJ"  '(windmove-swap-states-down :which-key "swap bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wd"  '(delete-window :which-key "delete window")
  ;; Others
  "at"  '(ansi-term :which-key "open terminal")
  "fd"  '(cust-find-user-init-file :which-key "open init.el")
  "fy"  '(camdez/show-buffer-file-name :which-key "copy path of buffer to clipboard")
  "gl"  '(git-link :which-key "open code in github")
  ;; projectile
  "pp"  '(projectile-switch-project :which-key "find project")
  "pf"  '(projectile-find-file :which-key "fuzzy search find file")
  "ps"  '(counsel-ag :which-key "fuzzy search in project")
  "pb"  '(projectile-switch-to-buffer :which-key "find buffer")
  "pk"  '(projectile-kill-buffers :which-key "kill buffers")
  "pt"  '(cust-neotree-project-dir-toggle :which-key "show tree")
  "pi"  '(projectile-invalidate-cache :which-key "clears cache")
  ;; search
  "ss"  '(swiper-isearch :which-key "search")
  ;; clojure
  "c;"  '(cider-jack-in :which-key "create cider repl")
  "cc"  '(cider-connect :which-key "connect to repl")
  "cs"  '(cider-switch-to-repl-buffer :which-key "switch to repl")
  "ch"  '(cider-doc :which-key "get doc of clojure var")
  "cg"  '(cider-find-var :which-key "go to definition of clojure var")
  "ta"  '(cider-test-run-ns-tests :which-key "run all tests ns")
  "tt"  '(cider-test-run-test :which-key "run test at point")
  ;; org mode
  "oy" '(org-download-screenshot :which-key "takes a screenshot and adds it to the org document")
  "oY" '(org-download-yank :which-key "gets an image url and adds it to the org document")
  "onl" '(org-roam-buffer-toggle :which-key "shows the backlinks for the org roam node")
  "onf" '(org-roam-node-find :which-key "finds or creates a new org roam node")
  "oni" '(org-roam-node-insert :which-key "inserts a new org roam node into the document")
  "oc" '(completion-at-point :which-key "autocompletes to org roam nodes")

  ;; nubank
  ;; https://github.com/nubank/nudev/blob/master/ides/emacs/nu.el

  )

;; Define common keybindings with different functions per major mode
(general-define-key
  :keymaps 'clojure-mode-map
  :states '(normal visual emacs evil)

  "SPC ee" '(cider-eval-last-sexp :which-key "eval expr")
  "SPC eb" '(cider-load-buffer :which-key "eval buffer")
  "SPC ef" '(cider-eval-defun-at-point :which-key "eval form")
  )

(general-define-key
  :keymaps 'scheme-mode-map
  :states '(normal visual emacs evil)

  "SPC ee" 'geiser-eval-last-sexp
  "SPC eb" 'geiser-eval-buffer
  "SPC c;" 'run-geiser
  ;; create new repl
  ;; eval form
  ;; jump to repl
  ;; jump to definition
  ;; get docs
  )

(general-define-key
  :keymaps 'emacs-lisp-mode-map
  :states '(normal visual emacs evil)

  "SPC ee" 'eval-last-sexp
  "SPC eb" 'eval-buffer
  "SPC ef" 'eval-defun
  ;; create new repl
  ;; eval form
  ;; jump to repl
  ;; jump to definition
  ;; get docs
  )

;; Lisp Modes!!
;; Adds in a bunch of keybindings
(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (progn
            (evil-lisp-state-leader "SPC k")))

;; How to use:

;; How do I reload the init file without closing emacs?
;; `(eval-buffer)`

;; How do I fuzzy search for text in a file?
;; How do I fuzzy search for filenames?
;; How do I look up keybindings?
;;   C-h k keybinding

;; TODO:
;; improve file navigation
;; local cross project earch
;; how to easily clone and open a new project

(setq-default word-wrap t)

;; Auto Generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-mode nil)
 '(package-selected-packages
   '(evil-lisp-state geiser-mit org-download org-roam hover lsp-ui lsp-dart lsp-mode markdown-mode wgrep ivy-hydra ivy-hyrda rainbow-delimiters company yaml-mode ws-butler git-link terraform-mode gnu-elpa-keyring-update undo-tree evil-cleverparens smartparens cider neotree neo-tree counsel clojure-mode swiper-helm evil use-package))
 '(warning-suppress-log-types '((auto-save) (auto-save)))
 '(warning-suppress-types '((auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
