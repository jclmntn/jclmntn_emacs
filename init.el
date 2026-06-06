;; ;; -*- lexical-binding: t; -*-
;; ;; Startup Configs 

(use-package emacs
  :custom
  ((custom-file "~/.emacs.d/emacs-custom.el")               ;; Define a localização do arquivo com opções customizadas
   (backup-directory-alist '(("." . "~/.emacs.d/backup/"))) ;; Define a localização dos backups
   (global-display-line-numbers-mode t)                     ;; Ativa a numeração global
   (display-line-numbers-type 'visual)                      ;; Ativa a numeração relativa
   (indent-tabs-mode nil)                                   ;; Good riddance, Tabs
   (xref-search-program 'ripgrep)
   (tab-always-indent 'complete)
   (inhibit-startup-message t)
   (ring-bell-function #'ignore)
   (ansi-color-for-compilation-mode t))
  :config
  ;; Inicia o Emacs com tela cheia
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Carrega arquivo de configurações
  (load custom-file)
  (dolist (mode '(eshell-mode-hook dired-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (pcase system-type
    (windows-nt
     ;; Ajustes de codificação quando estiver usando Windows
     (prefer-coding-system 'utf-8)
     (set-default-coding-systems 'utf-8)
     (set-terminal-coding-system 'utf-8)
     (set-keyboard-coding-system 'utf-8)
     (setq-default default-process-coding-system '(utf-8 . cp1252))
     ;; Tooling no Windows
     (add-to-list 'exec-path "C:/Users/jose/Portable/msys64/usr/bin/")
     (add-to-list 'exec-path "C:/Users/jose/Portable/fd")
     (add-to-list 'exec-path "C:/Users/jose/Portable/rg")
     (add-to-list 'exec-path "~/Portable/Git/bin")
     (add-to-list 'exec-path "~/Repos")
     (setenv "PATH" (mapconcat #'identity exec-path path-separator))))

  ;; Configurações de cores no compile-mode
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package ediff
  :custom
  ((ediff-split-window-function 'split-window-horizontally)
   (ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package dired
  :ensure nil
  :custom
  ((dired-kill-when-opening-new-dired-buffer t)
   (dired-dwim-target t)))

;; Adiciona o MELPA à lista de pacotes possíveis
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Deixando o Emacs mais bonito
;; Tema
(use-package modus-themes)
(load-theme 'modus-operandi)

;; ;; Fontes
(set-face-attribute 'default nil :font "IosevkaTerm" :height 140)
(set-face-attribute 'fixed-pitch nil :font "IosevkaTerm" :height 140)
(set-face-attribute 'variable-pitch nil :font "IosevkaTerm" :height 140 :weight 'regular)

;; Remove elementos visuais
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode '(20 . 0))

;; Nerd Fonts
(use-package nerd-icons
  :ensure t
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-corfu
  :ensure t
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)
  (with-eval-after-load 'marginalia
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-vcs-max-length 30))

;; Pulsar
(use-package pulsar
  :ensure t
  :bind
  ( :map global-map
    ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
    ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

;; Para não precisar poluir o ambiente com :ensure t
(use-package use-package
  :custom
  (use-package-always-ensure t))

;; Parênteses coloridos
(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

;; Configura o evil-mode e o evil-collection.
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  :ensure t
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (setq assocl-mark-set '(("À" . ?A) ("È" . ?E) ("Ì" . ?I) ("Ò" . ?O) ("Ù" . ?U) ("à" . ?a)
                          ("è" . ?e) ("ò" . ?o) ("ù" . ?u) ("U" . ?V) ("u" . ?v) ("?" . ?N)
                          ("?" . ?n) ("?" . ?W) ("?" . ?w) ("?" . ?Y) ("?" . ?y)))
  (setq assocl-line-set '(("Á" . ?A) ("É" . ?E) ("Í" . ?I) ("Ó" . ?O) ("Ú" . ?U) ("Ý" . ?y)
                          ("á" . ?a) ("é" . ?e) ("í" . ?i) ("ó" . ?o) ("ú" . ?u) ("ý" . ?y)
                          ("N" . ?N) ("n" . ?n) ("U" . ?V) ("u" . ?v) ("?" . ?W) ("?" . ?w)))
  (setq assocl-reg-set '(("Ä" . ?A) ("Ë" . ?E) ("Ï" . ?O) ("Ö" . ?U) ("Ü" . ?U) ("ä" . ?a)
                         ("ë" . ?e) ("ï" . ?i) ("ö" . ?o) ("ü" . ?u) ("ÿ" . ?y) ("Ÿ" . ?Y)
                         ("Ḧ" . ?H) ("ḧ" . ?h) ("Ẅ" . ?W) ("ẅ" . ?w) ("ẗ" . ?t)))
  (seq-mapn
   #'(lambda (item function)
       (define-key
        evil-normal-state-map (kbd (car item))
        (lambda () (interactive) (function (cdr item)))))
   assocl-mark-set
   '(evil-goto-mark evil-goto-mark-line evil-use-register)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-eat-setup))

;; Emacs EAT
(use-package eat
  :ensure t
  :config (eat-eshell-mode))

;; Árvore de undo
(use-package vundo)

;; Definição de leader keys
(use-package general
  :ensure t
  :config
  (general-create-definer jclmntn/leader-keys
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC"))

(jclmntn/leader-keys
  "RET" '(consult-bookmark :which-key)
  "/"   '(consult-ripgrep :which-key)
  "."   '(consult-fd :which-key)
  "i"   '(consult-imenu :which-key)
  "f"   '(consult-flymake :which-key)
  "oc"  '(org-capture :which-key)
  "oa"  '(org-agenda :which-key)
  "nn"  '(denote :which-key)
  "nr"  '(denote-rename-file :which-key)
  "nl"  '(denote-link :which-key)
  "nj"  '(denote-journal-new-or-existing-entry :which-key)
  "nf"  '(consult-notes :which-key)
  "nb"  '(denote-backlinks :which-key)
  "nwc" '(citar-create-note :which-key)
  "nwx" '(citar-denote-nocite :which-key))


;; Configurando o Vertico e amigos
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init (vertico-mode))

(use-package vertico-grid
  :ensure nil)

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)))

(use-package consult-imenu
  :after consult
  :ensure nil)


(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  :custom
  (consult-notes-file-dir-sources '(("Denote" ?d "~/Repos/Notes/denote-notes/")))) 

(use-package embark
  :ensure t
  :bind (
         ("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h b" . embark-bindings))
  :custom
  ((prefix-help-command #'embark-prefix-help-command))
  :config
  (keymap-set jinx-repeat-map "RET" 'jinx-correct)
  (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
  (add-to-list 'embark-default-action-overrides '(jinx . jinx-correct))
  (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
  (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
  (add-to-list 'embark-repeat-actions #'jinx-next)
  (add-to-list 'embark-repeat-actions #'jinx-previous)
  (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto-prefix 1)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (advice-add 'python-shell-completion-at-point :around
            (lambda (fun &optional arg)
              (cape-wrap-noninterruptible (lambda () (funcall fun arg))))))

(use-package cape
  :ensure t)

;; to integrate yasnippet-capf with eglot completion
;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot

;; Configuração do Eglot
(defun jclmntn/eglot-capf-with-yasnippet ()
  (setq-local completion-at-point-functions
              (list 
	       (cape-capf-super
		#'eglot-completion-at-point
		#'yasnippet-capf))))

(setq eglot-python/pyright-uvtool-command
      '("uv" "tool" "run" "--from" "pyright" "pyright-langserver" "--" "--stdio"))

(use-package eglot
  :hook (eglot-managed-mode . jclmntn/eglot-capf-with-yasnippet)
  :config
  (add-to-list 'eglot-server-programs `(python-mode . ,eglot-python/pyright-uvtool-command))
  (add-to-list 'eglot-server-programs `(python-ts-mode . ,eglot-python/pyright-uvtool-command)))

(use-package consult-eglot)

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))


;; Elementos visuais do org-mode
(defun jclmntn/org-font-setup ()
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "\U+2022"))))))

  ;; Set faces for headig levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch :foreground "#c3e88d" :weight 'bold)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun jclmntn/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode)
  (setq-local completion-at-point-functions
              (list 
               (cape-capf-super
                #'yasnippet-capf))))

(defun jclmntn/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

;; Org Mode
(use-package org
  :hook
  (org-mode . jclmntn/org-mode-setup)
  (org-babel-after-execute . jclmntn/babel-ansi)
  :ensure t
  :custom 
    ((org-todo-keywords
        '("TODO(t!)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)"))
    (org-todo-keyword-faces
	'(("TODO" . "#c3e88d")
	  ("NEXT" . "#c3e88d")
	  ("IDEA" . "LightBlue")
	  ("KILL" . "Red")))
    (org-capture-templates
     '(("i" "Idea" entry
        (file+olp "~/Repos/Notes/Tasks.org" "Caixa de Entrada") 
	"* IDEA %?\n%U\n %a\n %i"
        :empty-lines 1)
       ("l" "Log" entry
        (file+olp denote-journal-path-to-new-or-existing-entry "Logs")
        "* %U %?\n%i\n%a"
          :kill-buffer t
          :empty-lines 1)))
    (org-agenda-files '("~/Repos/Notes/Tasks.org"))
    (org-log-into-drawer t)
    (org-log-done 'time)
    (org-agenda-window-setup 'only-window)
    (org-agenda-restore-windows-after-quit t)
    (org-src-window-setup 'plain)
    (org-src-preserve-indentation t)
    (org-confirm-babel-evaluate nil)
    (org-plantuml-jar-path "~/.config/plantuml/plantuml-mit-1.2025.9.jar")
    (org-latex-src-block-backend 'engraved)
    (org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (org-cite-csl-link-cites nil)
    (org-cite-csl-nocitelinks-backends '(ascii md gfm))
    (org-refile-targets '(("~/Repos/Notes/Tasks.org" :maxlevel . 3))))
    :config
    (add-to-list 'org-src-lang-modes '("planuml" . plantuml))
    (jclmntn/org-font-setup)
    (org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (emacs-lisp . t)
      (python . t)
      (plantuml . t)
      (eshell . t)))
    (defun org--get-display-dpi ()
        "Hardcode display DPI to bypass PGTK/Wayland arithmetic overflow bug."
        200.0)
    ) 

(use-package plantuml-mode
  :custom
  ((plantuml-jar-path "~/.config/plantuml/plantuml-mit-1.2025.9.jar")
   (plantuml-default-exec-mode 'jar)))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-transclusion
  :after org)

;; Meio que foda-se ter isso
;; (defun jclmntn/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 120
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . jclmntn/org-mode-visual-fill))


;; Personal Knowledge Management
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :custom ((denote-templates '((journal . "* Logs"))))
  :config
  (setq denote-directory (expand-file-name "~/Repos/Notes/denote-notes"))
  ;; Denote buffers automatically renamed to have prefix + title
  (denote-rename-buffer-mode 1))


(use-package denote-journal
  :custom ((denote-journal-directory
            (expand-file-name "journal" denote-directory))
           (denote-journal-title-format 'day-date-month-year)))

(use-package citar
  :ensure t
  :custom
  ((citar-bibliography (file-expand-wildcards "~/Repos/Notes/bib/*.bib"))
  (citar-open-always-create-notes nil)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :hook (org-mode . citar-capf-setup))

(use-package citar-embark
 :ensure t
 :after citar embark
 :no-require
 :config (citar-embark-mode))

(use-package biblio)

(use-package biblio-openlibrary
  :vc (:url "https://github.com/fabcontigiani/biblio-openlibrary" :branch "master")
  :after biblio)

(use-package ebib
  :ensure t
  :custom (ebib-default-directory "~/Repos/Notes/bib/"))

(use-package ebib-biblio
  :ensure nil
  :after (ebib biblio)
  :bind (:map ebib-index-mode-map
              ("B" . ebib-biblio-import-doi)
              :map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))



(use-package citar-denote
  :ensure t
  :demand t ;; Ensure minor mode loads
  :after (:any citar denote)
  :custom
  ;; Package defaults
  (citar-denote-file-type 'org)
  (citar-denote-keyword "bib")
  (citar-denote-subdir "~/Repos/Notes/denote-notes/bib/")
  (citar-denote-template nil)
  (citar-denote-title-format "author-year-title")
  (citar-denote-title-format-andstr "and")
  (citar-denote-title-format-authors 1)
  :init
  (citar-denote-mode))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-action #'magit-display-buffer-same-window-except-diff-v1))

;; Não está funcionando atualmente, preciso entender o porquê.
;; Abri um PR no Forge.
(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; Programming Modes

(defun jclmntn-python-dynamic-shell-args (&rest _args)
  "Dynamically set python shell args based on pyproject.toml"
  (let ((pyproject-file (locate-dominating-file default-directory "pyproject.toml")))
    (when pyproject-file
      (let ((args (with-temp-buffer
                    (insert-file-contents (expand-file-name "pyproject.toml" pyproject-file))
                    (goto-char (point-min))
                    (cond
                     ((re-search-forward "kedro" nil t) "run kedro ipython --simple-prompt -i")
                     ((progn (goto-char (point-min)) (re-search-forward "ipython" nil t)) "run ipython")
                     (t "run python -i")))))
        (setq-local python-shell-interpreter-args args)
        (message "Python args set to: %s" args)))))

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-ts-mode))
  :hook ((python-ts-mode . display-fill-column-indicator-mode)
         (python-ts-mode . jclmntn-python-dynamic-shell-args)
         (python-ts-mode . jclmntn-python-dynamic-shell-args))
  :custom ((python-shell-interpreter "uv")
           (python-shell-interpreter-args "run python -i")
           (python-indent-offset 4)
           (python-indent-def-block-scale 1)
           (python-shell-prompt-detect-enabled nil))
  :config
  (advice-add 'run-python :before #'jclmntn-python-dynamic-shell-args)
  )

;; Yasnippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Repos/Notes/Snippets/"))
  (yas-global-mode 1))

(use-package yasnippet-capf
  :after cape
  :vc (:url https://github.com/elken/yasnippet-capf.git)) 

;; Spellchecking com o Jinx
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom (jinx-languages "pt_BR" "en_US"))

;; Abrir links com Navegador do Windows
;; Determine the specific system type. 
;; Emacs variable system-type doesn't yet have a "wsl/linux" value,
;; so I'm front-ending system-type with my variable: sysTypeSpecific.
;; I'm no elisp hacker, so I'm diverging from the elisp naming convention
;; to ensure that I'm not stepping on any pre-existing variable.
(setq-default sysTypeSpecific  system-type) ;; get the system-type value

(cond 
;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
((eq sysTypeSpecific 'gnu/linux)  
(when (string-match "Linux.*Microsoft.*Linux" 
                    (shell-command-to-string "uname -a"))

    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
    cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
    cmdExeArgs '("/c" "start" "") )
    (setq
    browse-url-generic-program  cmdExeBin
    browse-url-generic-args     cmdExeArgs
    browse-url-browser-function 'browse-url-generic)
    )))

;; Elfeed
(use-package elfeed
  :custom ((elfeed-feeds '(("https://www.tandfonline.com/feed/rss/cjas20" journal stats)
                           ("https://hdsr.mitpress.mit.edu/rss.xml" blog data)
                           ("https://rss.sciencedirect.com/publication/science/01482963" journal business stats)
                           ("http://feeds.harvardbusiness.org/harvardbusiness/" blog business)
                           ("https://www.insurancejournal.com/rss/news" news insurance)
                           ("https://www.nexojornal.com.br/rss.xml" news brazil)
                           ("https://www.counting-stuff.com/rss" blog stats)
                           ("https://blog.miguelgrinberg.com/feed" blog python)
                           ("https://grouplens.org/feed/" blog computing)))))

(use-package engrave-faces)

;;(use-package pandoc-mode)
(use-package ox-pandoc)

(use-package project
  :custom
  ((project-mode-line t)
   (project-vc-extra-root-markers '("pyproject.toml"))))

(use-package just-mode)

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
         '(("TODO"   . "#FF0000")
           ("FIXME"  . "#FF0000"))))
    

(with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t))


;; Para buildar o Blog
(use-package htmlize
  :ensure t)

;; Para trabalhar com o Xournal
;; (add-to-list 'load-path "~/.emacs.d/manual-packages/org-xournalpp")



;; Spellchecking
(use-package jinx
    :if (eq system-type 'gnu/linux)
    :hook (emacs-startup . global-jinx-mode)
    :bind (("M-$" . jinx-correct)
        ("C-M-$" . jinx-language))
    :custom (jinx-languages "pt_BR" "en_US"))

;; Gestão de citações
(use-package ebib
  :custom ((ebib-default-directory "~/Repos/Notes/bib/")))

(use-package biblio)

(use-package biblio-openlibrary
  :vc (:url "https://github.com/fabcontigiani/biblio-openlibrary.git")
  :after biblio)

(use-package ebib-biblio
  :ensure nil
  :after (ebib biblio)
  :bind (:map ebib-index-mode-map
              ("B" . ebib-biblio-import-doi)
              :map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

;; Exportações com Pandoc
(use-package ox-pandoc)
