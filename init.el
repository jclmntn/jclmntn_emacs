;; ;; -*- lexical-binding: t; -*-
;; ;; Startup Configs 

(use-package emacs
  :custom
  ((custom-file "~/.emacs.d/emacs-custom.el")               ;; Define a localização do arquivo com opções customizadas
   (backup-directory-alist '(("." . "~/.emacs.d/backup/"))) ;; Define a localização dos backups
   (global-display-line-numbers-mode t)                     ;; Ativa a numeração global
   (display-line-numbers-type 'visual)                      ;; Ativa a numeração relativa
   (indent-tabs-mode nil)                                   ;; Good riddance, Tabs
   (tab-width 4) ;;Tab = 4 espaços
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
;; Tornando marcas grandes novamente
(defun jclmntn/my-bind-layout-marks (alist evil-fn prefix)
  (dolist (item alist)
    (let* ((key (car item))
           (val (cdr item))
           (sym-name (format "my-layout-%s-%c" prefix val))
           (sym (intern sym-name)))
      (defalias sym `(lambda ()
                      (interactive)
                      (funcall ',evil-fn ,val)))
      (define-key evil-normal-state-map (kbd key) sym))))

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
  (setq alist-mark-set '(("À" . ?A) ("È" . ?E) ("Ì" . ?I) ("Ò" . ?O) ("Ù" . ?U) ("à" . ?a)
                          ("è" . ?e) ("ò" . ?o) ("ù" . ?u) ("Ǘ" . ?V) ("ǘ" . ?v) ("?" . ?N)
                          ("?" . ?n) ("?" . ?W) ("?" . ?w) ("?" . ?Y) ("?" . ?y)))
  (setq alist-line-set '(("Á" . ?A) ("É" . ?E) ("Í" . ?I) ("Ó" . ?O) ("Ú" . ?U) ("Ý" . ?y)
                          ("á" . ?a) ("é" . ?e) ("í" . ?i) ("ó" . ?o) ("ú" . ?u) ("ý" . ?y)
                          ("N" . ?N) ("n" . ?n) ("Ǘ" . ?V) ("ǘ" . ?v) ("Ẃ" . ?W) ("ẃ" . ?w)))
  (setq alist-reg-set '(("Ä" . ?A) ("Ë" . ?E) ("Ï" . ?O) ("Ö" . ?U) ("Ü" . ?U) ("ä" . ?a)
                         ("ë" . ?e) ("ï" . ?i) ("ö" . ?o) ("ü" . ?u) ("ÿ" . ?y) ("Ÿ" . ?Y)
                         ("Ḧ" . ?H) ("ḧ" . ?h) ("Ẅ" . ?W) ("ẅ" . ?w) ("ẗ" . ?t)))
  (jclmntn/my-bind-layout-marks alist-mark-set #'evil-goto-mark "mark")
  (jclmntn/my-bind-layout-marks alist-line-set #'evil-goto-line "line")
  (jclmntn/my-bind-layout-marks alist-reg-set #'evil-use-register "reg"))

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
  "nsl" '(denote-sequence-link :which-key)
  "nsf" '(denote-sequence-find :which-key)
  "nss" '(denote-sequence :which-key)
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
  (consult-notes-denote-mode)
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
    '((sequence "TODO(t!)" "PROJ(j)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)")))
   (org-todo-keyword-faces
    '(
      ("PROJ" . "Blue")
      ("TODO" . "#c3e88d")
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
   (org-refile-targets '(("~/Repos/Notes/Tasks.org" :maxlevel . 3)))
   (org-imenu-depth 3))
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
    200.0)) 

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

(use-package denote-sequence
  :ensure t)

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
  :bind (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
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

;; Pra forçar a codificação do comando git.
;; Engraçado que isso tenha dado certo, significa que por alguma razão o git tem outra codificação mesmo no WSL.
(setq-default process-coding-system-alist (cons '("git" . (utf-8 . utf-8)) process-coding-system-alist))

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

;; Spellchecking
;; Precisa do Enchant-2 instalado. Se eu estiver usando o aspell como dicionário, lembrar de instalar o enchant2-aspell.
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

;; EJC-SQL
(defun jclmntn/ejc-maybe-add-limit (args)
  "Inject LIMIT 200 into SELECT queries that do not already specify a limit."
  (let* ((sql (car args))
         (upper (when sql (upcase (string-trim sql)))))
    (if (and upper
             (or (string-match-p "\\`SELECT\\b" upper)
                 (string-match-p "\\`WITH\\b" upper))
             (not (string-match-p "\\bLIMIT\\b" upper)))
        (cons (concat (string-trim-right sql) "\nLIMIT 200") (cdr args))
      args)))

(defun jclmntn/ejc-sql-connected-hook ()
  (ejc-set-fetch-size 99)         ; Limit for the number of records to output.
  (ejc-set-max-rows 99)           ; Limit for the number of records in ResultSet.
  (ejc-set-show-too-many-rows-message t) ; Set output 'Too many rows' message.
  (ejc-set-column-width-limit nil) ; Limit for outputing the number of chars per column.
  (ejc-set-use-unicode t)         ; Use unicode symbols for grid borders.
  )

(use-package ejc-sql
  :ensure t
  :hook (ejc-sql-connected . jclmntn/ejc-sql-connected-hook)
  :custom
  (ejc-nrepl-timeout nil)
  (ejc-show-result-bottom t)
  :config
  (setq nrepl-sync-request-timeout nil)
  (require 'ejc-completion-common)
  (advice-add 'ejc-eval-user-sql :filter-args #'jclmntn/ejc-maybe-add-limit)
  (ejc-create-connection
   "BigQuery"
   :dependencies [[com.simba.googlebigquery/googlebigquery-jdbc42 "1.6.3.1004"]
                  [com.google.cloud/google-cloud-bigquerystorage "3.9.3"]
                  [com.google.apis/google-api-services-bigquery "v2-rev20240919-2.0.0"]
                  [com.google.auth/google-auth-library-oauth2-http "1.28.0"]]
   :classname "com.simba.googlebigquery.jdbc.Driver"
   :connection-uri (concat "jdbc:bigquery://https://www.googleapis.com/bigquery/v2:443"
                           ";ProjectId=azos-data-analytics"
                           ";OAuthType=0"
                           ";OAuthServiceAcctEmail=azos-feature-store@azos-data-analytics.iam.gserviceaccount.com"
                           ";OAuthPvtKeyPath=" (expand-file-name "~/.config/gcloud/feature-store.json")))

    (let* ((auth (car (auth-source-search :host "postgres-dis")))
         (user (plist-get auth :user))
         (subname (plist-get auth :subname))
         (password (let ((secret (plist-get auth :secret)))
                     (if (functionp secret) (funcall secret) secret))))
    (ejc-create-connection
     "PostgreSQLDis"
     :dependencies [[org.postgresql/postgresql "42.7.3"]]
     :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.7.3/"
                        "postgresql-42.7.3.jar")
     :subprotocol "postgresql"
     :subname subname
     :user user
     :password password)))

;; ECA emacs
(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest))
