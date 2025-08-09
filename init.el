;; Startup Configs 

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
   (ring-bell-function #'ignore))
  :config
  ;; Inicia o Emacs com tela cheia
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Carrega arquivo de configurações
  (load custom-file)
  (dolist (mode '(eshell-mode-hook dired-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Ajustes de codificação
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default default-process-coding-system '(utf-8 . cp1252))

  ;; Configurações de cores no compile-mode
  (setq ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package ediff
  :custom
  ((ediff-split-window-function 'split-window-horizontally)
   (ediff-window-setup-function 'ediff-setup-windows-plain)))

;; Tooling
(add-to-list 'exec-path "C:/Users/jose/Portable/msys64/usr/bin/")
(add-to-list 'exec-path "C:/Users/jose/Portable/fd")
(add-to-list 'exec-path "C:/Users/jose/Portable/rg")
(add-to-list 'exec-path "~/Portable/Git/bin")
(add-to-list 'exec-path "~/Repos")
(setenv "PATH" (mapconcat #'identity exec-path path-separator))


;; Adiciona o MELPA à lista de pacotes possíveis
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Tema
(use-package modus-themes)
(load-theme 'modus-operandi)

;; Fontes
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

;; Remove elementos visuais
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode '(20 . 0))

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
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

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
  "oc"  '(org-capture :which-key)
  "oa"  '(org-agenda :which-key)
  "nn"  '(denote :which-key)
  "nr"  '(denote-rename-file :which-key)
  "nl"  '(denote-link :which-key)
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

(use-package vertico-grid)

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
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package wgrep
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode))

;; Elementos visuais do org-mode
(defun jclmntn/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
    (visual-line-mode))

;; Org Mode
(use-package org
  :hook (org-mode . jclmntn/org-mode-setup)
  :custom 
    ((org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)")))
    (org-todo-keyword-faces
	'(("TODO" . "#c3e88d")
	  ("NEXT" . "#c3e88d")
	  ("IDEA" . "LightBlue")
	  ("KILL" . "Red")))
    (org-capture-templates
	'(("i" "Idea" entry (file+olp "~/Repos/Notes/Tasks.org" "Caixa de Entrada") 
	   "* IDEA %?\n %U\n %a\n %i" :empty-lines 1)))
    (org-agenda-files '("~/Repos/Notes/Tasks.org"))
    (org-log-into-drawer t)
    (org-log-done 'time)
    (org-agenda-window-setup 'only-window)
    (org-agenda-restore-windows-after-quit t))
    (org-src-window-setup 'plain)
    (org-src-preserve-indentation t)
    (org-confirm-babel-evaluate nil)
    :config
    (jclmntn/org-font-setup)
    (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
        (python . t)
        (eshell . t)
        (R . t))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun jclmntn/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jclmntn/org-mode-visual-fill))


;; Personal Knowledge Management
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/Repos/Notes/denote-notes"))
  ;; Denote buffers automatically renamed to have prefix + title
  (denote-rename-buffer-mode 1))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/Repos/Notes/bib/references.bib" "~/Repos/Notes/bib/Paper2025a.bib"))
  (citar-open-always-create-notes nil)
  :hook (org-mode . citar-capf-setup))

(use-package citar-embark
 :ensure t
 :after citar embark
 :no-require
 :config (citar-embark-mode))

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


;; Yasnippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Repos/Notes/Snippets/"))
  (yas-global-mode 1))


;; Programming Modes
(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . company-mode)
	 (python-ts-mode . display-fill-column-indicator-mode))
  :mode (("\\.py\\'" . python-ts-mode)))


;; Para buildar o Blog
(use-package htmlize
  :ensure t)

;; Para trabalhar com o Xournal
(add-to-list 'load-path "~/.emacs.d/manual-packages/org-xournalpp")

(use-package org-xournalpp
  ;; Estou usando um fork: gitlab.com/vherrmann/org-xournalpp.
  ;; Parece atender as minhas necessidades até agora.
  ;; Eu só uso para criar links com arquivos .xopp, porque a visualização no arquivo está praticamente impossível.
  :ensure nil
  ;; :hook (org-mode . org-xournalpp-mode)
  :config
  (setq org-xournalpp-export-dir "~/repos/notes/figs/")
  (setq org-xournalpp-path-default "~/repos/notes/figs/")
  (setq org-xournalpp-executable "C:/Program Files/Xournal++/bin/xournalpp.exe"))
