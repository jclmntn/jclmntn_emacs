;; Startup Configs 

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
   (ring-bell-function #'ignore))
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
  (setq ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package ediff
  :custom
  ((ediff-split-window-function 'split-window-horizontally)
   (ediff-window-setup-function 'ediff-setup-windows-plain)))

;; Adiciona o MELPA à lista de pacotes possíveis
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)

;; Tema
(use-package modus-themes)
(load-theme 'modus-operandi)

;; Fontes
(set-face-attribute 'default nil :font "IosevkaTerm" :height 140)
(set-face-attribute 'fixed-pitch nil :font "IosevkaTerm" :height 140)
(set-face-attribute 'variable-pitch nil :font "IosevkaTerm" :height 140 :weight 'regular)

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
  "i"   '(consult-imenu :whick-key)
  "oc"  '(org-capture :which-key)
  "oa"  '(org-agenda :which-key)
  "nn"  '(denote :which-key)
  "nr"  '(denote-rename-file :which-key)
  "nl"  '(denote-link :which-key)
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
         ("M-." . embark-dwim)
         ("C-." . embark-act)
         ("C-h b" . embark-bindings))
  :custom
  ((prefix-help-command #'embark-prefix-help-command))
  :config
  (keymap-set jinx-repeat-map "RET" 'jinx-correct)
  (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
  (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
  (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
  (add-to-list 'embark-repeat-actions #'jinx-next)
  (add-to-list 'embark-repeat-actions #'jinx-previous)
  (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target))
  (add-to-list 'embark-keymap-alist '(hledger-mode hledger-mode-map)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  ((corfu-cycle t)
  (corfu-auto t))
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t)

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
    (set-face-attribute (car face) nil :font "Noto Sans Mono" :weight 'regular :height (cdr face)))

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
        '("TODO(t!)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)"))
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
    (org-agenda-restore-windows-after-quit t)
    (org-src-window-setup 'plain)
    (org-src-preserve-indentation t)
    (org-confirm-babel-evaluate nil)
    (org-imenu-depth 3))
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
  :config
  (setq denote-directory (expand-file-name "~/Repos/Notes/denote-notes"))
  ;; Denote buffers automatically renamed to have prefix + title
  (denote-rename-buffer-mode 1))

(use-package denote-sequence
  :ensure t)

(use-package citar
  :ensure t
  :custom
  ((citar-bibliography (file-expand-wildcards "~/Repos/Notes/bib/*.bib"))
   (citar-open-always-create-notes nil)
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar))
  :hook (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

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
  ((citar-denote-file-type 'org)
   (citar-denote-keyword "bib")
   (citar-denote-subdir "~/Repos/Notes/denote-notes/bib/")
   (citar-denote-template nil)
   (citar-denote-title-format "author-year-title")
   (citar-denote-title-format-andstr "and")
   (citar-denote-title-format-authors 1))
  :init
  (citar-denote-mode))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-action #'magit-display-buffer-same-window-except-diff-v1))

;; Pra forçar a codificação do comando git.
;; Engraçado que isso tenha dado certo, significa que por alguma razão o git tem outra codificação mesmo no WSL.
(setq-default process-coding-system-alist (cons '("git" . (utf-8 . utf-8)) process-coding-system-alist))


;; Yasnippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Repos/Notes/Snippets/"))
  (yas-global-mode 1))


;; Programming Modes
(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 ;; (python-ts-mode . company-mode)
	 (python-ts-mode . display-fill-column-indicator-mode))
  :mode (("\\.py\\'" . python-ts-mode)))


;; Para buildar o Blog
(use-package htmlize
  :ensure t)

;; Para trabalhar com o Xournal
;; (add-to-list 'load-path "~/.emacs.d/manual-packages/org-xournalpp")

;; (use-package org-xournalpp
;;   ;; Estou usando um fork: gitlab.com/vherrmann/org-xournalpp.
;;   ;; Parece atender as minhas necessidades até agora.
;;   ;; Eu só uso para criar links com arquivos .xopp, porque a visualização no arquivo está praticamente impossível.
;;   :ensure nil
;;   ;; :hook (org-mode . org-xournalpp-mode)
;;   :config
;;   (setq org-xournalpp-export-dir "~/repos/notes/figs/")
;;   (setq org-xournalpp-path-default "~/repos/notes/figs/")
;;   (setq org-xournalpp-executable "C:/Program Files/Xournal++/bin/xournalpp.exe"))


(add-to-list 'load-path "~/.emacs.d/manual-packages/atomic-chrome/")

(use-package atomic-chrome
  :ensure nil
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


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

;; Exportações com Pandoc
(use-package ox-pandoc)


;; Racket Mode
(use-package racket-mode
  ;; :custom (racket-program "C:/Program Files/Racket/Racket.exe")
  :mode (("\\.scm\\'" . racket-mode))
  :config
  (add-to-list 'display-buffer-alist
               '(
                 "\\*Racket REPL"
                 (display-buffer-reuse-mode-window
                  display-buffer-below-selected)
                 (window-height . 10)
                 (dedicated . t))))

;; Experimentando com hledger
(defun org-babel-execute:hledger (body params)
  "Execute a block of hleder code with org-babel."
  (let ((in-file (org-babel-temp-file "n" ".journal")))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (format "hledger bs" (org-babel-process-file-name in-file)) "")))


;; Hledger Mode
(defun my-short-hledger-amount ()
  "Target an amount at point of the form hledger-amount-value-regex"
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (str (buffer-substring-no-properties line-start line-end))
           (match (string-match (concat hledger-currency-string " ?" hledger-amount-value-regex) str))
            ;; Embark is expecting the starting and ending positions of the match in the buffer. 
           (buffer-match-start (+ line-start (match-beginning 0)))
           (buffer-match-end (+ line-start (match-end 0))))
      (save-match-data
        (when (and (derived-mode-p 'hledger-mode) match)
          `(hledger-amount
            ,(format "%s" (match-string 0 str))
            ,buffer-match-start . ,buffer-match-end))))))


(defun hledger-completion-accounts ()
  (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                          (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (point) hledger-accounts-cache)))

(defun jclmntn/hledger-imenu ()
    (setq-local imenu-create-index-function #'imenu-default-create-index-function)
    (setq-local imenu-generic-expression '(("Dates" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" 1))))

(use-package hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :custom
  ((hledger-jfile "~/Repos/hlfinances/2026.journal") ;; Sempre atualizar para o mais recente.
   (hledger-currency-string "R$")
   (hledger-year-of-birth 1995)
   (hledger-reporting-day 1)
   (hledger-ratios-liquid-asset-accounts "assets:bank assets:wallet assets:caixinha")
   (hledger-ratios-essential-expense-accounts "expenses:food expenses:groceries expenses:energy expenses:water expenses:streaming expenses:internet expenses:cellphone expenses:telephone"))
  :hook
  ((hledger-mode . (lambda ()
                     (add-hook 'completion-at-point-functions 'hledger-completion-accounts)))
   (hledger-mode . jclmntn/hledger-imenu))
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'my-short-hledger-amount)
    (add-to-list 'embark-keymap-alist '(hledger-amount hledger-amount-keymap))
  (defvar-keymap hledger-amount-keymap
    :doc "Keymap for 'hledger-amount'"
    :parent embark-general-map
    "RET" #'hledger-edit-amount)))
 
;; (defun my-imenu-sort-by-date (a b)
;;   "Sort Imenu items A and B by the date found in their names."
;;   (let* ((name-a (car a))
;;          (name-b (car b))
;;          ;; Extract date strings (adjust the regex if your format is different)
;;          (date-str-a (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" name-a)
;;                        (match-string 1 name-a)))
;;          (date-str-b (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" name-b)
;;                        (match-string 1 name-b)))
;;          ;; Convert to Emacs time objects, fallback to epoch if missing
;;          (time-a (if date-str-a (date-to-time date-str-a) '(0 0 0 0)))
;;          (time-b (if date-str-b (date-to-time date-str-b) '(0 0 0 0))))
;;     ;; Compare times (change to time-less-p for chronological order)
;;     (time-less-p time-b time-a)))


(use-package just-mode)
