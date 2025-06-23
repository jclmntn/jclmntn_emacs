(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Foi a única coisa que me permitiu buscar por símbolos usando o ripgrep
(setq-default default-process-coding-system '(utf-8 . cp1252))

;; Configurações de cores no emacs compile.
(setq ansi-color-for-compilation-mode t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Adicionando MSYS2 ao path
;; Útil quando eu não tiver acesso a variáveis de ambiente, mas posso
;; ajustar num script que executa o runemacs.exe também
(add-to-list 'exec-path "C:/msys64/usr/bin/")

;; Isso aqui é mais elegante do que fazer vários setenv seguidos.
(setenv "PATH" (mapconcat #'identity exec-path path-separator))

;; Configurações de início
(setq inhibit-startup-message t ; Tira a mensagem inicial
      ring-bell-function      'ignore ; Remove som no limite do arquivo
      package-check-signature nil
;;      visibile-bell           1 ; Dica visual no limite do arquivo
      )

;; Números nas linhas
(global-display-line-numbers-mode t)       ; Ativa globalmente
(setq display-line-numbers-type 'visual) ; Números são relativos
(dolist (mode '(term-mode-hook             ; Para cada um desses modos, desative numeração
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Maximizado
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Configurações de Botões
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configurações de aparência/interface
;;; Menu
(scroll-bar-mode -1)        ; Desativa o scrollbar
(tool-bar-mode -1)          ; Desativa o toolbar
(tooltip-mode -1)           ; Desativa os tooltips
(menu-bar-mode -1)          ; Desativa o menu superior   
(set-fringe-mode '(20 . 0)) ; Adiciona uma margem do lado esquerdo

;;; Fonte geral
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

;;;Fonte fixa
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)

;;;Fonte variável
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

;; Inicializando fontes de pacotes
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")    ; Essa variável que define em quais repositórios pacotes podem ser procurados
                         ;                         ;;("org" . "https://orgmode.org/elpa")         ; Abandonado
                         ("elpa" . "https://elpa.gnu.org/packages/"))) ; Não funciona por aqui
      

(package-initialize)             ; Inicializa pacotes
(unless package-archive-contents ; Verifica se existe um arquivo de pacotes se existir, ele não precisa recriar tudo
  (package-refresh-contents))

;;; Inicializando use-package em plataformas que não usam o kernel Linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package) ; Permite que um pacote seja usado e instalado, mas não quer dizer que o pacote funcione por padrão
(setq use-package-always-ensure t)

;; Instalando pacotes
;; (use-package command-log-mode) ; Se eu quiser ver um histórico de execuções

(use-package ivy            ; Cria um buffer para completion de comandos digitados
             :diminish               ; Esconde o modo de edição do modeline
             :bind (("C-s" . swiper) ; bind permite definir keybindings
                    :map ivy-minibuffer-map
                    ("TAB" . ivy-alt-done)
                    ("C-l" . ivy-alt-done)
                    ("C-j" . ivy-next-line)
                    ("C-k" . ivy-previous-line)
                    :map ivy-switch-buffer-map
                    ("C-k" . ivy-previous-line)
                    ("C-l" . ivy-done)
                    ("C-d" . ivy-switch-buffer-kill)
                    :map ivy-reverse-i-search-map
                    ("C-k" . ivy-previous-line)
                    ("C-d" . ivy-reverse-i-search-kill))
             :demand   ; Necessário para fazer com que o buffer do Ivy apareça
             :config
             (ivy-mode 1))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

; Buscas mais informativas
(use-package ivy-rich
             :init
             (ivy-rich-mode 1))


(use-package counsel
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Não inicia buscas com ^

;; Ícones par ao modeline
;; Rodar: M-x all-the-icons-install-fonts para instalar as fontes
(use-package all-the-icons)

;; Uma modeline atraente
(use-package doom-modeline
             :ensure t
             :init (doom-modeline-mode 1)
             )

;;; Um tema mais interessante
(use-package modus-themes
  :ensure
  :config (load-theme 'modus-operandi-tinted))

;; Facilita edição de elisp
(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

;; Whichkey para saber quais teclas apertar
(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.3))

;; General
;; Permite que keybindings sejam definidas de um jeito mais simples.
(use-package general
             :config
             ;; Para termos algo parecido com o leader do nvim
             (general-create-definer jclmntn/leader-keys
                                     :keymaps '(normal insert visual emacs)
                                     :prefix "SPC"
                                     :global-prefix "C-SPC"))

(jclmntn/leader-keys
  "o"	'(:ignore t :which-key "org-mode")
  "oa"  '(org-agenda :which-key "org-agenda")
  "oc"  '(org-capture :which-key "org-capture")
  ":"	'(counsel-M-x :which-key "M-x") 
  "."	'(projectile-find-file :which-key "find file") 
  "/"	'(projectile-ripgrep :which-key "live grep projects")
  "RET" '(counsel-bookmark :which-key "bookmarks")
  ;; Denote keybindings
  "n"   '(:ignore t :which-key "denote")
  "nn"  '(denote :which-key "create denote note")
  "nr"  '(denote-rename-file :which-key "rename file with denote")
  "nl"  '(denote-link :which-key "link to denote file")
  "nf"  '(consult-notes :which-key "find note")
  "nj"  '(denote-journal-new-or-existing-entry :which-key "create or open journal entry")
  "nk"  '(denote-journal-link-or-create-entry :which-key "link to journal entry")
  "nb"  '(denote-backlinks :which-key "find denote backlinks")
  "nw"  '(:ignore t :which-key "citar-denote")
  "nwc" '(citar-create-note :which-key "create bibliographic note")
  "nwx" '(citar-denote-nocite :which-key "find notes without citations")
  )

;; Vim Keybindings!!
; Não me vejo querendo que algum modo comece sem vim keybindings, mas se um dia acontecer...
; (defun jclmntn/evil-hook ()
;  (dolist (mode '(custom-mode
;                  eshell-mode
;                  git-rebase-mode
;                  erc-mode
;                  circe-server-mode
;                  circe-chat-mode
;                  circe-query-mode
;                  sauron-mode
;                  term-mode))
;    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
             :init

             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             (setq evil-want-C-u-scroll t)
             (setq evil-want-C-i-jump nil)
             ;:hook (evil-mode . jclmntn/evil-hook)
             :config
             (evil-mode 1)
             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

             ;; Usar movimentos visuais mesmo quando fora de buffers visuais
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

             ;(evil-set-initial-state 'org-agenda-mode 'normal)
             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
             :after evil
             :config
             (evil-collection-init))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))
  (global-undo-tree-mode 1))

;; Live fucking grep!!
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;:custom ((projectile-completion-system 'ivy))  ; Caso a integração com o Ivy não esteja funcionando
  :bind-keymap
  ("C-c p" . projectile-command-map) ; Todas as keybindings tem prefixo C-c p
  :init
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode)) 

;; Fucking magit!!
(use-package magit
  :custom
  (magit-display-buffer-action #'magit-display-buffer-same-window-except-diff-v1))

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
    (visual-line-mode)
    )

(use-package org
  :hook (org-mode . jclmntn/org-mode-setup)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-cite-global-bibliography '("~/Repos/Notes/bibs/lista_de_leitura.bib" "~/Repos/Notes/bibs/mba_literatura.bib"))
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/Repos/Notes/Tasks.org"))

  (setq org-todo-keywords
	'((sequence "TODO(t!)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)")))

  (setq org-refile-targets '((nil :maxlevel . 9)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keyword-faces
	'(("TODO" . "#c3e88d")
	  ("NEXT" . "#c3e88d")
	  ("IDEA" . "LightBlue")
	  ("KILL" . "Red")))

  (setq org-capture-templates
	'(("i" "Ideas")
	  ("ii" "Idea" entry (file+olp "~/Repos/Notes/Tasks.org" "Inbox")
	   "* IDEA %?\n %U\n %a\n %i" :empty-lines 1)

	  ("j" "Journal" entry
           (file denote-journal-path-to-new-or-existing-entry)
           "* %U %?\n%i\n%a"
          :kill-buffer t
	  :empty-lines 1)))

  (jclmntn/org-font-setup)
  (org-super-agenda-mode)

  ;; Configurando linguagem para o org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (eshell . t)
     (R . t)))

  ;; Ajustando buffers de detangle e preservando identação
  (setq org-src-window-setup 'plain)
  (setq org-src-preserve-indentation t)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Next"  ; A próxima coisa a se fazer
                :todo "NEXT")
         (:name "Today"  ; Coisas para fazer hoje
                :and (
                    :scheduled today
                    :date today
                    :not (:habit)))
         (:name "Habit"  ; Tarefas diárias/semanais para manutenção da vida
                :tag "habit"
                :habit t)
         (:name "Personal Sprint"  ; Tarefas relacionadas a estudos e leituras
                :tag "sprint")
	 )))


(defun jclmntn/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jclmntn/org-mode-visual-fill))

(setq org-confirm-babel-evaluate nil)

;; LSP - Python
;; Não esqueça de rodar interativamente o treesit-install-language-grammar 
(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . company-mode)
	 (python-ts-mode . display-fill-column-indicator-mode))
  :mode (("\\.py\\'" . python-ts-mode)))

;; Autocomplete no buffer
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

;; Configurando o ambiente para programar em clojure
(use-package cider
  :bind (("C-c u" . cider-user-ns)
	 ("C-M-r" . cider-refresh))
  :hook ((cider-repl-mode . company-mode)
	 (clojure-mode . company-mode)
	 (clojure-mode . display-fill-column-indicator-mode))
;;  :mode (("\\.clj\\'" . cider-mode))
  :config
  (setq cider-show-error-buffer t
	cider-auto-select-error-buffer t
	cider-repl-history-file "~/.emacs.d/cider-history"
	cider-repl-pop-to-buffer-on-connect t
	cider-repl-wrap-history t))

;; Ediff configuration
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Fuck TABS
(setq-default indent-tabs-mode nil)

;; Yasnippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Repos/Notes/Snippets/"))
  (yas-global-mode 1))

;; Manage backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))


;; Testes com org-noter
;; Desinstalei o pacote e reinstalei, agora tô com preguiça de arrumar
;; porque não compila sem ajustes no windows.
;; (use-package pdf-tools
;;   :config
;;   (pdf-tools-install))
;; (use-package org-noter)

;; Para otimizar busca de notas nas duas bases de notas
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-file-dir-sources '(("Roam"  ?r  "~/Repos/Notes/roam/")
                                         ("Denote" ?d "~/Repos/Notes/denote-notes/"))) ;; Set notes dir(s), see below
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

;; Testes com Denote
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/Repos/Notes/denote-notes"))
  ;; Denote buffers automatically renamed to have prefix + title
  (denote-rename-buffer-mode 1))


;; Configurando o Denote Journal
;; Não se esqueça de baixar uma cópia local do denote de lá do github
(add-to-list 'load-path "~/.emacs.d/manual-packages/denote-journal")

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

;; Testes com citar
(use-package citar
  :ensure t
  :defer t
  :custom
  ;; set bibliography's location
  (citar-bibliography '("~/Repos/Notes/bibs/lista_de_leitura.bib" "~/Repos/Notes/bibs/mba_literatura.bib"))
  ;; Allow multiple notes per bibliographic entry
  (citar-open-always-create-notes nil)
  :bind ("C-c w c" . citar-create-note))

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
;; Bind all available commands
;; :bind (("C-c w d" . citar-denote-dwim)
;;        ("C-c w e" . citar-denote-open-reference-entry)
;;        ("C-c w a" . citar-denote-add-citekey)
;;        ("C-c w k" . citar-denote-remove-citekey)
;;        ("C-c w r" . citar-denote-find-reference)
;;        ("C-c w l" . citar-denote-link-reference)
;;        ("C-c w f" . citar-denote-find-citation)
;;        ("C-c w x" . citar-denote-nocite)
;;        ("C-c w y" . citar-denote-cite-nocite)
;;        ("C-c w z" . citar-denote-nobib)))

;; Set search as ripgrep for many programs
;; Funciona com o citar-denote!!!
(setq xref-search-program 'ripgrep)

;; Configurando o r-ts-mode direto da fonte
(add-to-list 'load-path "~/.emacs.d/manual-packages/r-ts-mode/")

(use-package r-ts-mode
  :ensure nil 
  :hook ((r-ts-mode . eglot-ensure)
         (r-ts-mode . company-mode)
         (r-ts-mode . display-fill-column-indicator-mode))
  :mode (("\\.r\\'" . r-ts-mode))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 
    'eglot-server-programs 
    '(r-ts-mode . ("R" "--slave" "-e" "languageserver::run()"))))
  )

(use-package ess
   :hook ((ess-r-mode . r-ts-mode)))

(use-package htmlize
  :ensure t)


;; Testes com org-xournalapp
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
