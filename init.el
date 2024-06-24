;;; Commentary: Algumas dicas de hotkeys do emacs
;;; M-x = ESC-x ou ALT-x
;;; M-x revert-buffer dá reload no arquivo
;;; M-x eval-buffer roda as execuções do arquivo

;; Algumas notas sobre o elisp
;;;  'value é um símbolo
;;;  "value" é uma string

;; Outros pontos sobre o emacs em geral
;;;  O emacs tem um gerenciador de pacotes: M-x list-packages

;; Configurações de início
(setq inhibit-startup-message t ; Tira a mensagem inicial
      ring-bell-function      'ignore ; Remove som no limite do arquivo
      package-check-signature nil
;;      visibile-bell           1 ; Dica visual no limite do arquivo
      )

;; Números nas linhas
(global-display-line-numbers-mode t)       ; Ativa globalmente
(setq display-line-numbers-type 'relative) ; Números são relativos
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

;;; Tema pré-existente
;(load-theme 'wombat)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" default))
 '(package-selected-packages
   '(visual-fill-column org-bullets evil-magit magit counsel-projectile projectile undo-tree evil-collection evil general which-key rainbow-delimiters counsel compat doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Buscas mais informativas
(use-package ivy-rich
             :init
             (ivy-rich-mode 1))


(use-package counsel
             :bind (("M-x" . counsel-M-x)
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
(use-package doom-themes
             :ensure t
             :config (setq doom-themes-enable-bold t
                           doom-themes-enable-italic t)
             (load-theme 'doom-palenight t)
             )

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

; Em teoria, ficaria como o Harpoon, que abriria uma lista de buffers para navegarmos.
;(general-define-key
;  "C-e" 'counsel-switch-buffer)

(jclmntn/leader-keys
  "o"  '(:ignore t :which-key "org-mode")
  "oa"  '(org-agenda :which-key "org-agenda")
  "oc"  '(org-capture :which-key "org-capture")
  ":"  '(counsel-M-x :which-key "M-x") 
  "."  '(projectile-find-file :which-key "find file") 
  "/"  '(projectile-ripgrep :which-key "live grep projects") 
  )

;; Vim Keybindings!!
; Não me vejo querendo que algum buffer comece sem vim keybindings, mas se um dia acontecer...
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

             (evil-set-initial-state 'org-agenda-mode 'normal)
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
  :config (counsel-projectile-mode)) ; M-o permite utilizar opções adicionais, inclusive usar o rg em um diretório inteiro.

;; Fucking magit!!
(use-package magit
  :custom
  (magit-display-buffer-action #'magit-display-buffer-same-window-except-diff-v1))

;(use-package evil-magit ;; O pacote agora é parte do evil collection!
;  :after magit)

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
  (setq org-hide-emphasis-markers t
	org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/Repos/Notes/Tasks.org"))
  (setq org-cite-global-bibliography '("~/Repos/Notes/lista_de_leitura.bib"))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)")))

  (setq org-refile-targets '((("Archive.org" :maxlevel . 1)
			      ("Tasks.org" :maxlevel . 1)
			     )))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keyword-faces
	'(("TODO" . "#c3e88d")
	  ("NEXT" . "#c3e88d")
	  ("IDEA" . "LightBlue")))

  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates
	'(("t" "Ideas")
	  ("tt" "Idea" entry (file+olp "~/Repos/Notes/Tasks.org" "Inbox")
	   "* IDEA %?\n %U\n %a\n %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/Repos/Notes/Journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	  :clock-in :clock-resume
	  :empty-lines 1)))

  (jclmntn/org-font-setup)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 90
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;; Mais aparências do visual
;(add-hook
; 'text-mode-hook
; 'auto-fill-mode)
;(add-hook
; 'text-mode-hook
; 'olivetti-mode)
