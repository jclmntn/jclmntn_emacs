;; Algumas dicas de hotkeys do emacs
;;;  M-x = ESC-x ou ALT-x
;;;  M-x revert-buffer dá reload no arquivo
;;;  M-x eval-buffer roda as execuções do arquivo

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

;; Configurações de Botões
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configurações de aparência/interface
;;; Menu
(scroll-bar-mode -1)        ; Desativa o scrollbar
(tool-bar-mode -1)          ; Desativa o toolbar
(tooltip-mode -1)           ; Desativa os tooltips
(menu-bar-mode -1)          ; Desativa o menu superior   
(set-fringe-mode '(20 . 0)) ; Adiciona uma margem do lado esquerdo

;;; Fonte
(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

;;; Tema pré-existente
(load-theme 'wombat)

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
   '(evil-collection evil general which-key rainbow-delimiters counsel compat doom-modeline ivy)))
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
             (setq which-key-idle-delay 0))

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
(general-define-key
  "C-e" 'counsel-switch-buffer)

(jclmntn/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
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

             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
             :after evil
             :config
             (evil-collection-init))


