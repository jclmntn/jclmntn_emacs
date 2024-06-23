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
;;      visibile-bell           1 ; Dica visual no limite do arquivo
      )

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
 '(package-selected-packages '(counsel compat doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :config
    (setq ivy-initial-inputs-alist nil)) ; Não inicia buscas com ^

;; Uma modeline atraente
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  )
