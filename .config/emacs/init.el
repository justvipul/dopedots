;; (fira-code-mode-set-font)
(setq gc-cons-threshold-original gc-cons-threshold) ;; unused
(setq gc-cons-threshold (* 1024 1024 100))
(setq read-process-output-max (* 1024 4024)) ;; 4mb
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq package-native-compile t)
(setq use-package-always-ensure t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(custom-set-variables '(warning-suppress-types '((use-package))))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)              ; Disable visible scrollbar
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(set-fringe-mode 10)              ; Give some breathing room

(menu-bar-mode -1)                ; Disable the menu bar
(winner-mode 1)                   ; Disable the menu bar
(setq visible-bell nil)           ; Set up the visible bell
(column-number-mode)              ; Display Column Number in the modline
(setq use-dialog-box nil)         ; I don't like to confirm anything with a mouse!
(defalias 'yes-or-no-p 'y-or-n-p) ; I don't want to type (yes) everytime!, 'y' is enough


(setq backup-directory-alist `(("." . "~/.local/share/emacs-backups")))
(setq make-backup-files t          ; backup of a file the first time it is saved.
    backup-by-copying t          ; don't clobber symlinks
    version-control t            ; version numbers for backup files
    vc-make-backup-files t       ; version control for git/vcs dirs
    delete-old-versions t        ; delete excess backup files silently
    delete-by-moving-to-trash t
    kept-old-versions 6          ; oldest versions to keep when a new numbered backup is made 
    kept-new-versions 9          ; newest versions to keep when a new numbered backup is made 
    auto-save-default t          ; auto-save every buffer that visits a file
    auto-save-timeout 20         ; number of seconds idle time before auto-save (default: 30)
    auto-save-interval 200       ; number of keystrokes between auto-saves (default: 300)
    create-lockfiles nil)         ; don't use lockfiles (default: t)
    
(setq scroll-conservatively 10)
(setq scroll-margin 3)
(use-package smooth-scrolling
  :custom (smooth-scrolling-mode 1))

(setq my/ui/monofont "VictorMono Nerd Font")
(setq my/ui/varfont "Noto Serif")

(defun my/ui/font-check ()
  "Do font check, then remove self from `focus-in-hook'; need to run this just once."
  (set-face-attribute 'default nil :font my/ui/monofont :height 130)
  (set-face-attribute 'fixed-pitch nil :font my/ui/monofont :height 130)
  (set-face-attribute 'variable-pitch nil :font my/ui/varfont :height 130)
  ;; Info has a horrible mono font
  (set-face-attribute 'Info-quoted nil :font my/ui/monofont :height 130)
  (remove-hook 'focus-in-hook #'my/ui/font-check))
(add-hook 'focus-in-hook #'my/ui/font-check)
;; (use-package all-the-icons)
(use-package doom-modeline
  ;; :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15) (doom-modeline-icon t)))

(use-package solaire-mode
  :init (solaire-global-mode +1))
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-flatwhite t)
  ;; (load-theme 'doom-one-light t)
  (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;; '(flycheck-warning ((t (:background "#282828" :underline "#fabd2f"))))
   '(org-date ((t (:inherit fixed-pitch)))))
  ;; '(ivy-posframe-border ((t (:background "#ffffff")))))



  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; for the first frame
(set-frame-parameter nil 'alpha-background 0.9)
;; for other frames
(add-hook 'server-after-make-frame-hook
      (lambda nil  (set-frame-parameter nil 'alpha-background 0.9)))

(use-package olivetti
  :diminish
  :hook (text-mode . olivetti-mode)
  :hook (prog-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 120))

(global-display-line-numbers-mode 1)
(defun disable-line-numbers () "Disables line number" (interactive) (display-line-numbers-mode 0))

(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                cargo-test-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode #'disable-line-numbers))  

(use-package undo-fu)
(setq org-roam-v2-ack t) ; anonying startup message
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "M-<f8>") '(lambda () (interactive) (org-agenda  nil "a")))
(global-set-key (kbd "<f8>"  ) '(lambda () (interactive) (org-agenda  nil "a")))
;; (global-set-key (kbd "M-<f6>") 'elfeed-dashboard)

(use-package general
  :after evil
  :defer t
  :preface

  (defun my/keybind/config ()
   (interactive)
   (counsel-find-file "emacs" "~/.config/"))

 (defun my/keybind/capture-inbox ()
   (interactive)
   (org-capture  nil "gi"))

 (defun my/counsel-insert-file-path ()
   "Insert relative file path using counsel minibuffer"
   (interactive)
   (unless (featurep 'counsel) (require 'counsel))
   (ivy-read "Insert filename: " 'read-file-name-internal
               :matcher #'counsel--find-file-matcher
               :action
               (lambda (x)
                (insert (file-relative-name x)))))

 :config
 (general-create-definer my/leader-keys
   :keymaps '(normal insert visual emacs)
   :prefix "SPC"
   :global-prefix "C-SPC")


 (my/leader-keys
  "." '(counsel-find-file :which-key "find file")
  "SPC" '(projectile-find-file :which-key "projectile find file")
  "/" '(counsel-projectile-rg :which-key "projects")
  ;; "," '(counsel-rg :which-key "rg")
  "u" '(universal-argument :which-key "universal arg")
  ";" '(counsel-M-x :which-key "M-x")
  ":" '(eval-expression :which-key "eval expression")

  "t"  '(:ignore t :which-key "toggles")

  "h"  '(:ignore t :which-key "Help")

  "ht" '(counsel-load-theme :which-key "Choose Theme")
  "hk" '(helpful-key :which-key "Describe Key")
  "hf" '(counsel-describe-function :which-key "Describe Function")
  "hv" '(counsel-describe-variable :which-key "Describe Variable")
  "hF" '(counsel-describe-face :which-key "Describe Face")
  "hi" '(info :which-key "info")

  "s"  '(:ignore t :which-key "Search")

  "sb" '(swiper :which-key "swiper")

  "f"  '(:ignore t :which-key "Files")

  "fr" '(counsel-recentf :which-key "Recent Files")
  "fp" '(my/keybind/config :which-key "Config")
  "fd" '(dired :which-key "dired prompt")
  "fD" '(dired-jump :which-key "dired current")

  "r"  '(:ignore t :which-key "Roam+Org")

  "ra"  '(org-agenda :which-key "Agenda")

  "rD" '(deft :which-key "Deft")
  "rf" '(org-roam-node-find :which-key "Find Note")
  "rl" '(org-roam-buffer-toggle :which-key "Toggle Sidebar")
  "rr" '(org-roam-db-sync :which-key "Roam Sync")
  "ri" '(org-roam-node-insert :which-key "Node  Insert")
  "rI" '(org-id-get-create :which-key "Org Id get/create")
  "rT" '(counsel-org-tag :which-key "Org Id get/create")


;; Dailies
  "rd"  '(:ignore t :which-key "Dailies")
  "rdT" '(org-roam-dailies-goto-today :which-key "Go To Today")
  "rdt" '(org-roam-dailies-capture-today :which-key "Capture Today")
  "rdY" '(org-roam-dailies-goto-yesterday :which-key "Go To yesterday")
  "rdy" '(org-roam-dailies-capture-yesterday :which-key "Capture yesterday")
  "rdM" '(org-roam-dailies-goto-tomorrow :which-key "Go To tomorrow")
  "rdm" '(org-roam-dailies-capture-tomorrow :which-key "Capture tomorrow")

;; Clocks
  "rc"  '(:ignore t :which-key "Clocks")
  "rci" '(org-clock-in :which-key "Clock In")
  "rcI" '(org-clock-in-last :which-key "Clock In Last")
  "rco" '(org-clock-out :which-key "Clock Out")
  "rcp" '(org-pomodoro :which-key "Pomodoro")
  "rcR" '(org-clock-report :which-key "Clock Report")
  "rcg" '(org-clock-goto :which-key "Goto Clock")

;; Anki
  ;; "rn"  '(:ignore t :which-key "AnKi")
  ;; "rnp" '(anki-editor-push-notes :which-key "Clock In")
  ;; "rni" '(anki-editor-insert-notes :which-key "Clock In")


;; Schedules and Deadlines
;; TODO!

  "C"  '(org-capture :which-key "Org-Capture")
  "I"  '(my/keybind/capture-inbox :which-key "Capture Inbox")

  "o"  '(:ignore t :which-key "Open")

  "oT" '(vterm :which-key "Vterm in current window")
;"ot" '(vterm-other-window :which-key "Vterm in other window")
  "ob" '(bookmark-jump :which-key "Bookmark Jump")
  "oB" '(bookmark-set :which-key "Bookmark set")
  "op" '(list-processes :which-key "List Proccess")

;; "om" '(mu4e :which-key "mu4e")
  ;; "ot" '(telega :which-key "Telega")

  ;; "oe" '(elfeed-dashboard :which-key "Elfeed Dashboard")

  "i"  '(:ignore t :which-key "Insert")
  "ie" '(emoji-insert :which-key "Emoji")
  "if" '(my/counsel-insert-file-path :which-key "Insert Relative path")

  "b"  '(:ignore t :which-key "buffers")

  "bs" '(save-buffer :which-key "Save Buffer")
  "bk" '(kill-current-buffer :which-key "Kill Buffer")
  "bl" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
  "bi" '(ibuffer :which-key "Ibuffer")
  "br" '(revert-buffer :which-key "Revert Buffer")
  "bb" '(switch-to-buffer :which-key "Switch to buffer")

  "w"  '(:ignore t :which-key "Windows")

  "wj" '(evil-window-down :which-key "Window Down")
  "wk" '(evil-window-up :which-key "Window Up")
  "wl" '(evil-window-right :which-key "Window Left")
  "wh" '(evil-window-left :which-key "Window Down")
  "wJ" '(evil-window-move-very-bottom :which-key "Move Window Down")
  "wK" '(evil-window-move-very-top :which-key "Move Window Up")
  "wL" '(evil-window-move-far-right :which-key "Move Window Left")
  "wH" '(evil-window-move-far-left :which-key "Move Window Down")

  "ws" '(evil-window-split :which-key "Window Split")
  "wv" '(evil-window-vsplit :which-key "Window Vsplit")
  "wd" '(evil-window-delete :which-key "Window delete")
  "wu" '(winner-undo :which-key "Window Undo")
  "wo" '(other-window :which-key "Window Other")
  "wr" '(winner-redo :which-key "Window Redo")
  "wt" '(treemacs :which-key "Treemacs")

  "c"  '(:ignore t :which-key "code")

  "cE" '(eval-defun :which-key "Eval Function at Point")
  "ce" '(eval-last-sexp :which-key "Eval Function")
  "cb" '(eval-buffer :which-key "Eval Buffer")
  "ca" '(lsp-execute-code-action :which-key "Code Action")
  "cl" '(lsp-avy-lens :which-key "Code Action")
  "ci" '(lsp-ui-imenu :which-key "lsp imenu")
  "cr" '(lsp-rename :which-key "rename")
  "cs" '(lsp-find-refernces :which-key "find refernces")
  "cd" '(lsp-find-definition :which-key "goto defintion")

  "g"  '(:ignore t :which-key "Git")
  "gg" '(magit-status :which-key "Magit")

  "p"  '(projectile-command-map t :which-key "Projectile")

  "q"  '(:ignore t :which-key "Quit and Stuff")
  "qf" '(delete-frame :which-key "Close Frame")))

(use-package general
  :config
  (general-evil-setup t))

  (general-define-key
    :states '(normal visual)
    :keymaps 'LaTeX-mode-map
    :prefix "C-,"
      "f"  '(LaTeX-fill-region :which-key "latex-fill-region")
      "SPC"  '(try/TeX-command-save-buffer-and-run-all :which-key "latex-save-run")
      "c"  '(try/ivy-bibtex-cite :which-key "ivy-cite")
      "i"   '(try/counsel-insert-file-path :which-key "insert-relative-filepath")
      "b" '(ivy-bibtex :which-key "ivy-bibtex")
      "t" '(tab-bar-switch-to-tab :which-key "tab-switch-name")
   "o"  '(outshine-imenu :which-key "menu")
   "t"  '(outshine-cycle-buffer :which-key "fold-buffer")
  )



(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn); TODO: fix
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-undo-system 'undo-redo)
        
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "\C-f" 'evil-forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-normal-state-map "\C-b" 'evil-backward-char)
  (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
  (define-key evil-visual-state-map "\C-b" 'evil-backward-char)

  (define-key evil-insert-state-map "\C-d" 'evil-delete-char)

  (define-key evil-normal-state-map "\C-i" 'evil-jump-forward)

  (define-key evil-normal-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-visual-state-map "\C-n" 'evil-next-line)
  (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
  ;; (define-key evil-normal-state-map "\C-w" 'evil-delete);; in custom
  (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word)
  (define-key evil-visual-state-map "\C-w" 'evil-delete-backward-word)
  (define-key evil-normal-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-visual-state-map "\C-y" 'yank)

  (define-key evil-normal-state-map "K" 'lsp-ui-doc-glance); TODO: all modes
  (define-key evil-visual-state-map "\C-y" 'yank)
                                        ;(define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
  (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
  ;; (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(defun +evil-default-cursor-fn (interactive)
  )
(defun +evil-emacs-cursor-fn () (interactive)
       (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))


(use-package evil-collection
  :after evil
  :custom
   (evil-collection-outline-bind-tab-p  t)
  :config
  (evil-collection-init))

(use-package key-chord
 :config
 (key-chord-define evil-insert-state-map "jk" 'evil-normal-state) 
 (key-chord-define evil-replace-state-map "jk" 'evil-normal-state) 
 :init
 (key-chord-mode 1))

   ;; (use-package evil-escape
   ;;   :after evil
   ;;   :init
   ;;   (setq  'evil-escape-excluded-major-modes '(magit-status-mode))
   ;;   (evil-escape-mode)
   ;;   :config
   ;;   (setq evil-escape-key-sequence "jk")
   ;;   (setq evil-escape-delay 0.2)
   ;;   (setq evil-escape-unordered-key-sequence t))
;; (defun my-jk ()
;;   (interactive)
;;   (let* ((initial-key ?j)
;;          (final-key ?k)
;;          (timeout 0.5)
;;          (event (read-event nil nil timeout)))
;;     (if event
;;         ;; timeout met
;;         (if (and (characterp event) (= event final-key))
;;             (evil-normal-state)
;;           (insert initial-key)
;;           (push event unread-command-events))
;;       ;; timeout exceeded
;;       (insert initial-key))))

;; (define-key evil-insert-state-map (kbd "j") 'my-jk)

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg))

  ;; bind evil-jump-out-args
  ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args))
  

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-org
  :hook (org-mode . evil-org-mode))

(use-package evil-snipe
 :config
 (setq evil-snipe-repeat-scope 'whole-visible)
 (evil-snipe-mode +1))

(use-package evil-numbers
 :config
  (evil-define-key '(normal visual) 'global (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental))
  

(use-package ivy
  :defer t
  :diminish
  :bind (("C-s" . swiper); TODO: move to Keybinds
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
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))

(use-package ivy-posframe
  :after ivy
  :diminish
  :custom-face
  (ivy-posframe-border ((t (:background "#ffffff"))))
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 120)
  (setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

  (ivy-posframe-mode +1))


(use-package counsel
  :defer t
  :bind (("M-x" . counsel-M-x)
         ;("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         ("C-w" . 'evil-delete-backward-word))
  :config (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with '^'

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1
                                             	which-key-max-display-columns 5)) 

(use-package company
  ;; :ensure
  :defer 5
  :diminish company-mode
  :custom
  (global-company-mode t)
  (company-idle-delay 0.3) ;; how long to wait until popup
  (company-minimum-prefix-length 1) ;; The minimum prefix length for idle completion.
  (company-selection-wrap-around t)
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-w". evil-delete-backward-word)
        ("<tab>" . company-complete-common-or-cycle)
        ("RET" . company-complete-selection)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))


;; (use-package company-lsp)
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(defun my-backends ()
    (set (make-local-variable 'company-backends)
        '((company-capf ;; I think this must come first?
            :with
            company-yasnippet
            company-files
            company-dabbrev-code))))

(use-package prescient
  :defer t
  :diminish
  :config (prescient-persist-mode 1))

(use-package ivy-prescient
  :after counsel
  :init (ivy-prescient-mode 1))

(use-package company-prescient
  :after company
  :config
   (company-prescient-mode 1)
   (prescient-persist-mode))
 
;; (use-package selectrum-prescient)
(use-package yasnippet
  :defer 4
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
 :after yasnippet)



(use-package helm :after org-books)

(setq org-directory "~/Documents/gtd/"
  org-roam-directory "~/Documents/roam"
  org-agenda-files (list org-directory)
  ;; elfeed-dashboard-file "~/Documents/private.el/elfeed-dashboard.org"
  org-preview-latex-image-directory  "~/.cache/ltx/ltximg"
  ;; org-my-anki-file (concat org-roam-directory "anki.org")
  org-refile-targets '((org-agenda-files . (:level . 1))))
  

(defun my/org-mode/org-mode-setup ()
 (interactive)
 (org-indent-mode)
 (variable-pitch-mode 0)
 (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . my/org-mode/org-mode-setup)
  (org-mode . my/org-mode/load-prettify-symbols); symbols
  (org-mode . auto-fill-mode)
  :config
  (require 'org-tempo)
  (require 'org-habit)
  (setq geiser-default-implementation  'guile)
  (setq org-ellipsis " ⤵")
  (setq org-agenda-start-with-log-mode t)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (dolist (face '((org-document-title . 2.0)
                  (org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font my/ui/varfont :weight 'regular :height (cdr face)))
    (set-face-attribute (car face) nil :font my/ui/varfont :weight 'regular :height (cdr face)))
;)


 (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
   ;; Ensure that anything that should be fixed-pitch in Org files appears that way
 (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-todo nil :background "#444527")
 (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package doct
  ;; :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . ?)
                  ("#+end_src" . ?)
                  ("#+begin_example" . ?)
                  ("#+end_example" . ?)
                  ("#+header:" . ?)
                  ("#+name:" . ?﮸)
                  ("#+title:" . "")
                  ("#+results:" . ?)
                  ("#+call:" . ?)
                  (":properties:" . ?)
                  (":logbook:" . ?))))
  (prettify-symbols-mode 1))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
    ;(restclient . t)
      (sql . t)
    ;(mermaid . t)
      (octave . t)
      (scheme . t)
      (shell . t)))

;; This is needed as of Org 9.2

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("re" . "src restclient"))
(add-to-list 'org-structure-template-alist '("sq" . "src sql"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("oc" . "src octave"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))

(use-package ob-mermaid
  :after org)

(setq org-capture-templates
 (doct `(("Consume: Read/watch" :keys "c"
          :file ,(concat org-directory "inbox.org")
          :prepend t
          :template ("* %{todo-state} %^{Description}"
                     ":PROPERTIES:"
                     ":Created: %U"
                     ":END:"
                     "%?")
          :children (("Read"   :keys "r"
                      :headline "Read"
                      :todo-state "TODO")
                     ("Watch" :keys "w"
                        :headline "Watch"
                        :todo-state "TODO")))
         ("Ideas" :keys "i"
          :file ,(concat org-directory "inbox.org")
          :prepend t
          :template ("* %{todo-state} %^{Description}"
                     ":PROPERTIES:"
                     ":Created: %U"
                     ":END:"
                     "%?")
          :children (("Project"   :keys "p"
                      :olp ("Ideas" "Project")
                      :todo-state "")
                     ("Blogs"   :keys "b"
                      :olp ("Ideas" "Blog")
                      :todo-state "")
                     ("placeholder" :keys "w"
                        :headline "Watch"
                        :todo-state "TODO")))
         ("GTD" :keys "g"
          :file ,(concat org-directory "inbox.org")
          :prepend t
          :template ("* %{todo-state} %^{Description}"
                     ":PROPERTIES:"
                     ":Created: %U"
                     ":END:"
                     "%?")
          :children (("Inbox"   :keys "i"
                      :headline "Inbox"
                      :todo-state "")
                     ("placeholder" :keys "w"
                        :headline "Watch"
                        :todo-state "TODO"))))))

(defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    ;(make-frame '((name . "org-capture") (window-system . x))); window-system breaks for some reason :(
    (make-frame '((name . "org-capture")))
    (select-frame-by-name "org-capture")
    (counsel-org-capture)
    (delete-other-windows))

(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "|" "CANCELED(c)")))

(setq org-agenda-start-on-weekday 0 ;0 is sunday
      org-agenda-weekend-days '(5 6))

(eval-after-load 'org-agenda
 '(progn
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "<RET>") 'org-agenda-goto
      ;;;; (kbd "\t") 'org-agenda-goto

      "q" 'org-agenda-quit
      "S" 'org-save-all-org-buffers

      ;;;; Clocking
      "c" nil
      "ci" 'org-agenda-clock-in
      "co" 'org-agenda-clock-out
      "cx" 'org-agenda-clock-cancel
      "cR" 'org-agenda-clockreport-mode

      ;;;; Properties
      "s" 'org-agenda-schedule
      "d" 'org-agenda-deadline
      "p" 'org-agenda-priority
      "t" 'org-agenda-todo
      "T" 'counsel-org-tag
      ":" 'org-agenda-set-tags
      "e" 'org-agenda-set-effort

      ;;;; Movement
      "j"  'org-agenda-next-line
      "k"  'org-agenda-previous-line
      "f" 'org-agenda-later
      "b" 'org-agenda-earlier
      "J" 'org-agenda-next-date-line
      "K" 'org-agenda-previous-date-line
      "." 'org-agenda-goto-today

      ;;;; View toggles
      "vt" 'org-agenda-toggle-time-grid
      "vw" 'org-agenda-week-view
      "vd" 'org-agenda-day-view
      "vl" 'org-agenda-log-mode
      "vr" 'org-agenda-redo
      "F" 'org-agenda-follow-mode

      ;;;; Other
      "C" 'org-capture
      "R" 'org-agenda-refile
      "A" 'org-agenda-archive
      "g/" 'org-agenda-filter-by-tag

      ;;;; cool but inactive
      ;; "gj" 'org-agenda-goto-date
      ;; "gJ" 'org-agenda-clock-goto
      ;; "gm" 'org-agenda-bulk-mark
      ;; "go" 'org-agenda-open-link
      ;; "+" 'org-agenda-priority-up
      ;; "-" 'org-agenda-priority-down
      ;; "y" 'org-agenda-todo-yesterday
      ;; "n" 'org-agenda-add-note
      ;; ";" 'org-timer-set-timer
      ;; "I" 'helm-org-task-file-headings
      ;; "i" 'org-agenda-clock-in-avy
      ;; "O" 'org-agenda-clock-out-avy
      ;; "u" 'org-agenda-bulk-unmark
      ;; "x" 'org-agenda-exit
      ;; "va" 'org-agenda-archives-mode
      ;;"vc" 'org-agenda-show-clocking-issues
      ;; "o" 'delete-other-windows
      ;; "gh" 'org-agenda-holiday
      ;; "gv" 'org-agenda-view-mode-dispatch
      "n" nil)))  ; evil-search-next
      ;; "{" 'org-agenda-manipulate-query-add-re
      ;; "}" 'org-agenda-manipulate-query-subtract-re
      ;; "0" 'evil-digit-argument-or-evil-beginning-of-line
      ;; "<" 'org-agenda-filter-by-category
      ;; ">" 'org-agenda-date-prompt
      ;; "H" 'org-agenda-holidays
      ;; "L" 'org-agenda-recenter
      ;; "Z" 'org-agenda-sunrise-sunset
      ;; "T" 'org-agenda-show-tags
      ;; "X" 'org-agenda-clock-cancel
      ;; "[" 'org-agenda-manipulate-query-add
      ;; "g\\" 'org-agenda-filter-by-tag-refine
      ;; "]" 'org-agenda-manipulate-query-subtract

;; TODO check this

(setq org-habit-graph-column 80   ; prevent overwriting title
      org-habit-show-all-today t) ; show even if DONE

(use-package org-pomodoro
 :defer t
 :custom
 (org-pomodoro-length 25)
 (org-pomodoro-keep-killed-pomodoro-time t)
 (org-pomodoro-manual-break t))

(use-package org-roam
  :defer t
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum) ;; preformance
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?" :if-new
       (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :unnarrowed t)))
  :config
  ;; side window
  ;(require 'org-roam-protocol)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(use-package websocket
    :after org-roam)

(use-package simple-httpd
    :after org-roam)

(use-package org-roam-ui
    :straight (org-roam-ui
               :type git
               :host github
               :repo "org-roam/org-roam-ui"
               :files ("*.el" "out"))
    :after org-roam ;; or :after org
    :hook (org-roam . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t)
    )


(use-package deft
  :after org
  :bind
  :custom
  (deft-strip-summary-regexp "\\`\\(.+\n\\)+\n")
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))
(setq deft-recursive t)
(setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
(setq deft-use-filename-as-title 't)



(use-package org-download
    :after org)
(use-package org-cliplink
  :after org)

(use-package org-books
 :after org
 :config 
 (setq org-books-file "~/Documents/books/list.org"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
        (prog-mode . show-paren-mode))
        ;; (prog-mode . electric-pair-mode)
  

;; (use-package paredit :defer t)

;; (use-package parinfer-rust-mode
;;     :defer 4
;;     :hook emacs-lisp-mode scheme-mode clojure-mode
;;     :init
;;     (setq parinfer-rust-auto-download t))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package recentf
  :defer 10
  :config (recentf-mode  1))

(setq gc-cons-threshold 100000000)           ;; 100 mb
(setq read-process-output-max (* 1024 4024)) ;; 4mb

(use-package lsp-mode
  :commands (lsp lsp-deferred)
 ;;  :hook
 ;; (lsp-mode . my/lsp/lsp-mode-setup)
  :custom
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-eldoc-enable-hover nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-idle-delay 0.6)
  (lsp-completion-provider :none) 
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  ;(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil); anonying tabs
  (lsp-log-io nil) ; if set to true can cause a performance hit
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-headerline-breadcrumb-mode -1)
  (flycheck-mode 1)
  :bind
  (:map lsp-mode-map))
        ;; ("<tab>" . company-indent-or-complete-common); commented cuz tabs for yasnippet!
    
    
(use-package lsp-ui
    ;; :ensure
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-doc-mode t)
    (lsp-ui-sideline-show-hover t)
    ;; (lsp-ui-doc-enable nil)
    :bind
    (:map lsp-ui-mode-map
     ("C-c z" . lsp-ui-doc-focus-frame)
     :map lsp-ui-doc-frame-mode-map
     ("C-g" . lsp-ui-doc-unfocus-frame)))
  

;; (use-package lsp-treemacs
;;   :after lsp)

(use-package flycheck :ensure
 :custom-face (flycheck-warning ((t (:underline (:color "#fabd2f" :style line :position line)))))
             (flycheck-error ((t (:underline (:color "#fb4934" :style line :position line)))))
             (flycheck-info ((t (:underline (:color "#83a598" :style line :position line))))))

(use-package origami
  :hook (prog-mode . origami-mode))

(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc))

(use-package treemacs-evil
 ;:when (package-installed-p 'evil-collection)
 ;:defer t
  :after treemacs
  :init
  :config
 (general-def evil-treemacs-state-map
   [return] #'treemacs-RET-action
   [tab]    #'treemacs-TAB-action
   "TAB"    #'treemacs-TAB-action
   "o v"    #'treemacs-visit-node-horizontal-split
   "o s"    #'treemacs-visit-node-vertical-split))


(use-package lsp-treemacs
    :after (treemacs lsp))
(use-package treemacs-magit
    :after treemacs magit)
(use-package treemacs-persp
    :after treemacs
    :config (treemacs-set-scope-type 'Perspectives))

(use-package prettier
 :defer t)
  

(eval-after-load 'markdown-mode
 '(custom-set-faces
   '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
   '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
   '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
   '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
   '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.1))))
   '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0))))))

;; (add-hook 'markdown-mode-hook 'my/org-mode/org-mode-visual-fill)
;; (add-hook 'markdown-mode-hook 'outline-minor-mode)

;; latexmk
(use-package auctex-latexmk
 :defer t)
;; company
(use-package company-math
    :after company)
(use-package company-auctex
    :after company)
(use-package company-reftex
    :after company)


;;  use cdlatex
(use-package cdlatex
 :defer t)

;; https://gist.github.com/saevarb/367d3266b3f302ecc896
;; https://piotr.is/2010/emacs-as-the-ultimate-latex-editor/

(use-package latex
  :straight auctex
  :defer t
  :custom
  (olivetti-body-width 120)
  (cdlatex-simplify-sub-super-scripts nil)
  (reftex-default-bibliography
   '("~/Documents/refs.bib")
   '("~/Documents/library.bib"))
  (bibtex-dialect 'biblatex)
  :mode
  ("\\.tex\\'" . latex-mode)
  :bind (:map LaTeX-mode-map
              ("TAB" . cdlatex-tab)
              ("'" . cdlatex-math-modify)
              ("C-c C-e" . cdlatex-environment))

  :hook
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . flycheck-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . try/latex-mode-setup)
  (LaTeX-mode . turn-on-cdlatex)
      ;; (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . lsp)
      ;; (LaTeX-mode . olivetti-mode);; already set as a text-mode-hook
      ;; (LaTeX-mode . TeX-PDF-mode) ;; what does it do?
      ;; (LaTeX-mode . company-mode) ;; already enabled globaly
      ;; (LaTeX-mode . xenops-mode)  ;; svgs too lagy :(
      ;; (LaTeX-mode . flycheck-mode);; already enabled with lsp
      ;; (LaTeX-mode . LaTeX-math-mode)
  :config
  ;; (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-command-default "LatexMK")
  (setq TeX-save-query nil)

  ;; this is becuase i set $out_dir = '/tmp/tex' in `.latexmkrc`
  ;; and I want to enable forward synctex. don't use it if you don't do like me...
  ;; (setq-default TeX-output-dir "/tmp/tex")

  ;; (setq reftex-plug-into-AUCTeX t)

  ;; ;; pdftools
  ;; ;; https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer#21764
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
        ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))

;; clean intermdiate tex crap
 (add-to-list 'LaTeX-clean-intermediate-suffixes '"-figure[0-9]*\\.\\(pdf\\|md5\\|log\\|dpth\\|dep\\|run\\.xml\\)")
 (add-to-list 'LaTeX-clean-intermediate-suffixes '".auxlock")


   ;; to have the buffer refresh after compilation,
   ;; very important so that PDFView refesh itself after comilation
   ;; (add-hook 'TeX-after-compilation-finished-functions
   ;;           #'TeX-revert-document-buffer)

   ;; latexmk
 (require 'auctex-latexmk)
 (auctex-latexmk-setup)
 (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(defun try/latex-mode-setup ()
  (require 'company-reftex)
  (turn-on-reftex)
  (require 'company-auctex)
  (require 'company-math)
 (setq-local company-backends
      
     (append '((company-reftex-labels company-reftex-citations)
               (company-math-symbols-unicode company-math-symbols-latex company-latex-commands)
               (company-auctex-macros company-auctex-symbols company-auctex-environments)
               company-ispell)
             company-backends)))

(use-package outshine
  :defer t
  :config
 (setq LaTeX-section-list '(
                            ("part" 0)
                            ("chapter" 1)
                            ("section" 2)
                            ("subsection" 3)
                            ("subsubsection" 4)
                            ("paragraph" 5)
                            ("subparagraph" 6)
                            ("begin" 7)))
                           
      
 (add-hook 'LaTeX-mode-hook #'(lambda ()
                                (outshine-mode 1)
                                (setq outline-level #'LaTeX-outline-level)
                                (setq outline-regexp (LaTeX-outline-regexp t))
                                (setq outline-heading-alist
                                      (mapcar (lambda (x)
                                                (cons (concat "\\" (nth 0 x)) (nth 1 x)))
                                              LaTeX-section-list)))))

  

(general-define-key
  :states '(normal visual)
  :keymaps 'LaTeX-mode-map
  "TAB"  '(outshine-cycle :which-key "outshine-cycle"))
  

(use-package ivy-bibtex
  :defer t
  :custom
  (bibtex-completion-bibliography
   '("~/Documents/refs.bib")
   '("~/Documents/library.bib"))
  (bibtex-completion-library-path '("~/papers"))
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-cite-default-as-initial-input t))


(setq verilog-linter "verilator --lint-only")
(use-package restart-emacs)
(unless (server-running-p) (server-start))
;; (add-hook 'server-after-make-frame-hook '(lambda () (set-cursor-color "#FFFFFF")))
(use-package vterm
    :commands vterm)
    ;; :ensure t)

(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   ;; (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?*  . ,(regexp-opt '("*>" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
(use-package keyfreq
 :defer 10
 :custom
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

;; (use-package elfeed-org
;;   :commands elfeed
;;   :config (elfeed-org))


;; (use-package elfeed-dashboard
;;   :commands elfeed-dashboard
;;   :config
;;   ;; (setq elfeed-dashboard-file "~/Documents/private.el/elfeed-dashboard.org")
;;   ;; update feed counts on elfeed-quit
;;   (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)
;;   (evil-set-initial-state 'elfeed-dashboard-mode 'emacs)
;;  :hook (elfeed-dashboard-mode . (lambda () (variable-pitch-mode -1))))


;; (defun my/elfeed/visit-entry-dwim (&optional arg)
;;  (interactive "P")
;;  (if arg
;;      (elfeed-search-browse-url)
;;    (-let [entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))]
;;      (if (s-matches? (rx "https://www.youtube.com/watch" (1+ any))
;;                      (elfeed-entry-link entry))
;;          (let* ((quality (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720" "1080")))
;;                 (format (if (= 0 (string-to-number quality)) "" (format "--ytdl-format=[height<=?%s]" quality))))
;;            (message "Opening %s with height ≤ %s with mpv..."
;;                     (elfeed-entry-link entry) quality)
;;            (elfeed-untag entry 'unread)
;;            (start-process "elfeed-mpv" nil "mpv" format (elfeed-entry-link entry))
;;            (elfeed-search-update :force))
;;        (if (eq major-mode 'elfeed-search-mode)
;;            (elfeed-search-browse-url)
;;          (elfeed-show-visit))))))

;; (defun my/elfeed/toggle-search-tag (tag)
;;   (interactive)
;;    ;example: tag = "unread"
;;   (elfeed-search-set-filter
;;    ;s-contains matches agains "+unread"; i.e (concat "+" tag) => "+unread"
;;    (if (s-contains? (concat "+" tag) elfeed-search-filter)
;;    ;regex will be " ?\\+unread"
;;        (s-replace-regexp (concat " ?\\+" tag) "" elfeed-search-filter)
;;    ;concat will be " +unread"
;;        (concat elfeed-search-filter (concat " +" tag)))))

;; (defun my/elfeed/toggle-search-unread () (interactive) (my/elfeed/toggle-search-tag  "unread"))
;; (defun my/elfeed/toggle-search-to_read () (interactive) (my/elfeed/toggle-search-tag "to_read"))

;; (use-package elfeed
;;     :defer t
;;     :config 
;;     ;; (defun my/elfeed/visual ()
;;     ;; (interactive)
;;     ;;     (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family my/ui/varfont :size 13))
;;     ;;   (setq visual-fill-column-width 110
;;     ;;         visual-fill-column-center-text t
;;     ;;         fill-column 90)
;;     ;;   (visual-fill-column-mode 1)
;;     ;;   (visual-line-mode 1))

;;     (evil-define-key 'normal elfeed-search-mode-map
;;          "O" 'my/elfeed/visit-entry-dwim
;;          ;"tr" 'my/elfeed/toggle-read
;;          "tr" 'my/elfeed/toggle-search-unread
;;          "tt" 'my/elfeed/toggle-search-to_read)
;;   :custom
;;   (elfeed-db-directory  "~/.local/share/elfeed")
;;   :hook ;(elfeed-show-mode  . my/elfeed/visual)
;;   (elfeed-show-mode  . olivetti-mode))


;; (use-package circe
;;   :defer t
;;   :preface
;;   (defun my/circe/clear ()
;;     (interactive)
;;     (circe-command-CLEAR))
;;   :config
;;   (add-to-list 'circe-networks `("flinner's znc" :host "flinner.my.to" :port 6697
;;                                  :tls t
;;                                  ;; :sasl-strict t 
;;                                  :nick ,my/secret/znc/flinner.my.to/username
;;                                  ;; :sasl-username ,my/secret/znc/flinner.my.to/sassl-username
;;                                  ;; :sasl-password ,my/secret/znc/flinner.my.to/secret
;;                                  :pass ,my/secret/znc/flinner.my.to/secret
;;                                  :user ,my/secret/znc/flinner.my.to/username))
                                 
;;   (setq circe-color-nicks-min-constrast-ratio 4.5
;;         circe-color-nicks-everywhere t)
;;   :hook (circe-channel-mode . enable-circe-color-nicks)
;;   :custom
;;   (circe-format-say "{nick:-16s} {body}"))
  ;; :bind(("C-l" . my/circe/clear))
  
;; (defun circe-command-ZNC (what)
;;   "Send a message to ZNC incorporated by user '*status'."
;;   (circe-command-MSG "*status" what))

;; assumed Maildir layout
;; ~/Maildir/Account0/{Inbox,Sent,Trash}
;; ~/Maildir/Account1/{Inbox,Sent,Trash}
;; where Account0 is context name
;; (defun my-make-mu4e-context (context-name full-name mail-address signature)
;;   "Return a mu4e context named CONTEXT-NAME with :match-func matching
;; folder name CONTEXT-NAME in Maildir. The context's `user-mail-address',
;; `user-full-name' and `mu4e-compose-signature' is set to MAIL-ADDRESS
;; FULL-NAME and SIGNATURE respectively.
;; Special folders are set to context specific folders."
;;   (let ((dir-name (concat "/" context-name))
;;         (context-filter (concat " maildir:/" context-name "/")))
;;     (make-mu4e-context
;;      :name context-name
;;      ;; we match based on the maildir of the message
;;      ;; this matches maildir /Arkham and its sub-directories
;;      :match-func
;;      `(lambda (msg)
;;         (when msg
;;           (string-match-p
;;          ,(concat "^" dir-name)
;;          (mu4e-message-field msg :maildir))))
;;      :vars
;;      `(
;;         (mu4e-bookmarks .
;;           ,`(
;;           (:name "All Unread messages"  :query ,"flag:unread AND NOT flag:trashed" :key ?a)
;;           (:name "Unread messages"  :query ,(concat "flag:unread AND NOT flag:trashed"  context-filter) :key ?u)
;;           (:name "Today's messages"     :query ,(concat "date:today..now" context-filter)                   :key ?t)
;;           (:name "Last 7 days"          :query ,(concat "date:7d..now" context-filter) :hide-unread t       :key ?w)
;;           (:name "Messages with images" :query ,(concat "mime:image/*" context-filter)                      :key ?p)))

;;        (user-mail-address    .   ,mail-address)
;;        ;; (mu4e-maildir         .   ,(concat "~/.mail" dir-name))
;;        (user-full-name       .   ,full-name)
;;        (mu4e-sent-folder     .   ,(concat dir-name "/Sent"))
;;        (mu4e-drafts-folder   .   ,(concat dir-name "/Drafts"))
;;        (mu4e-trash-folder    .   ,(concat dir-name "/Trash"))
;;        (mu4e-refile-folder   .   ,(concat dir-name "/Archive"))
;;        (mu4e-compose-signature . ,signature)))))
;; ;;Fixing duplicate UID errors when using mbsync and mu4e

;; (use-package mu4e
;;   ;; :ensure-system-package mu
;;   :config
;;   ;; (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-view-action))
;;   (add-hook 'mu4e-view-mode-hook #'visual-line-mode) 
;;   (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
;;   :custom
;;   (mu4e-change-filenames-when-moving t)
;;   (mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
;;   (mu4e-attachment-dir "~/Downloads")
;;   (mu4e-compose-signature-auto-include nil)
;;   (mu4e-get-mail-command "mbsync -a")

;;   (mu4e-update-interval 300)
;;   (mu4e-use-fancy-chars t)
;;   (mu4e-view-show-addresses t)
;;   (mu4e-view-show-images t))


;; (setq sendmail-program "/usr/bin/msmtp"
;;       message-sendmail-f-is-evil t
;;       message-sendmail-extra-arguments '("--read-envelope-from")
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'message-send-mail-with-sendmail)
;; ;; This is a sample, it doesn't get included in init.el
;; ;; I put mine at location specified in the  next code block
;; ;; any number of email can be used ofc
;; (setq mu4e-contexts `(
;;   ,(my-make-mu4e-context
;;           "maildir-context" "Full Name"
;;           "Email Address" "Signature")
;;   ,(my-make-mu4e-context
;;           "maildir-context2" "Full Name2"
;;           "Email Address2" "Signature2")
;;   ))

;; (eval-after-load 'mu4e
;;   (load "~/Documents/passwords/mu4e-context.el"))

;; (setf (alist-get 'trash mu4e-marks)
;;       (list :char '("d" . "▼")
;;             :prompt "dtrash"
;;             :dyn-target (lambda (target msg)
;;                           (mu4e-get-trash-folder msg))
;;             :action (lambda (docid msg target)
;;                       ;; Here's the main difference to the regular trash mark,
;;                       ;; no +T before -N so the message is not marked as
;;                       ;; IMAP-deleted:
;;                       (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))


;; (defvaralias 'mu4e~context-current  'mu4e--context-current)

;; (use-package telega
;;  :defer t
;;  :init
;;     (defun my/telega/olivetti () (setq-local olivetti-body-width 80))
;;     (defun my/telega/company-backends ()
;;         (setq-local company-backends
;;             (append '(telega-company-username telega-company-botcmd)
;;                     company-backends)))
;;  :hook (telega-chat-mode . olivetti-mode)
;;       (telega-chat-mode . my/telega/olivetti)
;;       (telega-chat-mode . my/telega/company-backends)
;; ;; installed telegram-tdlib from AUR
;;  :config
;;  (telega-mode-line-mode)
;;  (telega-notifications-mode)
;;  :custom (telega-server-libs-prefix "/usr")
;;         (telega-chat-bidi-display-reordering 'right-to-left)
;;         (telega-emoji-use-images nil))

(use-package beacon)
(  beacon-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(use-package dashboard
  :after solaire-mode
  :init
  (dashboard-setup-startup-hook)
  :config
  ;; (setq dashboard-startup-banner "~/Downloads/haskell-rec.png")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which
  ;;   displays whatever image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("pdf" . "zathura")
                                ("mkv" . "mpv")))) 
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)) 
(use-package tiny) 
(require 'tiny)
(tiny-setup-default)
