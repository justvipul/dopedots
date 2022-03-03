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
;; (use-package pdf-tools
;;    :pin manual
;;    :config

;;    (setq-default pdf-view-display-size 'fit-height)
;;    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;    :custom
;;    (pdf-annot-activate-created-annotations t "automatically annotate highlights")package-always-ensure t)


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pdf-view-display-size 'fit-height)
 '(warning-suppress-types '((use-package))))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar
(winner-mode 1)             ; Disable the menu bar
(setq visible-bell nil)     ; Set up the visible bell
(column-number-mode)        ; Display Column Number in the modline

(setq scroll-conservatively 10)
(setq scroll-margin 3)
(use-package smooth-scrolling
	     :custom (smooth-scrolling-mode 1))

(use-package all-the-icons)
  (use-package doom-modeline
    :ensure t
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
  '(org-date ((t (:inherit fixed-pitch))))
  ;; '(ivy-posframe-border ((t (:background "#ffffff")))))
))
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(use-package olivetti
  :diminish
  :hook (text-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

(setq my/ui/monofont "VictorMono Nerd Font Oblique")
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

(use-package which-key)

(use-package general
  :after evil
  :defer t
  :preface

  (defun my/keybind/config ()
  (interactive)
  (counsel-find-file "emacs" "~/.config/"))

  :config
(general-create-definer my/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(my/leader-keys
"." '(counsel-find-file :which-key "find file")
"SPC" '(projectile-find-file :which-key "projectile find file")
"/" '(counsel-projectile-rg :which-key "projects")
"," '(counsel-rg :which-key "rg")
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
"b"  '(:ignore t :which-key "buffers")

"bs" '(save-buffer :which-key "Save Buffer")
"bk" '(kill-this-buffer :which-key "Kill Buffer")
"bl" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
"bi" '(ibuffer :which-key "Ibuffer")
"br" '(revert-buffer :which-key "Ibuffer")
"bb" '(switch-to-buffer :which-key "Ibuffer")

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

"q"  '(:ignore t :which-key "Quit and Stuff")
"qf" '(delete-frame :which-key "Close Frame")

))



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

;; (use-package general
;; :defer t
;; :preface
;; :config
;;   (general-create-definer first-leader-key
;;     :keymaps 'LaTeX-mode-map
;;     :prefix "C-,"
;;     :global-prefix "C-,")
  
;;   (first-leader-key
;;        "f"  '(LaTeX-fill-region :which-key "latex-fill-region")
;;        "SPC"  '(try/TeX-command-save-buffer-and-run-all :which-key "latex-save-run")
;;        "c"  '(try/ivy-bibtex-cite :which-key "ivy-cite")
;;        "i"   '(try/counsel-insert-file-path :which-key "insert-relative-filepath")
;;        "b" '(ivy-bibtex :which-key "ivy-bibtex")
;;        "t" '(tab-bar-switch-to-tab :which-key "tab-switch-name")
;;        "o"  '(outshine-imenu :which-key "menu")
;;        "t"  '(outshine-cycle-buffer :which-key "fold-buffer")
;; ))

  


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
    :ensure
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
   (prescient-persist-mode)
 )
;; (use-package selectrum-prescient)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
        (prog-mode . show-paren-mode)
        ;; (prog-mode . electric-pair-mode)
	)

 (use-package recentf
  :defer 10
  :config (recentf-mode  1))

(setq gc-cons-threshold 100000000)	     ;; 100 mb
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
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  ;(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil); anonying tabs
  (lsp-log-io nil) ; if set to true can cause a performance hit
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-headerline-breadcrumb-mode -1)
  (flycheck-mode 1)
  :bind
    (:map lsp-mode-map
          ;; ("<tab>" . company-indent-or-complete-common); commented cuz tabs for yasnippet!
    )
    )

(use-package lsp-ui
    :ensure
    :commands lsp-ui-mode
    :custom
    ;(lsp-ui-peek-always-show t)
    (lsp-ui-doc-mode t)
    ;(lsp-ui-sideline-show-hover t)
         (lsp-ui-doc-enable nil)
    :bind
        (:map lsp-ui-mode-map
        ("C-k" . lsp-ui-doc-focus-frame)
    :map lsp-ui-doc-frame-mode-map
        ("C-k" . lsp-ui-doc-unfocus-frame)
	))

(use-package flycheck :ensure)

(use-package origami
:hook (prog-mode . origami-mode))

;; (add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)


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
'("~/Documents/refs.bib"))
;;"~/Videos/nmims-assgmts/latex.study/bibliography-database.bib"))
  (bibtex-dialect 'biblatex)
  :mode
  ("\\.tex\\'" . latex-mode)
  :bind (:map LaTeX-mode-map
              ("TAB" . cdlatex-tab)
              ("'" . cdlatex-math-modify)
              ("C-c C-e" . cdlatex-environment))

  :hook
 (LaTeX-mode . olivetti-mode)
 (LaTeX-mode . TeX-PDF-mode)
 (LaTeX-mode . company-mode)
 (LaTeX-mode . flyspell-mode)
 (LaTeX-mode . xenops-mode)
 (LaTeX-mode . flycheck-mode)
 (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . try/latex-mode-setup)
  (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . lsp)

  :config
  ;; (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-command-default "Latex")
  (setq TeX-save-query nil)

  (setq reftex-plug-into-AUCTeX t)

  ;; ;; pdftools
  ;; ;; https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer#21764
  ;------------------------------------------------------------
  ;uncomment below two line for zathura as default reader in LaTex.
  ;------------------------------------------------------------
 (setq TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
 ;maybe synctex config
 (setq  TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

  ;------------------------------------------------------------
;; zathura and synctec stuff START
  ;------------------------------------------------------------

(with-eval-after-load "tex"
  ;; enable synctex support for latex-mode
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; add a new view program
  (add-to-list 'TeX-view-program-list
        '(;; arbitrary name for this view program
          "Zathura"
          (;; zathura command (may need an absolute path)
           "zathura"
           ;; %o expands to the name of the output file
           " %o"
           ;; insert page number if TeX-source-correlate-mode
           ;; is enabled
           (mode-io-correlate " --synctex-forward %n:0:%b"))))
  ;; use the view command named "Zathura" for pdf output
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("Zathura")))
  ;------------------------------------------------------------
;; zathura and synctec stuff STOP
  ;------------------------------------------------------------
 
  ;------------------------------------------------------------
  ;uncomment below two line for PDF tools as default reader in LaTex.
  ;------------------------------------------------------------
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

 
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

(defun try/TeX-command-save-buffer-and-run-all ()
    "Save the buffer and run TeX-command-run-all"
    (interactive)
    (let (TeX-save-query) (TeX-save-document (TeX-master-file)))
    (TeX-command-run-all nil))

;; copied ivy-bibtex and modified it to cite action
(defun try/ivy-bibtex-cite (&optional arg local-bib)
  "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`ivy-bibtex-with-local-bibliography'."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
          (key (bibtex-completion-key-at-point))
          (preselect (and key
                          (cl-position-if (lambda (cand)
                                            (member (cons "=key=" key)
                                                    (cdr cand)))
                                          candidates))))
    (ivy-read (format "Insert citation %s: " (if local-bib " (local)" ""))
              candidates
              :preselect preselect
              :caller 'ivy-bibtex
              :history 'ivy-bibtex-history
              :action 'ivy-bibtex-insert-citation)))

(defun try/latex-mode-setup ()
  (require 'company-reftex)
        (turn-on-reftex)
        (require 'company-auctex)
        (require 'company-math)
(setq-local company-backends
      
    (append '(
                              (company-reftex-labels
                                company-reftex-citations)
              (company-math-symbols-unicode company-math-symbols-latex company-latex-commands)
              (company-auctex-macros company-auctex-symbols company-auctex-environments)
              company-ispell
              )
            company-backends)))


(defun try/counsel-insert-file-path ()
  "Insert relative file path using counsel minibuffer"
  (interactive)
  (unless (featurep 'counsel) (require 'counsel))
  (ivy-read "Insert filename: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (insert (file-relative-name x)))))



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
                           ("begin" 7)
                           )
      )
(add-hook 'LaTeX-mode-hook #'(lambda ()
                               (outshine-mode 1)
                               (setq outline-level #'LaTeX-outline-level)
                               (setq outline-regexp (LaTeX-outline-regexp t))
                               (setq outline-heading-alist
                                     (mapcar (lambda (x)
                                               (cons (concat "\\" (nth 0 x)) (nth 1 x)))
                                             LaTeX-section-list))))

  )

    (general-define-key
      :states '(normal visual)
      :keymaps 'LaTeX-mode-map
      "TAB"  '(outshine-cycle :which-key "outshine-cycle")
  )

(use-package ivy-bibtex
  :defer t
  :custom
  (bibtex-completion-bibliography
   '("~/Documents/refs.bib"))
   (bibtex-completion-library-path '("~/papers"))
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-cite-default-as-initial-input t)
)

  ; "~/Videos/nmims-assgmts/latex.study/bibliography-database.bib"))
 ;



(global-set-key (kbd "C-RET") (kbd "C-e RET"))
(use-package beacon)
(  beacon-mode 1)

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
        evil-undo-system 'undo-redo
        )
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
  (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
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
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args))
  )

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))


(use-package evil-snipe
:config
(setq evil-snipe-repeat-scope 'whole-visible)
(evil-snipe-mode +1))

(add-hook 'pdf-view-mode-hook
          (lambda ()
        (set (make-local-variable 'evil-normal-state-cursor) (list nil))
        (internal-show-cursor nil nil))
        ) 

(setq
   split-width-threshold 0
   split-height-threshold nil)

(use-package pdf-tools)
(pdf-tools-install)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-date ((t (:inherit fixed-pitch)))))

