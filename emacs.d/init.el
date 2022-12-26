;; -*- lexical-binding: t -*-

;; Set up package repos.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package for configuration management.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Theme
(use-package labburn-theme
  :ensure
  :config
  (load-theme 'labburn t)
  (set-face-attribute 'fringe t :background "#3f3f3f" :foreground "dark cyan"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-type nil)
 '(fringe-mode nil nil (fringe))
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido))
 '(indicate-empty-lines t)
 '(olivetti-body-width 100)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight normal :height 181 :width normal))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face)))))

(setq package-selected-packages
      '(zetteldeft
        markdown-mode
        olivetti
        reveal-in-osx-finder
        deft
        magit
        slime
        vertico
        consult
        avy
        which-key
        orderless))

;; When typing, replace the selected text.
(setq delete-selection-mode t)

;; utf-8
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(use-package avy
  :init
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  )

;; Always use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; Sort apropos results by relevancy.
(setq apropos-sort-by-scores t)

;; A simpler binding for C-x o to switch between windows.
(global-set-key (kbd "M-o") 'other-window)

;; windmove (S-dir to move between windows)
(windmove-default-keybindings)

;; Temporary save files
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '((".*" . "~/.emacs-saves/"))    ; don't litter my fs tree
      delete-old-versions t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs-saves/\\1" t)))
(make-directory "~/.emacs-saves/" t)


;; Shrink and enlarge windows
(global-set-key (kbd "C-S-<down>") 'enlarge-window)
(global-set-key (kbd "C-S-<up>") 'shrink-window)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)

;; Deft configuration
(setq deft-directory "~/Documents/notes")
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-extensions '("md"))
;; (global-set-key (kbd "C-x C-g") 'deft-find-file)
(setq deft-default-extension "md")
(setq deft-new-file-format "%Y%m%d%H%M")

;; Zetteldeft configuration
(zetteldeft-set-classic-keybindings)
(setq zetteldeft-id-format "%Y%m%d%H%M")
(setq zetteldeft-id-regex "[0-9]\\{12\\}")
(setq zetteldeft-link-indicator "§")
(setq zetteldeft-link-suffix "")
(setq zetteldeft-home-id "202201031224")
(setq zetteldeft-title-prefix "# ")
(setq zetteldeft-title-suffix "")
(font-lock-add-keywords 'markdown-mode
   `((,zetteldeft-id-regex
      . font-lock-warning-face)))
(global-set-key (kbd "C-c d w") 'zetteldeft-copy-id-current-file)

;; reveal-in-osx-finder
(global-set-key [f10] 'reveal-in-osx-finder)

;; Olivetti
(global-set-key (kbd "C-c o") 'olivetti-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(use-package which-key
  :init
  (which-key-mode))

;; SLIME
;; https://common-lisp.net/project/slime/doc/html/Installation.html#Installing-from-Git
; (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.26.1/")
; (require 'slime-autoloads)
(if (eq system-type 'windows-nt)
    (setq inferior-lisp-program "sbcl")
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))
(slime-setup '(slime-fancy))
(global-set-key (kbd "C-c s") 'slime-selector)

;; org-mode
;; (global-set-key [f9] 'org-toggle-inline-images)

;; Make a horizontal layout vertical
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; Highlight current line
(global-hl-line-mode t)

;; Hippie Expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Insert …
(defun insert-multiple-dots ()
  (interactive)
  (insert "…"))
(global-set-key (kbd "C-x 8 :") 'insert-multiple-dots)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Notes
(load (expand-file-name "notes.el" user-emacs-directory))

;; Blog
;; (load "~/.emacs.d/blog.el")

