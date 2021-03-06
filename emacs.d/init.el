(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(labburn))
 '(custom-safe-themes
   '("da53c5d117ebada2664048682a345935caf8e54094a58febd5f021462ef20ba2" "80ae3a89f1eca6fb94a525004f66b544e347c6f756aaafb728c7cdaef85ea1f5" default))
 '(delete-selection-mode t)
 '(display-line-numbers-type nil)
 '(fringe-mode nil nil (fringe))
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido))
 '(indicate-empty-lines t)
 '(olivetti-body-width 100)
 '(package-selected-packages
   '(markdown-mode olivetti reveal-in-osx-finder deft magit labburn-theme))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight normal :height 181 :width normal))))
 '(fringe ((t (:background "#3f3f3f" :foreground "dark cyan"))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face)))))

;; utf-8
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

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
      backup-directory-alist '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

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
;; (global-set-key (kbd "C-x C-g") 'deft-find-file)
(setq deft-default-extension "md")
(setq deft-new-file-format "%Y%m%d%H%M")

;; reveal-in-osx-finder
(global-set-key [f10] 'reveal-in-osx-finder)

;; Olivetti
(global-set-key (kbd "C-c o") 'olivetti-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

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

;; Notes

(defun yk-filter-query (str)
  "Ask and apply filter and show Deft."
  (interactive
   (list (read-from-minibuffer "Search: ")))
  (deft-filter str t)
  (deft))
;; (f)ilter) (q)uery
(global-set-key (kbd "C-, f q") 'yk-filter-query)

(defun yk-filter-at-point ()
  "Set Deft filter to tag at point and show Deft."
  (interactive)
  ;; A tag is most like a 'filename.
  (deft-filter (thing-at-point 'filename 'no-properties) t)
  (deft))
;; (f)ilter (p)oint
(global-set-key (kbd "C-, f p") 'yk-filter-at-point)

(defun yk-insert-date ()
  "Insert current date in ZK format."
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M")))
;; (i)nsert (d)ate
(global-set-key (kbd "C-, i d") 'yk-insert-date)

;; Blog
(load "~/.emacs.d/blog.el")

