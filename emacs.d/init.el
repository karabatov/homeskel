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
 '(custom-enabled-themes (quote (labburn)))
 '(custom-safe-themes
   (quote
    ("80ae3a89f1eca6fb94a525004f66b544e347c6f756aaafb728c7cdaef85ea1f5" default)))
 '(display-line-numbers-type nil)
 '(fringe-mode nil nil (fringe))
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indicate-empty-lines t)
 '(package-selected-packages
   (quote
    (markdown-mode olivetti reveal-in-osx-finder deft magit labburn-theme)))
 '(show-paren-mode t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Hack"))))
 '(fringe ((t (:background "#3f3f3f" :foreground "dark cyan")))))

;; Sort apropos results by relevancy.
(setq apropos-sort-by-scores t)

;; A simpler binding for C-x o to switch between windows.
(global-set-key (kbd "M-o") 'other-window)

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
(setq deft-directory "~/OneDrive/Writing/Org")
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                              (nospace . "-")
                              (case-fn . downcase)))
(global-set-key (kbd "C-x C-g") 'deft-find-file)
(setq deft-default-extension "org")

;; reveal-in-osx-finder
(global-set-key [f10] 'reveal-in-osx-finder)

;; Olivetti
(global-set-key (kbd "C-c o") 'olivetti-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; org-mode
(global-set-key [f9] 'org-toggle-inline-images)
(setq org-agenda-files '("~/OneDrive/Writing/Org/"))

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

;; Blog
(load "~/.emacs.d/blog.el")

