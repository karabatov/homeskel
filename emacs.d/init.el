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
 '(package-selected-packages (quote (deft magit labburn-theme)))
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

;; Deft configuration
(setq deft-directory "~/Dropbox/Writing/Org")
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)
(global-set-key (kbd "C-x C-g") 'deft-find-file)
(setq deft-default-extension "org")

;; org-mode
(global-set-key [f9] 'org-toggle-inline-images)

;; English-language blog
(setq org-publish-project-alist
      `(("blog-en"
	 :components ("blog-articles", "blog-pictures"))
	("blog-articles"
	 :base-directory "~/Dropbox/Writing/Org/"
	 :base-extension "org"
	 :select-tags ("blog_en")
	 :publishing-directory "~/emacs-publish/blog-en/"
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Blog"
	 :sitemap-sort-files anti-chronologically)
	("blog-pictures"
	 :base-directory "~/Dropbox/Writing/Org/Pictures/"
	 :base-extension ".*"
	 :publishing-directory "~/emacs-publish/blog-en/Pictures/"
	 :publishing-function org-publish-attachment
	 :recursive t)))
