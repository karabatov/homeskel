(require 'seq) ;; For seq-filter.
(require 'ox-publish) ;; For org-publish-project.

(defun blog-publish-blog (project-name)
  "Publishes PROJECT-NAME forcefully."
  (interactive "MProject name: ")
  (org-publish-project project-name t))

(defun blog-publish-list-files (directory publish-tag)
  "Returns a list of org files in DIRECTORY
that include PUBLISH-TAG in the `#+FILETAGS:` property."
  ;; Return a list of relative file names.
  (mapcar
   (lambda (file) (file-relative-name file directory))
   ;; Filter list to include only files with PUBLISH-TAG.
   (seq-filter
    (apply-partially #'blog-publish-tag-in-file? publish-tag)
    ;; List all org files in DIRECTORY.
    (directory-files directory t ".*\\.org$" t))))

(defun blog-publish-tag-in-file? (tag file)
  "Returns t if FILE contains TAG in the
#+FILETAGS property, otherwise nil."
  (member tag (blog-publish-get-tags file)))

(defun blog-publish-get-tags (file)
  "Extract the `#+FILETAGS:` from FILE as a list of strings."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+filetags:[ ]*:\\(.*\\):$" nil t)
          (split-string (match-string 1) ":")
	(if (search-forward-regexp "^\\#\\+filetags:[ ]*\\(.+\\)$" nil t)
            (split-string (match-string 1))
	  )))))

(setq blog-publish-base-directory "~/OneDrive/Writing/Org/")
(setq blog-publish-export-directory "~/emacs-publish/")
(setq org-publish-project-alist
      `(("blog-en"
	 :components ("blog-articles", "blog-pictures"))
	("blog-articles"
	 :base-directory ,blog-publish-base-directory
	 :exclude ".*"
	 :include ,(blog-publish-list-files
		   blog-publish-base-directory
		   "blog_en_publish")
	 :publishing-directory ,(concat blog-publish-export-directory "blog/en/")

	 ;; Formatting
	 :section-numbers nil ;; Don't put numbers on headings
	 :with-title nil ;; Don't put title in the output
	 :with-toc nil ;; Don't output TOC

	 ;; HTML
	 :publishing-function org-html-publish-to-html
	 :html-postamble ,(lambda (lang) "<p>§ org-publish-project</p>")

	 ;; Sitemap
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Yuri Karabatov"
	 :sitemap-sort-files anti-chronologically)
	("blog-pictures"
	 :base-directory ,(concat blog-publish-base-directory "Pictures/")
	 :base-extension ".*"
	 :publishing-directory ,(concat blog-publish-export-directory "blog/en/Pictures/")
	 :publishing-function org-publish-attachment
	 :recursive t)))
