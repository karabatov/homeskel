(require 'seq) ;; For seq-filter.
(require 'ox-publish) ;; For org-publish-project.

(defun blog-publish-blog (project-name)
  "Publishes PROJECT-NAME forcefully."
  (interactive "MProject name: ")
  (blog-publish-setq-project-alist)
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

(setf org-export-html-coding-system 'utf-8-unix)
(setf my-head-extra
      (concat
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "<link rel='stylesheet' href='./static/main.css' />"
       ))
(defun my-sitemap-entry (entry style project)
  "Custom formatting function for the sitemap entry.
Takes ENTRY name, sitemap STYLE and PROJECT."
  (format "[[file:%s][%s]] (%s)"
	  entry
	  (org-publish-find-title entry project)
	  (format-time-string "%d %B %Y" (org-publish-find-date entry project))))

(defun my-sitemap-function (title list)
  "Default site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
	  "* All posts\n"
	  (org-list-to-org list)))

(setq blog-publish-base-directory "~/OneDrive/Writing/Org/")
(setq blog-publish-export-directory "~/emacs-publish/")

(defun blog-publish-setq-project-alist ()
    "Reset 'org-publish-project-alist' to include new files."
  (setq org-publish-project-alist
	`(("blog-en"
	   :components ("blog-articles", "blog-pictures", "blog-static"))
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
	   :with-author t
	   :with-creator nil
	   :with-tags nil

	   ;; HTML
	   :publishing-function org-html-publish-to-html
	   :html-postamble ,(lambda (lang) "<p>∴</p>")
	   :html-link-home "/"
	   :html-head-include-default-style nil
	   :html-head-include-scripts nil
	   :html-head-extra ,my-head-extra
	   :html-home/up-format ""
	   :html-link-up ""
	   :html-metadata-timestamp-format "%d %B %Y"
	   :html-preamble ,(concat
			    "<header>"
			    "<img src=\"./static/sagrada.jpg\" />"
			    "<h2><a href=\"./index.html\">%a</a></h2>"
			    "<h3><time>%d</time></h3>"
			    "</header><br />"
			    "<p class=\"flag\">Observations, stories, projects, photos. 
In&nbsp;English and&nbsp;<a href=\"../ru/\">Russian</a>.</p>"
			    "<h5>Projects</h5>"
			    "<ul class=\"links\">"
			    "<li><a href=\"https://notsofastapp.com\">Not So Fast</a></li>"
			    "</ul>")

	   ;; Sitemap
	   :auto-sitemap t
	   :sitemap-style list
	   :sitemap-filename "index.org"
	   :sitemap-function my-sitemap-function
	   :sitemap-format-entry my-sitemap-entry
	   :sitemap-title "Yuri Karabatov"
	   :sitemap-sort-files anti-chronologically)
	  ("blog-pictures"
	   :base-directory ,(concat blog-publish-base-directory "Pictures/")
	   :base-extension ".*"
	   :publishing-directory ,(concat blog-publish-export-directory "blog/en/Pictures/")
	   :publishing-function org-publish-attachment
	   :recursive t)
	  ("blog-static"
	   :base-directory ,(concat blog-publish-base-directory "static/")
	   :base-extension ".*"
	   :publishing-directory ,(concat blog-publish-export-directory "blog/en/static/")
	   :publishing-function org-publish-attachment
	   :recursive t)))
  )
