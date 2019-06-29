(require 'seq) ;; For seq-filter.

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
