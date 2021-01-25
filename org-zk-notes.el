;;; org-zk-notes --- All functionality related to note creation and manipulation -*- lexical-binding: t; -*-
;; Copyright (c) 2020 Simon Bugge Siggaard

;; Author: Simon Bugge Siggaard <simsig@gmail.com>
;; Maintainer: Simon Bugge Siggaard <simsig@gmail.com>
;; Created: 31 May 2020
;; Version: 0.1
;; Keywords: some keywords here
;; URL: https://github.com/buggaarde/org-zk

;;; Commentary:

;;; Code:

(require 'ivy)

(defvar org-zk-directory)

(defun org-zk-new-note ()
  "Create a new zettel and open file in buffer."
  (interactive)
  (let ((fname
		 (concat org-zk-directory (format-time-string "%Y%m%d%H%M%S") ".org")))
	(find-file fname)
	(insert "#+title:\n#+startup: showall\n** Note\n\n\n** References\n\n** Sources\n\n")
	(goto-char (point-min))
	(let ((inhibit-message t))
	  (forward-line (1- 4)))))

(defun org-zk--all-notes-filenames ()
  "Return a list of tuples with (note-title note-filename) as contents.

This function has the same output structure as org-zk-db--all-notes-filenames."
  (let* ((filenames (seq-filter #'file-regular-p (directory-files org-zk-directory t)))
		 (titles (mapcar #'org-zk--title-of-note-in-file filenames))
		 (title-filename (cl-mapcar (lambda (t f) `(,t ,f)) titles filenames)))
	title-filename))

(defun org-zk--ivy-notes-list (str pred _)
  "Generate the ivy notes list."
  (mapcar (lambda (title-filename)
			(propertize (nth 0 title-filename)
						'file-name (nth 1 title-filename)))
		  (org-zk--all-notes-filenames)))

(defun org-zk-open-note ()
  "Open an existing zettel in current buffer."
  (interactive)
  (ivy-read "Open note: " #'org-zk--ivy-notes-list
			:action (lambda (title)
					  (let ((path (get-text-property 0 'file-name title)))
						(find-file path)))))

(provide 'org-zk-notes)

;;; org-zk-notes.el ends here
