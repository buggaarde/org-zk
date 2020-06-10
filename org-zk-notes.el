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


(provide 'org-zk-notes)

;;; org-zk-notes.el ends here
