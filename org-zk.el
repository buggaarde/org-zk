;;; org-zk --- Opinionated zettelkasten workflow in org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Simon Bugge Siggaard

;; Author: Simon Bugge Siggaard <simsig@gmail.com>
;; Maintainer: Simon Bugge Siggaard <simsig@gmail.com>
;; Created: 28 May 2020
;; Version: 0.1
;; Keywords: org mode zettelkasten
;; URL: https://github.com/buggaarde/org-zk
;; Package-Requires: ((ivy "0.13.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:


;;; Code:

;; Customizable

(defgroup org-zk nil
  "Zettelkasten in org-mode"
  :prefix "org-zk-"
  :link '(url-link :tag "github" "https://github.com/buggaarde/org-zk"))

(defcustom org-zk-directory (expand-filename "~/org-zk/")
  "All zettels are in this directory."
  :type 'directory
  :group 'org-zk)

(require 'org-zk-common)
(require 'org-zk-db)
(require 'org-zk-gather)
(require 'org-zk-notes)
(require 'ivy)

(defun org-zk--ivy-notes-list (str pred _)
  "Generate the ivy notes list."
  (mapcar (lambda (title-filename)
			(propertize (nth 0 title-filename)
						'file-name (nth 1 title-filename)))
		  (org-zk-db--all-notes-filenames)))

(defun org-zk--insert-link-in-file (filename path description)
  "Insert link to PATH with DESCRIPTION in FILENAME.

The link is inserted under the `References' headline by appending the link
to the headline content in the org-element AST."
  (with-temp-file filename
	(let* ((ast (org-zk--org-element-parse-file filename))
		   (references
			(org-element-map ast 'headline
			  (lambda (h)
				(when (string= (org-element-property :raw-value h) "References")
				  h))
			  nil t))
		   (paragraph (nth 2 (nth 2 references))))
	  (let ((el (or paragraph references)))
		(org-element-set-element
		   el (append el
					  `((link (:type "file" :path ,path :format bracket)
							  ,description) "\n")))
		(insert (org-element-interpret-data ast))))))

(defun org-zk--insert-backlink (&optional link-prefix backlink-prefix ivy-prompt-text)
  "Insert link to note, and also insert a backnote to the current note.

Optionally, specify a LINK-PREFIX, a BACKLINK-PREFIX and an IVY-PROMPT-TEXT."
  (let* ((path (file-name-nondirectory (buffer-file-name)))
		 (desc (concat backlink-prefix
					   (or (org-zk--title-of-note-in-current-buffer) path)))
		 (prompt (or ivy-prompt-text "Link with: ")))
	(ivy-read prompt #'org-zk--ivy-notes-list
			  :action (lambda (title)
						(let* ((full-path (get-text-property 0 'file-name title))
							   (file-name (file-name-nondirectory full-path)))
						  (org-zk--insert-link-in-file full-path path desc)
						  (insert (concat
								   "[[file:" file-name "][" link-prefix title "]]")))))))

(defun org-zk-insert-org-link ()
  "Prompt for a note and insert a link to that file."
  (interactive)
  (ivy-read "Insert link: " #'org-zk--ivy-notes-list
			:action (lambda (title)
					  (let ((file-name
							 (file-name-nondirectory (get-text-property 0 'file-name title))))
						(insert (concat "[[file:" file-name "][" title "]]"))))
			:caller 'org-zk-insert-org-link))

(defun org-zk-insert-backlink ()
  "Select note from list, and insert link/backlink to/from that note."
  (interactive)
  (org-zk--insert-backlink))

(defun org-zk-insert-folge-backlink ()
  "Select note from list, and insert link/backlink to/from that note.

Link descriptions are prefixed by `<:' and `>:' respectively"
  (interactive)
  (org-zk--insert-backlink "<:" ">:" "Follow note: "))


(provide 'org-zk)

;;; org-zk.el ends here
