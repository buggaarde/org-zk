;;; org-zk --- Opinionated zettelkasten workflow in org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Simon Bugge Siggaard

;; Author: Simon Bugge Siggaard <simsig@gmail.com>
;; Maintainer: Simon Bugge Siggaard <simsig@gmail.com>
;; Created: 28 May 2020
;; Version: 0.1
;; Keywords: org mode zettelkasten
;; URL: https://github.com/buggaarde/org-zk
;; Package-Requires: ((emacs "25") (ivy "0.13.0") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

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

;; Customizables

(defgroup org-zk nil
  "Zettelkasten in org-mode"
  :prefix "org-zk-"
  :link '(url-link :tag "github" "https://github.com/buggaarde/org-zk"))

(eval-when-compile
  (defcustom org-zk-directory (expand-file-name "~/org-zk/")
	"All zettels are in this directory."
	:type 'directory
	:group 'org-zk)

  (defcustom org-zk-link-type-prefix-alist
	'(("<:" . :folge-prev) (">:" . :folge-next))
	"The link description prefix associated with each link type."
	:type '(alist :key-type string :value-type keyword)
	:group 'org-zk))

(require 'org-zk-common)
(require 'org-zk-db)
(require 'org-zk-gather)
(require 'ivy)

;; Opening and creating notes

(defun org-zk--new-note (title)
  "Create a new zettel and open file in buffer.
Returns the full path to the newly created file.

TITLE will be the title of the note."
  (let ((fname
		 (concat org-zk-directory (format-time-string "%Y%m%d%H%M%S") ".org")))
	(find-file fname)
	(insert
	 (format
	  "#+title: %s\n#+startup: showall\n** Note\n\n\n** References\n\n** Sources\n\n"
	  title))
	(goto-char (point-min))
	(let ((inhibit-message t))
	  (forward-line (1- 4)))
	fname))

(defun org-zk-new-note ()
  "Create a new zettel and open file in buffer."
  (interactive)
  (org-zk--new-note ""))

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

;; Links between notes

(defun org-zk--insert-link-in-ast (ast path description)
  "Provided an `org-mode' AST, insert link to PATH with DESCRIPTION.
Return the modified AST.

The link is inserted under the `References' headline by appending
the link to the headline content in the org-element AST."
  (let* ((references
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
		ast)))

(defun org-zk--insert-link-in-buffer (filename path description)
  "Insert link to PATH with DESCRIPTION in buffer containing contents from FILENAME.

The link is inserted under the `References' headline by appending
the link to the headline content in the org-element AST."
  (with-current-buffer (find-buffer-visiting filename)
	(let ((ast (org-zk--insert-link-in-ast
				(org-element-parse-buffer) path description))
		  (tmp-buffer (generate-new-buffer " *org-zk-tmp-buffer*")))
	  (save-excursion
		(with-current-buffer tmp-buffer
		  (insert (org-element-interpret-data ast)))
		(replace-buffer-contents tmp-buffer)
		(kill-buffer tmp-buffer)))))


(defun org-zk--insert-link-in-file (filename path description)
  "Insert link to PATH with DESCRIPTION in FILENAME.

The link is inserted under the `References' headline by appending the link
to the headline content in the org-element AST."
  (with-temp-file filename
	(let ((ast (org-zk--insert-link-in-ast
				(org-zk--org-element-parse-file filename) path description)))
	  (insert (org-element-interpret-data ast)))))

;; (defun org-zk--insert-link-in-file (filename path description)
;;   "Insert link to PATH with DESCRIPTION in FILENAME.

;; The link is inserted under the `References' headline by appending the link
;; to the headline content in the org-element AST."
;;   (with-temp-file filename
;; 	(let* ((ast (org-zk--org-element-parse-file filename))
;; 		   (references
;; 			(org-element-map ast 'headline
;; 			  (lambda (h)
;; 				(when (string= (org-element-property :raw-value h) "References")
;; 				  h))
;; 			  nil t))
;; 		   (paragraph (nth 2 (nth 2 references))))
;; 	  (let ((el (or paragraph references)))
;; 		(org-element-set-element
;; 		   el (append el
;; 					  `((link (:type "file" :path ,path :format bracket)
;; 							  ,description) "\n")))
;; 		(insert (org-element-interpret-data ast))))))

(defun org-zk--link-prefix-from-link-type (link-type)
  "Return link prefix if LINK-TYPE exists, otherwise return nil."
  (car (rassq link-type org-zk-link-type-prefix-alist)))

(defun org-zk--insert-backlink (&optional link-type backlink-type ivy-prompt-text)
  "Insert link to note, and also insert a backnote to the current note.

Optionally, specify a LINK-TYPE, a BACKLINK-TYPE and an IVY-PROMPT-TEXT."
  (let* ((this-path (concat
					 org-zk-directory
					 (file-name-nondirectory (buffer-file-name))))
		 (link-prefix (org-zk--link-prefix-from-link-type link-type))
		 (backlink-prefix (org-zk--link-prefix-from-link-type backlink-type))
		 (desc (concat backlink-prefix
					   (or (org-zk--title-of-note-in-current-buffer) this-path)))
		 (prompt (or ivy-prompt-text "Link with: ")))
	(ivy-read prompt #'org-zk--ivy-notes-list
			  :action (lambda (title)
						(let* ((link-path (get-text-property 0 'file-name title))
							   (file-name (file-name-nondirectory link-path)))
						  (org-zk--insert-link-in-file link-path this-path desc)
						  (insert (concat
								   "[[file:" file-name "][" link-prefix title "]]")))))))

(defun org-zk-insert-backlink ()
  "Select note from list, and insert link/backlink to/from that note."
  (interactive)
  (org-zk--insert-backlink))

(defun org-zk-insert-folge-backlink ()
  "Select note from list, and insert link/backlink to/from that note.

Link descriptions are prefixed by `<:' and `>:' respectively"
  (interactive)
  (org-zk--insert-backlink :folge-prev :folge-next "Follow note: "))

;; Index files

(defun org-zk-create-new-index (subject)
  "Create a new index note for a new subject and link to the main index.

SUBJECT is the name of the subject."
  (interactive "sCreate index for which subject? ")
  (let* ((this-index-name (format "Index - %s" subject))
		 (main-index-file (concat org-zk-directory "index.org"))
		 (this-index-file (org-zk--new-note this-index-name)))
	(org-zk--insert-link-in-file-or-buffer
	 main-index-file
	 this-index-file
	 (format "Index - %s" subject))
	))

;; ;;;;; this is for adding existing files to the database
;; (require 'cl-lib)

;; (defun add-links-to-db (filename)
;;   (let* ((ast (org-zk--org-element-parse-file filename))
;; 		 (title (org-zk--title-of-note-in-file filename))
;; 		 (link-paths (org-element-map ast 'link
;; 					   (lambda (l)
;; 						 (when (string= (org-element-property :type l) "file")
;; 						   (org-element-property :path l)))))
;; 		 (link-descriptions (org-element-map ast 'link
;; 							  (lambda (l)
;; 								(when (string= (org-element-property :type l) "file")
;; 								  (car (org-element-contents l)))))))
;; 	(message (format "%s" link-paths))
;; 	(message (format "%s" link-descriptions))

	;; Add all links between notes
	;; (cl-mapc
	;;  (lambda (p d)
	;;    (let ((filename-full (concat
	;; 						 org-zk-directory
	;; 						 (file-name-nondirectory filename)))
	;; 		 (path-full (concat
	;; 					 org-zk-directory
	;; 					 (file-name-nondirectory p)))
	;; 		 (link-type :default)
	;; 		 (link-type (dolist
	;; 						(prefix-type org-zk--link-type-prefix-alist ltype)
	;; 					  (let ((prefix (car prefix-type))
	;; 							(type (cdr prefix-type))))
	;; 					  (when (string= (string-prefix-p d) prefix)
	;; 						(setq ltype type)))))
  	;; 	 (org-zk-db--add-link path-full filename-full link-type)))
	;;  link-paths link-descriptions)
	;; (message "\n")))

;; (defun add-existing-notes-to-database ()
;;   (let* ((all-files (cdr (cdr (directory-files org-zk-directory t))))
;; 		 (all-files-full (mapcar
;; 						  (lambda (f) (concat org-zk-directory (file-name-nondirectory f)))
;; 						  all-files))
;; 		 (all-titles (mapcar #'org-zk--title-of-note-in-file all-files-full)))
;; 	(cl-mapc #'org-zk-db--add-note all-titles all-files-full)
;; 	(mapc #'add-links-to-db all-files)))

;; (add-existing-notes-to-database)

(provide 'org-zk)

;;; org-zk.el ends here
