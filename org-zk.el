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
  
  (defcustom org-zk-main-index-file (concat org-zk-directory "index.org")
	"The main index file that link to all other subject-specific index files."
	:type 'file
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

;; -- Opening and creating notes

(defun org-zk--create-new-note (title)
  "Create a new zettel with TITLE and save to disk.

Return the file name of the zettel."
  (let ((file-name (concat
					org-zk-directory
					(format-time-string "%Y%m%d%H%M%S")
					".org")))
	(with-temp-file file-name
	  (insert (format
			   "#+title: %s\n#+startup: showall\n** Note\n\n\n** References\n\n** Sources\n\n"
			   title)))
	file-name))

(defun org-zk--create-new-note-and-open (title)
  "Create a new zettel with TITLE and open the file in buffer."
  (let ((file-name (org-zk--create-new-note title)))
	(find-file file-name)
	(goto-char (point-min))
	(let ((inhibit-message t))
	  (forward-line (1- 4)))))

(defun org-zk-create-note-with-title (title)
  "Create a new zettel with TITLE, but don't open the file."
  (interactive "sTitle of the new zettel: ")
  (org-zk--create-new-note title))

(defun org-zk-create-empty-note-and-open ()
  "Create an empty zettel and open file in buffer."
  (interactive)
  (org-zk--create-new-note-and-open ""))

(defun org-zk-create-note-with-title-and-open (title)
  "Create a new zettel with TITLE and open file in buffer."
  (interactive "sTitle of the new zettel: ")
  (org-zk--create-new-note-and-open title))

(defun org-zk-open-note ()
  "Open an existing zettel in current buffer."
  (interactive)
  (ivy-read "Open note: " #'org-zk--ivy-notes-list
			:action (lambda (title)
					  (let ((path (get-text-property 0 'file-name title)))
						(find-file path)))))

(defun org-zk--prune-notes-without-titles ()
  "Delete all notes that contain no titles."
  (let ((all-files (seq-filter #'file-regular-p (directory-files org-zk-directory t))))
	(mapc (lambda (file)
			(let ((title (org-zk--title-of-note-in-file file)))
			  (when (string= title "")
				(delete-file file))))
		  all-files)))

(defun org-zk-prune-notes-without-titles ()
  "Delete all notes that contain no titles."
  (interactive)
  (org-zk--prune-notes-without-titles))

;; -- Ivy helpers

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


;; -- Links between notes

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

(defun org-zk--add-link-in-buffer (buffer path description)
  "Insert link to PATH with DESCRIPTION in buffer containing contents from FILENAME.

The link is inserted under the `References' headline by appending
the link to the headline content in the org-element AST."
  (with-current-buffer buffer
	(let ((ast (org-zk--insert-link-in-ast
				(org-element-parse-buffer) path description))
		  (tmp-buffer (generate-new-buffer " *org-zk-tmp-buffer*")))
	  (save-excursion
		(with-current-buffer tmp-buffer
		  (insert (org-element-interpret-data ast)))
		(replace-buffer-contents tmp-buffer)
		(kill-buffer tmp-buffer)))))

(defun org-zk--add-link-in-file (filename path description)
  "Insert link to PATH with DESCRIPTION in FILENAME.

The link is inserted under the `References' headline by appending the link
to the headline content in the org-element AST."
  (with-temp-file filename
	(let ((ast (org-zk--insert-link-in-ast
				(org-zk--org-element-parse-file filename) path description)))
	  (insert (org-element-interpret-data ast)))))

(defun org-zk--add-link-in-file-or-buffer (filename path description)
  "Insert link to PATH with DESCRIPTION in FILENAME, whether the file is open in a buffer or not.

The link is inserted under the `References' headline by appending
the link to the headline content in the org-element AST."
  (let ((buffer (find-buffer-visiting filename)))
	(if buffer
		(org-zk--add-link-in-buffer buffer path description)
	  (org-zk--add-link-in-file filename path description))))

(defun org-zk--link-this-and-that-note
	(this-note that-note &optional this-note-prefix that-note-prefix)
  "Add a link from THIS-NOTE to THAT-NOTE and vice versa.

THIS-NOTE and THAT-NOTE are full paths to the notes.
THIS-NOTE-PREFIX and THAT-NOTE-PREFIX prefixes the respective descriptions."
  (progn
	(org-zk--add-link-in-file-or-buffer
	 this-note that-note
	 (concat that-note-prefix (org-zk--title-of-note-in-file that-note)))
	(org-zk--add-link-in-file-or-buffer
	 that-note this-note
	 (concat this-note-prefix (org-zk--title-of-note-in-file this-note)))))

(defun org-zk--insert-inline-link-to-note (note &optional description-prefix)
  "Insert a link, inline in the current note, to NOTE.

NOTE is the full path to the note."
  (insert
   (concat "[[" (file-name-nondirectory note) "]["
		   (concat description-prefix (org-zk--title-of-note-in-file note)) "]]")))

(defun org-zk--link-prefix-from-link-type (link-type)
  "Return link prefix if LINK-TYPE exists, otherwise return nil."
  (car (rassq link-type org-zk-link-type-prefix-alist)))

(defun org-zk--add-backlink-to-references (&optional link-type backlink-type ivy-prompt-text)
  "Insert link to note, and also insert a backnote to the current note.
If the note doesn't already exist, create it before linking with it.

Optionally, specify a LINK-TYPE, a BACKLINK-TYPE and an IVY-PROMPT-TEXT."
  (let* ((this-path (concat
					 org-zk-directory
					 (file-name-nondirectory (buffer-file-name))))
		 (this-prefix (org-zk--link-prefix-from-link-type backlink-type))
		 (that-prefix (org-zk--link-prefix-from-link-type link-type))
		 (prompt (or ivy-prompt-text "Link with: ")))
	(ivy-read prompt #'org-zk--ivy-notes-list
			  :action (lambda (title)
						(let* ((that-path (or (get-text-property 0 'file-name title)
											  (org-zk--create-new-note title))))
						  (org-zk--link-this-and-that-note
						   this-path that-path
						   this-prefix that-prefix))))))

(defun org-zk-insert-backlink (&optional link-type backlink-type ivy-prompt-text)
  "Add backlink to the references section of the note, as well as in the note itself.
If the note doesn't already exist, create it before linking with it.

Optionally, specify a LINK-TYPE, a BACKLINK-TYPE and an IVY-PROMPT-TEXT."
  (interactive)
  (let* ((this-path (concat
					 org-zk-directory
					 (file-name-nondirectory (buffer-file-name))))
		 (this-prefix (org-zk--link-prefix-from-link-type backlink-type))
		 (that-prefix (org-zk--link-prefix-from-link-type link-type))
		 (prompt (or ivy-prompt-text "Link with: ")))
	(ivy-read prompt #'org-zk--ivy-notes-list
			  :action (lambda (title)
						(let* ((that-path (or (get-text-property 0 'file-name title)
											  (org-zk--create-new-note title))))
						  (org-zk--link-this-and-that-note
						   this-path that-path
						   this-prefix that-prefix)
						  (org-zk--insert-inline-link-to-note that-path that-prefix))))))

(defun org-zk-add-backlink-to-references ()
  "Select note from list, and insert link/backlink to/from that note."
  (interactive)
  (org-zk--add-backlink-to-references))

(defun org-zk-add-folge-backlink-to-references ()
  "Select note from list, and insert link/backlink to/from that note.

Link descriptions are prefixed by `<:' and `>:' respectively"
  (interactive)
  (org-zk--add-backlink-to-references :folge-prev :folge-next "Follow note: "))

;; Index files

(defun org-zk-create-new-index (subject)
  "Create a new index note for a new subject and link to the main index.

SUBJECT is the name of the subject."
  (interactive "sCreate index for which subject? ")
  (let* ((this-index-name (format "%s -- Index" subject))
		 (main-index-file org-zk-main-index-file)
		 (this-index-file (org-zk--new-note this-index-name)))
	(when (not (file-exists-p main-index-file))
	  (org-zk--new-note "Index"))
	(org-zk--add-link-in-file-or-buffer
	 main-index-file
	 this-index-file
	 this-index-name)
	(org-zk--add-link-in-file-or-buffer
	 this-index-file
	 main-index-file
	 "Index")
	(find-file this-index-file)))

;; (defun org-zk--create-note-and-add-link (note-title)
;;   (let ((file-name (org-zk--new-note note-title)))
;; 	(org-zk--add-backlink-to-references)
;; 	file-name))

;; (defun org-zk-create-note-and-add-link (note)
;;   "Create a new NOTE and add the link to the content of the `Refereces' section of this note."
;;   (interactive "sCreate note and link to: ")
;;   (org-zk--create-note-and-add-link note))

;; (defun org-zk-create-note-and-insert-link (note)
;;   "Create a new NOTE and insert the link in the note and to the content of the `Refereces' section of this note."
;;   (interactive "sCreate note and link to: ")
;;   (let ((note-path (org-zk--create-note-and-add-link note)))
;; 	(insert (concat "[[" note-path "][" note "]]"))))

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
