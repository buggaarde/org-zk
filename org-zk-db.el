;;; org-zk-db --- The database backend for org-zk -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Simon Bugge Siggaard

;; Author: Simon Bugge Siggaard <simsig@gmail.com>
;; Maintainer: Simon Bugge Siggaard <simsig@gmail.com>
;; Created: 30 May 2020
;; Version: 0.1
;; Keywords: org mode zettelkasten
;; URL: https://github.com/buggaarde/org-zk
;; Package-Requires: ((emacsql "3.0.0") (emacsql-sqlite "1.0.0"))


;;; Commentary:


;;; Code:
(require 'emacsql)
(require 'emacsql-sqlite)

;; Define and create the databases

(defvar internal-org-zk-directory
  (expand-file-name ".org-zk/" user-emacs-directory))

(unless (file-exists-p internal-org-zk-directory)
  (make-directory internal-org-zk-directory))

(defvar zk-db (emacsql-sqlite
			   (expand-file-name "org-zk.db" internal-org-zk-directory)))

(defvar org-zk-db--notes-schema
  '([(id integer :primary-key)
	 note-title
	 (filename :unique)])
  "The table containing all notes and their respective file-names.")

(emacsql zk-db [:create-table :if-not-exists org-zk-db--notes $S1]
		 org-zk-db--notes-schema)

(defvar org-zk-db--links-schema
  '([(id integer :primary-key)
	 (from-note-id integer)
	 (to-note-id integer)
     type]
	(:unique [from-note-id to-note-id type])
	(:foreign-key [from-note-id] :references org-zk-db--notes [id]
				  :on-delete :cascade)
	(:foreign-key [to-note-id] :references org-zk-db--notes [id]
				  :on-delete :cascade))
  "The table connects all links between notes.")

(emacsql zk-db [:create-table :if-not-exists org-zk-db--links $S1]
		 org-zk-db--links-schema)

;;; Functions for accessing and modifying the database

(defun org-zk-db--all-notes-filenames ()
  "Return all notes from database."
  (emacsql zk-db
		   [:select [note-title filename]
		    :from org-zk-db--notes
			:order-by id :desc]))

(defun org-zk-db--all-links ()
  "Return all notes from database."
  (emacsql zk-db
		   [:select [from-note-id to-note-id type]
		    :from org-zk-db--links
			:order-by id :desc]))

(defun org-zk-db--filename-in-db? (db fname)
  "Return non-nil if filename FNAME is in DB."
  (car (car
		(emacsql db
				 [:select [filename]
				  :from org-zk-db--notes
				  :where (= filename $r1)]
				 fname))))

(defun org-zk-db--add-note (title filename)
  "Add note with TITLE and FILENAME to the database."
  (let ((this-id (string-to-number (file-name-base filename))))
	(condition-case err
		(emacsql zk-db [:insert-into org-zk-db--notes
						:values $v1]
				 `([,this-id ,title ,filename]))
	  (emacsql-error
	   (message (format "Note with file-name %s already exists. Didn't add note to database." filename))))))

(defun org-zk-db--num-rows (table)
  (car (car (emacsql zk-db [:select (funcall count id) :from $i1]
					 table))))

(defun org-zk-db--add-link (to-file-name from-file-name &optional link-type)
  "Add link between TO-FILE-NAME and FROM-FILE-NAME.

Optionally provide a LINK-TYPE, preferably as a keyword."
  (let ((this-id (1+ (org-zk-db--num-rows 'org-zk-db--links)))
		(to-id (string-to-number (file-name-base to-file-name)))
		(from-id (string-to-number (file-name-base from-file-name)))
		(ltype (or link-type :default)))
	(message "in add-link")
	(message "to-id: %s" to-id)
	(message "from-id: %s" from-id)
	(condition-case err
		(emacsql zk-db [:insert-into org-zk-db--links
						:values $v1]
				 `([,this-id ,to-id ,from-id ,ltype]))
	  (emacsql-error
	   (message (format "Link of type %s between %s and %s already exists. Didn't add link to database."
						ltype to-file-name from-file-name))))))

(defun org-zk-db--delete-all-rows (table)
  (emacsql zk-db [:delete :from $i1] table))

(provide 'org-zk-db)



;;; org-zk-db.el ends here
