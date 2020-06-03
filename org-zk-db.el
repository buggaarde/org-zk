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

(require 'org-zk-common)
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
	 filename])
  "The table containing all notes and their respective file-names.")

(emacsql zk-db [:create-table :if-not-exists org-zk-db--notes $S1]
		 org-zk-db--notes-schema)

(defvar org-zk-db--links-schema
  '([(id integer :primary-key)
	 (from-note-id integer)
	 (to-note-id integer)
	 prefix]
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
			:order-by notes:id]))

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
  (unless (org-zk-db--filename-in-db? zk-db filename)
	(let ((this-id (string-to-number filename)))
	  (emacsql zk-db [:insert-into org-zk-db--notes
					  :values $v1]
			   `([,this-id ,title ,filename])))))

(provide 'org-zk-db)

;;; org-zk-db.el ends here
