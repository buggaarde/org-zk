;; -*- lexical-binding: t; -*-

(defun org-zk--org-element-parse-file (filename)
  "Open FILENAME in temp buffer, and call org-element-parse-buffer.
Return the resulting org-element AST."
  (with-temp-buffer
	(progn
	  (org-mode)
	  (insert-file-contents filename)
	  (org-element-parse-buffer))))

(defun org-zk--title-of-note-in-current-buffer ()
  "Return the value of the #+TITLE in current buffer.
The function returns the value of the first encountered keyword value, and therefore assumes that #+TITLE is the first keyword in the buffer."
  (let ((ast (org-element-parse-buffer)))
	(org-element-map ast 'keyword 
	  (lambda (k) (org-element-property :value k))
	  nil t)))

(defun org-zk--title-of-note-in-file (filename)
  "Return the #+TITLE of the note in FILENAME.
Opens FILENAME in temp buffer, and call org-zk--title-of-note-in-current-buffer.
The function returns the value of the first encountered keyword value, and therefore assumes that #+TITLE is the first keyword in the file.

See also: org-zk--title-of-note-in-current-buffer."
  (with-temp-buffer
	(progn
	  (org-mode)
	  (insert-file-contents filename)
	  (zettel--title-of-note-in-current-buffer))))

(defun org-zk--org-headline-by-name (ast name)
  "Return first encountered headline with NAME from AST."
  (org-element-map ast 'headline
	(lambda (h)
	  (when (string= (org-element-property :raw-value h) name)
		h))
	nil t))

(provide 'org-zk-common)
