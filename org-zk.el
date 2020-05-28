;; -*- lexical-binding: t; -*-

(require 'setup-zettel-common)
(require 'setup-zettel-gather)

(defun zettel--ivy-notes-list (str pred _)
  "Generate the ivy notes list."
  (let* ((deft-files (deft-current-files))
		 (file-names deft-files)
		 (titles (mapcar #'deft-file-title deft-files)))
	(cl-mapcar (lambda (fn ti) (propertize ti 'file-name fn))
			   file-names titles)))

(defun zettel--insert-link-in-file (filename path description)
  "Insert link to PATH with DESCRIPTION in FILENAME.

The link is inserted under the `References' headline by appending the link
to the headline content in the org-element AST."
  (with-temp-file filename
	(let* ((ast (zettel--org-element-parse-file filename))
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

(defun zettel--insert-backlink (&optional link-prefix backlink-prefix ivy-prompt-text)
  "Insert link to note, and also insert a backnote to the current note.

Optionally, specify a LINK-PREFIX, a BACKLINK-PREFIX and an IVY-PROMPT-TEXT."
  (let* ((path (file-name-nondirectory (buffer-file-name)))
		 (desc (concat backlink-prefix
					   (or (zettel--title-of-note-in-current-buffer) path)))
		 (prompt (or ivy-prompt-text "Link with: ")))
	(ivy-read prompt #'zettel--ivy-notes-list
			  :action (lambda (title)
						(let* ((full-path (get-text-property 0 'file-name title))
							   (file-name (file-name-nondirectory full-path)))
						  (zettel--insert-link-in-file full-path path desc)
						  (insert (concat
								   "[[file:" file-name "][" link-prefix title "]]")))))))

(defun zettel-insert-org-link ()
  "Prompt for a note and insert a link to that file."
  (interactive)
  (ivy-read "Insert link: " #'zettel--ivy-notes-list
			:action (lambda (title)
					  (let ((file-name
							 (file-name-nondirectory (get-text-property 0 'file-name title))))
						(insert (concat "[[file:" file-name "][" title "]]"))))
			:caller 'zettel-insert-org-link))

(defun zettel-insert-backlink ()
  "Select note from list, and insert link/backlink to/from that note."
  (interactive)
  (zettel--insert-backlink))

(defun zettel-insert-folge-backlink ()
  "Select note from list, and insert link/backlink to/from that note.

Link descriptions are prefixed by `<:' and `>:' respectively"
  (interactive)
  (zettel--insert-backlink "<:" ">:" "Follow note: "))

(global-set-key (kbd "C-c z d") #'deft)
(global-set-key (kbd "C-c z l") #'zettel-insert-org-link)
(global-set-key (kbd "C-c z b") #'zettel-insert-backlink)
(global-set-key (kbd "C-c z f") #'zettel-insert-folge-backlink)
(global-set-key (kbd "C-c z g") #'zettel-gather-notes-beginning-here)
(global-set-key (kbd "C-c z r") #'deft-refresh)

(provide 'setup-zettel)
