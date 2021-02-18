;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun org-zk--insert-note-from-file (buffer filename)
  "Insert note from FILENAME into BUFFER. 

If BUFFER does not already exist, create it.

Depending on whether a FILENAME has type `:singular', `:branch' or `:root',
and whether a file is previously `:visited?' or not, insert the appropriate
heading and note content.

Contents of `:branch'ing notes are inserted under a headline one level larger
than that of the heading from which it is linked.  `:singular' notes have their
headings inserted not as an `org-mode' headline, but instead as text both in
*bold* and _underlined_.  The `:root' note is inserted as a level 1 headline.

When a FILENAME is `:visited?', it is marked as duplicate, and the note contents
are replaced with a message indicating its duplicity.  Otherwise, the headline 
is inserted as a radio-target, in preparation for potential duplicate notes."
  (with-current-buffer (get-buffer-create buffer)
	(let* ((title (org-zk--title-of-note-in-file filename))
		   (type (get-text-property 0 :type filename))
		   (level (get-text-property 0 :level filename))
		   (visited? (get-text-property 0 :visited? filename))
		   (ast (org-zk--org-element-parse-file filename))
		   (notes (org-zk--org-headline-by-name ast "Note"))
		   (note (org-element-map notes 'section #'identity nil t))
		   (note (when note (org-element-extract-element note))))
	  (if visited?
		  (setq note
				(concat
				 "/Duplicate note. The contents can be found where the note was first encountered in the gathering process./\n"
				 "/Activate radio targets (by pressing =C-c C-c= at an appropriate headline) to go to that note./\n"))
		(setq title `(radio-target () ,title)))
	  (cond
	   ((eq type :singular)
		;; insert title as bold, and the note underneath
		(insert
		 (org-element-interpret-data
		  `((bold
			 ()	(underline
				 () ,title))
			"\n" ,note "\n"))))
	   ((or (eq type :branch)
			(eq type :root))
		;; insert the title as a heading with the level associated with it
		(insert
		 (org-element-interpret-data
		  `(headline
			(:level ,level
			 :title ,title
			 :post-blank 1)
			,note))))))))

(defun org-zk--all-links-in-file (filename)
  "Return all links in FILENAME under the `References' headline."
  (let* ((ast (org-zk--org-element-parse-file filename))
		 (refs (org-zk--org-headline-by-name ast "References"))
		 (links (org-element-map refs 'link #'identity)))
	(mapcar #'org-element-extract-element links)))


(setq org-zk-excluded-prefixes '("<:"))

(defun org-zk--link-description-has-excluded-prefix? (link)
  "Predicate indicating whether a LINK description has an excluded prefix.

Excluded prefixes are stored in `org-zk-excluded-prefixes'."
  (let ((match? nil))
	(dolist (pre org-zk-excluded-prefixes match?)
	  (let* ((desc (org-zk--link-description link))
			 (desc-matches? (string-prefix-p pre desc)))
		(setq match? (or match? desc-matches?))))
	match?))

(defun org-zk--link-description (link)
  (car (org-element-contents link)))

(defun org-zk--link-path (link)
  (org-element-property :path link))

(defun org-zk--note-parent (note)
  (get-text-property 0 :parent note))

(defun org-zk--all-non-excluded-links-in-file (filename)
  "Return a list of all non-exluded links in FILENAME.

Propertize the links with either type `:singular' or `:branch', depending
on the number of links in a note, and update the `:level' based on the level
of FILENAME.

Only links from the `References' headline are parsed."
  (when-let* ((ast (org-zk--org-element-parse-file filename))
			  (refs (org-zk--org-headline-by-name ast "References"))
			  (links (org-element-map refs 'link #'identity))
			  (children (seq-remove
						 (lambda (l)
						   (when-let
							   ((path (org-zk--link-path l))
								(parent (org-zk--note-parent filename)))
							 (message path)
							 (message parent)
							 (message "\n")
							 (string=
							  (file-name-nondirectory path)
							  (file-name-nondirectory parent))))
						 links))
			  (children (seq-remove
						 #'org-zk--link-description-has-excluded-prefix?
						 children))
			  (child-paths (cl-loop for c in children
									collect (org-element-property :path c)))
			  (num-children (length children))
			  (parent-level (get-text-property 0 :level filename)))
	(if (= num-children 1)
		`(,(propertize (car child-paths) :type :singular :level parent-level :parent filename))
	  (cl-loop for c in child-paths
			   collect (propertize c :type :branch :level (1+ parent-level) :parent filename)))))

(defun org-zk--depth-first-traversal-link-filenames ()
  "Traverse links from note in depth-first manner, and return list of filenames.

When a duplicate link is encountered, push the note onto the stack a final time,
but do not traverse the branch again.  Instead mark that note as visited.

This traversal keeps track of the number of links in each note, as well as the
associated levels of the headlines in the final gathering."
  (let* ((file (propertize
				(file-name-nondirectory (buffer-file-name))
				:type :root :level 1))
		 (stack (list file))
		 (visited-notes (list)))
	(while stack
	  (let* ((filename (pop stack))
			 (links (org-zk--all-non-excluded-links-in-file filename))
			 (already-visited? (member filename visited-notes)))
		(when already-visited?
			(setq filename (propertize filename :visited? t)))
		(push filename visited-notes)
		(unless already-visited?
		  (when links
			(cl-loop for l in (nreverse links) do
					 (push l stack))))))
	(nreverse visited-notes)))

(defun org-zk-gather-notes-beginning-here ()
  "Gather all notes, beginning with the current buffer.

A gathering is an `org-mode' buffer containing a depth-first traversal of the
notes linked to in the References-section of each note.  Any branching from a
note (i.e.  when two or more notes are linked to) are collapsible under headings
with increased levels.  When only one link is found in a note, the headline is
instead inserted in *bold* and _underlined_.

Backlinks are automatically ignored in the depth-first search.  Backlinks are
identified by a prefix `<:' in the link description.  It is possible to
automatically exclude more prefixed notes, by modifying the
`org-zk-excluded-prefixes'-list.

When a note, deeper in the note tree, links back to a note earlier in the
tree, the specific branch is not traversed again.  Instead, the heading is
inserted without any contents, and a message indicating a duplicate note, and
how to reach the contents, is shown."
  (interactive)
  (let ((buffer
		 (get-buffer-create
		  (format "ZK gathering: %s"
				  (org-zk--title-of-note-in-current-buffer)))))
	(with-current-buffer buffer
	  (org-mode))
	(mapc (lambda (f) (org-zk--insert-note-from-file buffer f))
		  (org-zk--depth-first-traversal-link-filenames))))


(provide 'org-zk-gather)
