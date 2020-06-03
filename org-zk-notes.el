;;; org-zk-notes --- Description of your package -*- lexical-binding: t; -*-
;; Copyright (c) 2020 Simon Bugge Siggaard

;; Author: Simon Bugge Siggaard <simsig@gmail.com>
;; Maintainer: Simon Bugge Siggaard <simsig@gmail.com>
;; Created: 31 May 2020
;; Version: 0.1
;; Keywords: some keywords here
;; URL: https://github.com/buggaarde/org-zk

;;; Commentary:

;;; Code:

(require 'org-zk)

(defun org-zk-new-note ()
  "Create a new zettel and open file in buffer."
  (let ((filename
		 (concat org-zk-directory (format-time-string "%Y%m%d%H%M%S") ".org")))
	(with-temp-file filename
	  (insert "#+title:\n#+startup: showall\n** Note\n\n\n** References\n\n** Sources\n\n"))
	(find-file filename)
	(goto-line 5)))


(provide 'org-zk-notes)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; org-zk-notes.el ends here
