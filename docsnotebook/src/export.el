#!/usr/bin/env -S timeout 1.5s emacs -Q --script
(setq org-babel-load-languages nil)
(require 'org)
(require 'org-id)

;;; Org puts some metadata about the author in the generated output
;;; By default this is the username, which isn't really helpful
(setq user-login-name ""
	  user-full-name ""
	  user-real-login-name ""
	  user-mail-address "")

(setq org-html-preamble nil
	  org-html-postamble nil
	  org-html-scripts nil
	  org-src-fontify-natively nil
	  org-html-metadata-timestamp-format "")


(while argv
  (let ((filename (pop argv))
		(case-fold-search t)
		(inhibit-read-only t))
	(message "Exporting %s" filename)
	(find-file filename)
	(while (search-forward "#+include:" nil t)
	  (delete-region (point-at-bol) (point-at-eol)))
	(org-table-map-tables (lambda () (org-table-recalculate t t)))
	(org-html-export-to-html)))

(kill-emacs 0)
