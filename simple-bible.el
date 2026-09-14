;;; simple-bible.el --- Simple offline bible with querying and reading plans -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Sam Matthews
;; Version: 1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: religion, bible, reader

;;; Commentary:

;; The most popular bible reader on emacs right now uses bible gateway to retrieve bible passages
;; This requires an internet connection which is not always doable
;; This is heavily based of of this kjv project and uses the King James Bible TSV from there https://github.com/layeh/kjv

;; TODO
;; Format headings using overlays
;; - https://duckduckgo.com/?q=emacs+overlay+to+make+text+appear+centered&t=ffab&ia=web
;;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html
;; Alternative bibles
;;; - https://github.com/LukeSmithxyz/grb
;;; - https://github.com/LukeSmithxyz/vul
;; [[https://github.com/Zacalot/bible-mode/blob/main/bible-mode.el][Figure out chapter by chapter interface]]
;; [[https://biblereadingplangenerator.com/?start=2026-09-01&total=365&format=calendar&order=traditional&daysofweek=1,2,3,4,5,6,7&books=OT,NT&lang=en&logic=words&checkbox=1&colors=0&dailypsalm=0&dailyproverb=0&otntoverlap=0&reverse=0&stats=0&dailystats=0&nodates=0&includeurls=0&urlsite=biblegateway&urlversion=NIV][Try different reading plans]]
;; Link format to allow referencing the bible in org mode without inserting the entire text simple-bible://link?Short1=ch1-ch2;v1-v2&Short2=ch1...
;;; Advice around browse-url to handle the custom simple-bible protocol, take the query and plug it in to the regular routine to display the correct bible verses

;;; Code:
(defcustom simple-bible-path
	"~/.emacs.d/elpa/simple-bible"
	"Path to default bible text and plan datastore")
(defcustom simple-bible-plan
	"bibleplan"
	"Current plan name - without csv file extension")
(defcustom simple-bible-book
	"kjv"
	"Currently selected bible - without tsv file extension")
(defcustom simple-bible-link-preview
	t
	"Decide if you want to open the queried bible passages when you copy or insert a link")

(defun simple-bible--get-plan-path()
  (format "%s.csv" (file-name-concat simple-bible-path "plans" simple-bible-plan)))

(defun simple-bible--get-text-path()
  (format "%s.tsv" (file-name-concat simple-bible-path "texts" simple-bible-book)))

(require 'subr-x)
;Doesn't work to remove from buffer list but keeping here for now
(define-derived-mode simple-bible-mode view-mode "Simple Bible View"
 (local-set-key (kbd "q") (lambda ()
  	(interactive)
    (kill-current-buffer)
    (delete-window))))

;;;###autoload
;Entirely vibe coded
(defun simple-bible-list-books()
"Return unique list of bible books from the current TSV file and display in a new buffer."
  (interactive)
  (let ((seen (make-hash-table :test 'equal)) result)
    (with-temp-buffer
      (insert-file-contents (simple-bible--get-text-path))
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* (
  				(fields (split-string line "\t"))
          (book (concat (nth 0 fields) " (" (nth 1 fields) ")")))
          (unless (gethash book seen)
            (puthash book t seen)
            (push book result))))
  	  (kill-current-buffer))
      
    ;; Create and display in new buffer
    (let ((buf (get-buffer-create "*Bible Books*")))
      (with-current-buffer buf
        (erase-buffer)
        (dolist (book (nreverse result))
          (insert book "\n"))
        (goto-char (point-min))
        (simple-bible-mode))
    (select-window (display-buffer buf)))))

(defun simple-bible--print-bible-verses(content)
	(let (
		(books-seen (list))
		(book-chapters-seen (list)))
    (cl-loop for verse in (string-split content "\n") for index from 0 do (progn
		  (when (not (string-blank-p verse))
				(let* (
					(fields (split-string verse "\t"))
					(book (car fields)))
				  (when (not (member book books-seen)) (cl-pushnew book books-seen) (insert (format "\n----%s----\n" (nth 0 fields))))
	        (let* (
						(chapter (nth 3 fields))
						(book-chapter (concat book chapter)))
  			    (when (not (member book-chapter book-chapters-seen)) (cl-pushnew book-chapter book-chapters-seen) (insert (format "\n----Chapter %s----\n" chapter)))
  		      (insert (format "%s: %s\n" (nth 4 fields) (nth 5 fields))))))))))

(defun simple-bible--search(query)
  (let (
	  (books (string-split query ";"))
    (book-chapter-pairs (list)))
    (dolist (book books)
      (let* (
  	    (book-parts (string-split (string-trim book) " "))
			  ; Deals with things like "1 Kings"
        (numeric-prefix (> (string-to-number (car book-parts)) 0))
				(parts-count (- (length book-parts) (if numeric-prefix 1 0)))
        (name (string-replace "*" "[a-zA-Z1-3\s]*" (string-trim (if numeric-prefix (concat (car book-parts) " " (nth 1 book-parts)) (car book-parts))))))

				(setq-local
				  chapter-range-string nil
				  chapter-range nil
				  chapter-start nil
				  chapter-end nil
				  verse-range-string nil
					verse-range nil
					verse-start nil
					verse-end nil)
				; Subtract 1 from the length of the list when we have a numeric prefix - treating the two entries as one for the book name
        (when (> parts-count 1) (setq-local chapter-range-string (nth (if numeric-prefix 2 1) book-parts)))
				(when chapter-range-string (setq-local chapter-range (string-split chapter-range-string "-")))
        (when chapter-range (setq-local chapter-start (string-to-number (car chapter-range))))
        (when chapter-start (setq-local chapter-end (if (equal (length chapter-range) 1) chapter-start (string-to-number (nth 1 chapter-range)))))
				; We have more parameters after the chapters - this means we have verses
				(when (and chapter-range (> parts-count 2)) (setq-local verse-range-string (nth (+ (if numeric-prefix 2 1) 1) book-parts)))
				(when verse-range-string (setq-local verse-range (string-split verse-range-string "-")))
        (when verse-range (setq-local verse-start (string-to-number (car verse-range))))
        (when verse-start (setq-local verse-end (if (equal (length verse-range) 1) verse-start (string-to-number (nth 1 verse-range)))))
				(if chapter-range
			    ;Note that you cannot use \t for keep-lines - needs to be a literal tab character
					(if (string-search "*" chapter-range-string)
						(if verse-range
						  (if (string-search "*" verse-range-string)
								(cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*" name) book-chapter-pairs)
								(cl-loop for i from verse-start to verse-end do
									(cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*	[a-zA-Z0-9\s]*	%d	" name i) book-chapter-pairs)))
							(cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*" name) book-chapter-pairs))
  		      (cl-loop for i from chapter-start to chapter-end do
						  (if (and verse-range (not (string-search "*" verse-range-string)))
							  (cl-loop for j from verse-start to verse-end do
								  (cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*	%d	%d	" name i j) book-chapter-pairs))
							  (cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*	%d	" name i) book-chapter-pairs))))
				  (cl-pushnew (format "^[a-zA-Z1-3\s]*	%s	[0-9]*" name) book-chapter-pairs))))
	  book-chapter-pairs))

(defun simple-bible--query(query)
  (with-temp-buffer
    (insert-file-contents (simple-bible--get-text-path))
    (keep-lines (mapconcat 'identity (simple-bible--search query) "\\|"))
	  (buffer-string)))

(defun simple-bible--open(query)
	(with-current-buffer (generate-new-buffer "*Bible*")
		(simple-bible--print-bible-verses (simple-bible--query query))
		(goto-char (point-min))
		(simple-bible-mode)
		(switch-to-buffer (current-buffer))))

(defun simple-bible-insert-link(query)
	(interactive "sEnter Query: "))

(defun simple-bible-copy-link(query)
	(interactive "sEnter Query: "))

(defun simple-bible-insert-text(query)
	(interactive "sEnter Query: ")
	(simple-bible--print-bible-verses (simple-bible--query query)))

(defun simple-bible-copy-text(query)
	(interactive "sEnter Query: ")
	(with-temp-buffer
	  (simple-bible--print-bible-verses (simple-bible--query query))
	  (kill-new (buffer-string)))
	(message "Copied bible passages to clipboard"))

(defun simple-bible-open(query)
"Return the current bible and display the given query results"
  (interactive "sEnter Query: ")
  (simple-bible--open query))

(defun simple-bible-open-plan()
  (interactive)
  (with-temp-buffer
    (insert-file-contents (simple-bible--get-plan-path))
		(setq-local line-point (search-forward (format-time-string "%F") nil t))
		(when (not line-point) (setq-local line-point (search-forward (format-time-string "%m-%d") nil t)))
    (when line-point
      (let ((book-chapter-pairs (simple-bible--search (substring (nth 1 (string-split (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\",\"")) 0 -1 ))))
        (erase-buffer)
        (insert-file-contents (simple-bible--get-text-path))
        ;keep-lines expects a \| rather than \\|
        (keep-lines (mapconcat 'identity book-chapter-pairs "\\|"))
        (let ((content (buffer-string)))
          (with-current-buffer (generate-new-buffer "*Bible Daily Reading*")
				    (simple-bible--print-bible-verses content)
            (goto-char (point-min))
						;(view-mode)
            (simple-bible-mode)
            (switch-to-buffer (current-buffer))))))
  	(kill-current-buffer)))

(provide 'simple-bible)
;;; simple-bible.el ends here
