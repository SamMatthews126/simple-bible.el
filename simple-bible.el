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

(defun simple-bible--get-plan-path()
  (format "%s.csv" (file-name-concat simple-bible-path "plans" simple-bible-plan)))

(defun simple-bible--get-text-path()
  (format "%s.tsv" (file-name-concat simple-bible-path "texts" simple-bible-book)))

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

(defun simple-bible-open(book)
"Return the current bible and display the given book"
  (interactive "sEnter Book: ")
  (with-current-buffer (generate-new-buffer "*Bible*")
    (insert-file-contents (simple-bible--get-text-path))
    (keep-lines (format "^\\(%s\\)\t\\|\\(\t%s\t\\)" book book))
    (let (result)
			(dolist (line (split-string (buffer-string) "\n" t))
				(let* (
  				(fields (split-string line "\t"))
          (verse (concat (nth 4 fields) ": " (nth 5 fields))))
					(when (equal (nth 4 fields) "1") 
						(when (equal (nth 3 fields) "1")
							(push (nth 0 fields) result))
						(push (format "\nChapter %s\n" (nth 3 fields)) result))
          (push verse result)))
      (erase-buffer)
      (dolist (verse (nreverse result))
        (insert verse)
      (when (not (equal verse "\n")) (insert "\n"))))
    (goto-char (point-min))
    (simple-bible-mode)
    (switch-to-buffer (current-buffer))))

(defun simple-bible-open-plan()
  (interactive)
  (with-temp-buffer
    (insert-file-contents (simple-bible--get-plan-path))
		(let ((line-point (search-forward (format-time-string "%F"))))
      (when line-point
        (let (
				  (books (string-split (substring (nth 1 (string-split (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\",\"")) 0 -1) ";"))
          (book-chapter-pairs (list)))
          (dolist (book books)
            (let* (
  						(book-parts (string-split book " "))
              (chapter-range (string-split (format "%s" (car (last book-parts))) "-"))
              (name (string-trim (if (equal (length book-parts) 3) (concat (nth 0 book-parts) " " (nth 1 book-parts)) (nth 0 book-parts))))
              (chapter-start (string-to-number (nth 0 chapter-range)))
              (chapter-end (if (equal (length chapter-range) 1) chapter-start (string-to-number (nth 1 chapter-range)))))
							;Note that you cannot use \t for keep-lines - needs to be a literal tab character
  					  (cl-loop for i from chapter-start to chapter-end do (cl-pushnew (format "%s	[0-9]*	%d	" name i) book-chapter-pairs))))
          (erase-buffer)
          (insert-file-contents (simple-bible--get-text-path))
          ;keep-lines expects a \| rather than \\|
          (keep-lines (mapconcat 'identity book-chapter-pairs "\\|"))
          (let ((content (buffer-string)))
            (message content)
            (with-current-buffer (generate-new-buffer "*Bible Daily Reading*")
              (cl-loop for line in (string-split content "\n") for i from 0 do (progn
                (let ((fields (split-string line "\t")))
  					      (when (equal (nth 4 fields) "1") 
  						      (when (or (equal (nth 3 fields) "1") (equal i 0))
  							      (insert (format "\n----%s----\n" (nth 0 fields))))
  						      (insert (format "\n----Chapter %s----\n" (nth 3 fields))))
  							  (insert (format "%s: %s\n" (nth 4 fields) (nth 5 fields))))))
              (goto-char (point-min))
							;(view-mode)
              (simple-bible-mode)
              (switch-to-buffer (current-buffer)))))))
  	(kill-current-buffer)))

(provide 'simple-bible)
;;; simple-bible.el ends here
