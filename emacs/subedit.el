;;; subedit.el --- edit part of a buffer in a new buffer

;; Copyright (C) 2007 Guanpeng Xu.

;; Maintainer: Guanpeng Xu <herberteuler@hotmail.com>
;; Keywords: subedit

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Todo:
;;
;;    - Add warnings when a buffer having subedit buffers that are
;;      changed is being killed
;;
;;    - If the new region to be edited is overlapped with previous
;;      edited region, don't create a new buffer, but use the existing
;;      one

;;; Code:

(defvar subedit-new-buffer-id 0
  "New buffer's ID for current buffer in subedit.
It is appended to the new buffer's name and increased every time
a new buffer is created.")
(make-variable-buffer-local 'subedit-new-buffer-id)

(defvar subedit-line-prefix nil
  "The prefix of every line in the original region.")
(make-variable-buffer-local 'subedit-line-prefix)

(defvar subedit-line-suffix nil
  "The suffix of every line in the original region.")
(make-variable-buffer-local 'subedit-line-suffix)

(defvar subedit-original-buffer nil
  "The original buffer of the current buffer.")
(make-variable-buffer-local 'subedit-original-buffer)

(defvar subedit-original-region-beginning-marker nil
  "The marker at the beginning of the region in the original buffer.")
(make-variable-buffer-local 'subedit-original-region-beginning-marker)

(defvar subedit-original-region-end-marker nil
  "The marker at end of the region in original buffer.")
(make-variable-buffer-local 'subedit-original-region-end-marker)

(defvar subedit-old-window-configuration nil
  "The window configuration before a new subedit buffer is created.")
(make-variable-buffer-local 'subedit-old-window-configuration)

(defvar subedit-line-prefix-history nil
  "History of line prefixes entered with `subedit-region'.")

(defvar subedit-line-suffix-history nil
  "History of line suffixes entered with `subedit-region'.")

(defvar subedit-editing-mode-history nil
  "History of editing modes entered with `subedit-region'.")

;;;###autoload
(defun subedit-region (beg end prefix suffix mode)
  "Copy text between point and mark to a new buffer whose major mode is MODE.

The copied lines in the original region may all start with PREFIX
and/or end with SUFFIX; in that case, these prefixes and suffixes
of every line are stripped.  PREFIX and SUFFIX may contain
preceding and/or trailing whitespace characters; if a line does
not start with PREFIX and/or end with SUFFIX because of the
preceding and/or trailing whitespace characters, it is still
treated as starting with PREFIX and/or ending with SUFFIX, and
the prefix and/or suffix are stripped in the new buffer as well.

The new buffer is for editing the original region in MODE.
Later, changes in the new buffer could be copied back into the
original buffer with `subedit-commit', and the stripped prefix
and suffix (if exist) will be added back to every line."
  (interactive
   (let ((prefix
	  (read-string (if subedit-line-prefix-history
			   (format "Line prefix (default \"%s\"): "
				   (car subedit-line-prefix-history))
			 "Line prefix: ")
		       nil
		       'subedit-line-prefix-history
		       (if subedit-line-prefix-history
			   (car subedit-line-prefix-history))))
	 (suffix
	  (read-string (if subedit-line-suffix-history
			   (format "Line suffix (default \"%s\"): "
				   (car subedit-line-suffix-history))
			 "Line suffix: ")
		       nil
		       'subedit-line-suffix-history
		       (if subedit-line-suffix-history
			   (car subedit-line-suffix-history))))
	 (mode
	  (completing-read (if subedit-editing-mode-history
			       (format "Edit using mode (default \"%s\"): "
				       (car subedit-editing-mode-history))
			     "Edit using mode: ")
			   obarray
			   #'(lambda (symbol)
			       (and (commandp symbol t)
				    (string-match "-mode$"
						  (symbol-name symbol))))
			   t nil
			   'subedit-editing-mode-history
			   (if subedit-editing-mode-history
			       (car subedit-editing-mode-history)))))
     (list (region-beginning) (region-end) prefix suffix mode)))
  (let* ((bufid (prog1 subedit-new-buffer-id
		  (setq subedit-new-buffer-id (1+ subedit-new-buffer-id))))
	 (new-buffer (get-buffer-create (format "%s-subedit-%d"
						(buffer-name)
						bufid)))
	 (original-buffer (current-buffer))
	 (original-text (buffer-substring-no-properties beg end))
	 (prefix-1 (concat "^" (regexp-quote prefix)))
	 (prefix-2 (replace-regexp-in-string "\\s-+$" "" prefix-1))
	 (prefix-3 (replace-regexp-in-string "^\\^\\s-+" "^" prefix-2))
	 (suffix-1 (concat (regexp-quote suffix) "$"))
	 (suffix-2 (replace-regexp-in-string "\\s-+\\$$" "$" suffix-1))
	 (suffix-3 (replace-regexp-in-string "^\\s-+" "" suffix-2))
	 (beg-marker (save-excursion (goto-char beg)
				     (point-marker)))
	 (end-marker (save-excursion (goto-char end)
				     (point-marker))))
    ;; Setup the new buffer.
    (with-current-buffer new-buffer
      (insert original-text)
      ;; Strip line prefixes and/or line suffixes, if any.
      (goto-char (point-min))
      (when (and prefix (not (string= prefix "")))
	(while (or (re-search-forward prefix-1 (line-end-position) t)
		   (re-search-forward prefix-2 (line-end-position) t)
		   (re-search-forward prefix-3 (line-end-position) t))
	  (replace-match "" nil nil)
	  (forward-line 1))
	(unless (progn (forward-line 1)
		       (eobp))
	  (kill-buffer new-buffer)
	  (error (format "Lines in region are not all starting with \"%s\""
			 prefix)))
	(goto-char (point-min)))
      (when (and suffix (not (string= suffix "")))
	(while (or (re-search-forward suffix-1 (line-end-position) t)
		   (re-search-forward suffix-2 (line-end-position) t)
		   (re-search-forward suffix-3 (line-end-position) t))
	  (replace-match "" nil nil)
	  (forward-line 1))
	(unless (progn (forward-line)
		       (eobp))
	  (kill-buffer new-buffer)
	  (error (format "Lines in region are not all end with \"%s\""
			 suffix)))
	(goto-char (point-min)))
      (delete-trailing-whitespace)
      ;; The new buffer should be considered as not modified.
      (set-buffer-modified-p nil)
      (command-execute (intern mode))
      ;; Backup some variables.
      (setq subedit-line-prefix prefix
	    subedit-line-suffix suffix
	    subedit-original-buffer original-buffer
	    subedit-original-region-beginning-marker beg-marker
	    subedit-original-region-end-marker end-marker))
    ;; Arrange to show the new buffer friendly.
    (setq subedit-old-window-configuration (current-window-configuration))
    (split-window)
    (other-buffer)
    (switch-to-buffer new-buffer)))

;;;###autoload
(defun subedit-commit ()
  "Copy changes to current buffer back into its original buffer.
This will kill current buffer too."
  (interactive)
  (unless subedit-original-buffer
    (error "Current buffer has no original buffer"))
  (let ((original-buffer subedit-original-buffer))
    (when (buffer-modified-p)
      ;; Copy the changes back into its original buffer.
      (let ((beg subedit-original-region-beginning-marker)
	    (end subedit-original-region-end-marker)
	    (prefix subedit-line-prefix)
	    (suffix subedit-line-suffix)
	    (new-text (buffer-substring-no-properties (point-min)
						      (point-max))))
	(with-current-buffer original-buffer
	  ;; Replace the old content with the new one.
	  (delete-region beg end)
	  (goto-char beg)
	  (insert new-text)
	  ;; Add the stripped prefixes and suffixes back, if any.
	  (when (or prefix suffix)
	    (save-restriction
	      (narrow-to-region beg (point))
	      (goto-char (point-min))
	      (while (progn (re-search-forward "^")
			    (replace-match prefix)
			    (re-search-forward "$")
			    (replace-match suffix)
			    (= (forward-line 1) 0)))
	      (delete-trailing-whitespace))))))
    (kill-buffer (current-buffer))
    ;; Recover the old window-configuration.
    (with-current-buffer original-buffer
      (set-window-configuration subedit-old-window-configuration)
      (setq subedit-old-window-configuration (current-window-configuration)))))

(provide 'subedit)

;;; subedit.el ends here

