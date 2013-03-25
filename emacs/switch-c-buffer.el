;;; SWITCH-C-BUFFER.EL --- quick switch from buffer name.c to buffer name.h and vice versa

;; Copyright (C) 1995 Dimitry Kloper

;; Author: Dimitry Kloper <dimka@tochna1.technion.ac.il> "Dimitry KLoper"
;; Maintainer: Dimitry Kloper <dimka@tochna1.technion.ac.il> "Dimitry KLoper"
;; Created: 09 Oct 1995
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <dimka@tochna1.technion.ac.il> "Dimitry KLoper") or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; switch-c-buffer|Dimitry Kloper|<dimka@tochna1.technion.ac.il> "Dimitry KLoper"
;; |quick switch from buffer name.c to buffer name.h and vice versa
;; |$Date: 2001/01/30 16:39:11 $|$Revision: 1.1 $|~/packages/switch-c-buffer.el

;;; Commentary:

;;; Change log:
;; $Log: switch-c-buffer.el,v $
;; Revision 1.1  2001/01/30 16:39:11  dimka
;; Some old and modified packages
;;
;; Revision 1.3  1995/10/09 18:56:53  dimka
;; Now error is generated if can't find buffer
;;
; Revision 1.2  1995/10/09  18:24:40  dimka
; First working versionn . Variable switch-c-list has been defined
; it associates suffixes with target ones
;
; Revision 1.1  1995/10/09  16:57:18  dimka
; Initial revision
;

;;; Variables:


(defvar switch-c-alist '((".h" . ".c")
			 (".c" . ".h"))
  "Association list containing what must be substituted for each suffix
   in buffer name")
   
			 
(defconst switch-c-buffer-version (substring "$Revision: 1.1 $" 11 -2)
  "$Id: switch-c-buffer.el,v 1.1 2001/01/30 16:39:11 dimka Exp $

Report bugs to: Dimitry Kloper <dimka@tochna1.technion.ac.il>")

;;; Code:

(defun switch-c-buffer () 
  "Change current buffer to corresponding by name suffix"

  (interactive)
  (let ((suffix nil)
	(bname (buffer-name))
	(newbname nil))

    (if (local-variable-p 'switch-c-buffer-name)
	(setq newbname switch-c-buffer-name)
      (progn
	(if (string-match "\\.[a-zA-Z]*$" bname)
	    (setq suffix (cdr (assoc (match-string 0 bname) switch-c-alist))))
	(if suffix
	    (setq newbname (concat 
			    (substring bname 0 (match-beginning 0)) suffix)))))
    (if newbname
	(if (get-buffer newbname)
	    (switch-to-buffer newbname)
	  (error "No such buffer %s" newbname)))
      
    )
  )


;;; SWITCH-C-BUFFER.EL ends here

