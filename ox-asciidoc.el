;;; ox-asciidoc.el --- AsciiDoc Back-End for Org Export Engine

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Yasushi SHOJI <yasushi.shoji@gmail.com>
;; Keywords: org, asciidoc

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an AsciiDoc back-end for Org exporter.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-asciidoc-export-as-asciidoc' (temporary buffer) and
;; `org-asciidoc-export-to-asciidoc' (file).

;;; Code:
(require 'ox)

(defgroup org-export-asciidoc nil
  "Options for exporting Org mode files to Asciidoc."
  :tag "Org Export Asciidoc"
  :group 'org-export)

(org-export-define-backend 'asciidoc
  '((babel-call . org-asciidoc-identity)
    (bold . org-asciidoc-bold)
    (center-block . org-asciidoc-identity)
    (clock . org-asciidoc-identity)
    (code . org-asciidoc-code)
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
    (diary-sexp . org-asciidoc-identity)
    (drawer . (lambda (&rest args) ""))
    (dynamic-block . org-asciidoc-identity)
    (entity . org-asciidoc-identity)
    (example-block . org-asciidoc-example-block)
    (fixed-width . org-asciidoc-identity)
    (footnote-definition . org-asciidoc-identity)
    (footnote-reference . org-asciidoc-identity)
    (headline . org-asciidoc-headline)
    (horizontal-rule . org-asciidoc-identity)
    (inline-babel-call . org-asciidoc-identity)
    (inline-src-block . org-asciidoc-identity)
    (inlinetask . org-asciidoc-identity)
    (italic . org-asciidoc-italic)
    (item . org-asciidoc-item)
    (keyword . org-asciidoc-identity)
    (latex-environment . org-asciidoc-identity)
    (latex-fragment . org-asciidoc-identity)
    (line-break . org-asciidoc-identity)
    (link . org-asciidoc-identity)
    (node-property . org-asciidoc-identity)
    (paragraph . org-asciidoc-identity)
    (plain-list . org-asciidoc-plain-list)
    (planning . org-asciidoc-identity)
    (property-drawer . (lambda (&rest args) ""))
    (quote-block . org-asciidoc-identity)
    (quote-section . org-asciidoc-identity)
    (radio-target . org-asciidoc-identity)
    (section . org-asciidoc-identity)
    (special-block . org-asciidoc-identity)
    (src-block . org-asciidoc-identity)
    (statistics-cookie . org-asciidoc-identity)
    (strike-through . org-asciidoc-strike-through)
    (subscript . org-asciidoc-identity)
    (superscript . org-asciidoc-identity)
    (table . org-asciidoc-identity)
    (table-cell . org-asciidoc-identity)
    (table-row . org-asciidoc-identity)
    (target . org-asciidoc-identity)
    (timestamp . org-asciidoc-identity)
    (underline . org-asciidoc-underline)
    (verbatim . org-asciidoc-verbatim)
    (verse-block . org-asciidoc-identity))
  :options-alist '((:headline-levels nil nil 4 t))
  :menu-entry
  '(?a "Export to Asciidoc"
       ((?a "As Asciidoc buffer"
	    (lambda (a s v b) (org-asciidoc-export-as-asciidoc a s v)))
	(?A "As Asciidoc file"
	    (lambda (a s v b) (org-asciidoc-export-to-asciidoc a s v)))
	(?o "As Asciidoc file and open"
	    (lambda (a s v b)
	      (if a (org-asciidoc-export-to-asciidoc t s v)
		(org-open-file (org-asciidoc-export-to-asciidoc nil s v))))))))


(defun org-asciidoc-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (org-export-expand blob contents))


;;; Inline Text Format
(defun org-asciidoc-bold (bold contents info)
  (concat "*" contents "*"))

(defun org-asciidoc-code (code contents info)
  (concat "+" (org-element-property :value code) "+"))

(defun org-asciidoc-italic (italic contents info)
  (concat "'" contents "'"))

(defun org-asciidoc-strike-through (strike-through contents info)
  (concat "[line-through]#" contents "#"))

(defun org-asciidoc-underline (underline contents info)
  (concat "[underline]#" contents "#"))

(defun org-asciidoc-verbatim (verbatim contents info)
  (concat "+" (org-element-property :value verbatim) "+"))


;;; Head Line
(defun org-asciidoc-headline (headline contents info)
  "Transcode HEADLINE element into AsciiDoc format.
CONTENTS is the headline contents."
  (let* ((level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :title headline) info))
	 (limit (plist-get info :headline-levels)))
    (if (org-export-low-level-p headline info)
	(concat (make-string (- level limit) ?*) " " title "\n" contents)
      (let ((delimiter (make-string (1+ level) ?=)))
	(concat "\n" delimiter " " title " " delimiter "\n" contents)))))


;;; List
(defun org-asciidoc-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element into AsciiDoc format.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)

(defun org-asciidoc-item-list-depth (item)
  (let ((parent item)
	(depth 0))
    (while (and (setq parent (org-export-get-parent parent))
		(cl-case (org-element-type parent)
		  (item t)
		  (plain-list (cl-incf depth)))))
    depth))

(defun org-asciidoc-list-item-delimiter (item)
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (depth (org-asciidoc-item-list-depth item)))
    (case type
      (unordered
       (make-string depth ?*))
      (ordered
       (make-string depth ?.)))))

(defun org-asciidoc-item (item contents info)
  "Transcode an ITEM element into AsciiDoc format.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "%s %s" (org-asciidoc-list-item-delimiter item) contents))


;;; Example Block
(defun org-asciidoc-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element into AsciiDoc format.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-element-property :value example-block)))
    (concat "----\n" value "----")))


;;;###autoload
(defun org-asciidoc-export-as-asciidoc (&optional async subtreep visible-only)
  "Export current buffer to a buffer in Asciidoc format.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org ASCIIDOC Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org ASCIIDOC Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (text-mode)
	      (org-export-add-to-stack (current-buffer) 'asciidoc)))
	`(org-export-as 'asciidoc ,subtreep ,visible-only))
    (let ((outbuf
	   (org-export-to-buffer
	    'asciidoc "*Org ASCIIDOC Export*" subtreep visible-only)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-asciidoc-export-to-asciidoc (&optional async subtreep visible-only)
  "Export current buffer to a Asciidoc file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep)))
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'org))
	  `(expand-file-name
	    (org-export-to-file
	     'asciidoc ,outfile ,subtreep ,visible-only)))
      (org-export-to-file 'asciidoc outfile subtreep visible-only))))

(provide 'ox-asciidoc)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-asciidoc.el ends here
