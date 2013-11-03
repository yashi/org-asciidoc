;;; ox-asciidoc.el --- Test code for ox-asciidoc.el

;; Copyright (C) 2013 Yasushi SHOJI

;; Author: Yasushi SHOJI <yasushi.shoji@gmail.com>
;; Keywords: org, asciidoc

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is test code for ox-asciidoc.el.
;;
;; You can load this file with `load-file' and run the test with
;; `ert', Emacs Lisp Regression Testing.
;;
;; The test code depends on org-mode/testing/org-test.el.  So, make
;; sure you have org-test.el loaded or have it in your load path.
;; (add-to-list 'load-path "org-mode/testing")
;;
;; Obviously, you should have had ox-asciidoc.el in your load path,
;; too.

;;; Code:
(require 'org-test)
(require 'ox-asciidoc)

(defun org-asciidoc-test-transcode-body (str1 str2)
  (should (equal (org-test-with-temp-text str1
		   (org-export-as 'asciidoc nil nil t))
		 str2)))

(ert-deftest test-org-asciidoc/list ()
  (org-asciidoc-test-transcode-body
   "- list\n"
   "* list\n")
  (org-asciidoc-test-transcode-body
   "- list\n- list"
   "* list\n* list\n")
  (org-asciidoc-test-transcode-body
   "- list\n  - list"
   "* list\n** list\n")
  (org-asciidoc-test-transcode-body
   "
- list
  - list
    - list
      - list
        - list"
   "* list
** list
*** list
**** list
***** list\n"))