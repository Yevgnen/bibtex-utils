;;; bibtext-utils.el ---  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Utilities to get bibtex and PDF from websites.
;;
;; See documentation on https://github.com/Yevgnen/bibtext-utils.el.

;;; Code:

(require 'org-ref-core)

(defcustom bibtex-utils-bibtex-list
  '(acl
    (:bibtex-url "https://www.aclweb.org/anthology/%s.bib"
                 :pdf-url "https://www.aclweb.org/anthology/%s.pdf"))
  "Definitions of bibtex file fetchers.")

(defcustom bibtex-utils-modify-key-prompt-p
  t
  "Whether to prompt to modify =key= when adding new bibtex.")

(defun bibtex-utils-default-get-bibtex-function (site identifer)
  (lexical-let* ((id identifer))
    (with-current-buffer
        (url-retrieve-synchronously
         (format (bibtex-utils--get-field site :bibtex-url) id)
         t)
      (goto-char (point-min))
      (re-search-forward "@" nil t)
      (string-trim (buffer-substring-no-properties (1- (point)) (point-max))))))

(defun bibtex-utils-default-get-pdf-function (site identifier pdf-file)
  (let* ((pdf-url (format (bibtex-utils--get-field (bibtex-utils--get-params site) :pdf-url) identifier))
         (command (format "curl %s -o %s" pdf-url pdf-file)))
    (lexical-let ((id identifier)
                  (path pdf-file))
      (set-process-sentinel
       (start-process-shell-command
        "sh"
        "*Async Shell Command*"
        command)
       (lambda (process state)
         (if (or (string-match "exited abnormally with code.*" state)
                 (string-match "finished" state))
             (message "PDF saved in %s" path)
           (error "Failed to save pdf: %s" id)))))))

(defun bibtex-utils--get-params (site)
  (plist-get bibtex-utils-bibtex-list site))

(defun bibtex-utils--get-field (params field &optional default)
  (or (plist-get params field)
      default))

(defun bibtex-utils--get-bibtex (site identifier)
  (let ((site (bibtex-utils--get-params site)))
    (funcall
     (bibtex-utils--get-field site :get-bibtex-function #'bibtex-utils-default-get-bibtex-function)
     site
     identifier)))

(defun bibtex-utils--get-pdf-filename (key)
  (expand-file-name
   (format "%s.pdf" key)
   (or (bound-and-true-p bibtex-pdf-directory)
       (bound-and-true-p org-ref-pdf-directory)
       (bound-and-true-p bibtex-completion-library-path)
       (bound-and-true-p biblio-download-directory)
       default-directory)))

(defun bibtex-utils--get-pdf (site identifier pdf-file)
  (funcall
   (bibtex-utils--get-field (bibtex-utils--get-params site) :get-pdf-function #'bibtex-utils-default-get-pdf-function)
   site
   identifier
   pdf-file))

(defun bibtex-utils--write-bibtex (bibtex bibfile)
  (save-window-excursion
    (find-file bibfile)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert bibtex)
    (org-ref-clean-bibtex-entry)
    (save-buffer)
    (bibtex-completion-get-value "=key=" (bibtex-parse-entry))))

(defun bibtex-utils--add-bibtex (site identifier bibfile)
  (let* ((bibtex (bibtex-utils--get-bibtex site identifier))
         (key (bibtex-utils--write-bibtex bibtex bibfile))
         (pdf-file (bibtex-utils--get-pdf-filename key))
         (pdf (bibtex-utils--get-pdf site identifier pdf-file)))))

(provide 'bibtext-utils)

;;; bibtext-utils.el ends here
