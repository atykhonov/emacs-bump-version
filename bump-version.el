;;; bump-version.el --- Emacs package which helps bump version.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/emacs-bump-version
;; Version: 0.1.3
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:

(require 'cl)


(defvar bump-version-format-string "%s.%s.%s")

(defvar bump-version-emacs-lisp-version-str ";; VERSION: ")



(defun bump-version--patch (version)
  (format bump-version-format-string
          (bump-version--major-num version)
          (bump-version--minor-num version)
          (+ (bump-version--patch-num version)
             1)))

(defun bump-version--minor (version)
  (format bump-version-format-string
          (bump-version--major-num version)
          (+ (bump-version--minor-num version)
             1)
          0))

(defun bump-version--major (version)
  (format bump-version-format-string
          (+ (bump-version--major-num version)
             1)
          0 0))

(defun bump-version--version-to-list (version)
  (mapcar
   (lambda (n)
     (string-to-int n))
   (split-string version "\\.")))

(defun bump-version--major-num (version)
  (bump-version--num version 0))

(defun bump-version--minor-num (version)
  (bump-version--num version 1))

(defun bump-version--patch-num (version)
  (bump-version--num version 2))

(defun bump-version--num (version idx)
  (nth idx (bump-version--version-to-list version)))

(defun bump-version-patch ()
  (interactive)
  (bump-version-with-config 'bump-version--patch))

(defun bump-version-minor ()
  (interactive)
  (bump-version-with-config 'bump-version--minor))

(defun bump-version-major ()
  (interactive)
  (bump-version-with-config 'bump-version--major))

(defun bump-version-with-config (bump-func)
  (let* ((files (bump-version--files-to-bump))
         (current-version (bump-version--current-version))
         (next-version (funcall bump-func current-version)))
  (dolist (file files)
    (let* ((file (concat default-directory file))
           (current-versiont (bump-version--current-version))
           (next-version (funcall bump-func current-version)))
      (with-temp-file file
        (insert-file-contents file)
        (while (search-forward current-version nil t)
          (replace-match next-version nil t)))))))

(defun bump-version--read-config ()
  (with-temp-buffer
    (insert-file-contents (concat default-directory "/.bump-version.el"))
    (read (buffer-string))))

(defun bump-version--files-to-bump ()
  (let* ((config (bump-version--read-config))
         (files (car (cdr (assoc-string :files config)))))
    (cons ".bump-version.el" files)))

(defun bump-version--current-version ()
  (let ((config (bump-version--read-config)))
    (car (cdr (assoc-string :current-version config)))))

(defun bump-version-current-buffer (bump-func)
  (let* ((file (buffer-file-name))
         (ext (file-name-extension file)))
    (cond ((equal ext "el")
           (bump-version-emacs-lisp bump-func)))))

(defun bump-version-emacs-lisp (&optional bump-func)
  (interactive)
  (save-excursion
    (with-current-buffer (current-buffer)
      (goto-char (point-min))
      (when (search-forward
             bump-version-emacs-lisp-version-str
             nil t)
        (let ((start (point))
              (bumped-version ""))
          (end-of-line)
          (setq bumped-version
                (funcall bump-func
                         (buffer-substring-no-properties start (point))))
          ;; strip string
          (goto-char start)
          (kill-and-join-forward)
          (insert bumped-version)
          (save-buffer))))))


(provide 'bump-version)

;;; bump-version.el ends here
