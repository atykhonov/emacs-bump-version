;;; bump-version.el --- Emacs package which helps bump version.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/emacs-bump-version
;; Version: 0.2.1
;; Keywords: tool

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

;; A little tool which may help to bump version for your projects files.

;;; Installation

;; Assuming that you cloned emacs-bump-version to the
;; `~/.emacs.d/bump-version/' folder. Add the following lines to your
;; `.emacs' file:
;;
;; (add-to-list 'load-path "~/.emacs.d/bump-version/")
;; (require 'bump-version)
;; (global-set-key (kbd "C-c C-b p") 'bump-version-patch)
;; (global-set-key (kbd "C-c C-b i") 'bump-version-minor)
;; (global-set-key (kbd "C-c C-b m") 'bump-version-major)
;;

;;; Configuration

;;
;; Add `.bump-version.el' file to your project root directory. For
;; example, for `bump-version' project it looks like the following:
;;
;; ((:files
;;   ("Cask"
;;    "bump-version.el"
;;    "bump-version-pkg.el"))
;;  (:current-version "0.2.1"))
;;
;; Then you can interactively call `bump-version-patch',
;; `bump-version-minor' or `bump-version-major' commands and a version
;; will be updated for the specified files (also for the
;; `.bump-version.el' file).

;;; Contribution

;; Contribution is much welcome! When adding new features, please write tests for them!

;; Thank you! And Enjoy!

;;; Code:

(require 'cl)


(defvar bump-version-format-string "%s.%s.%s")

(defvar bump-version-config-file ".bump-version.el")



(defun bump-version--patch (version)
  (format bump-version-format-string
          (bump-version--major-version version)
          (bump-version--minor-version version)
          (+ (bump-version--patch-level version)
             1)))

(defun bump-version--minor (version)
  (format bump-version-format-string
          (bump-version--major-version version)
          (+ (bump-version--minor-version version)
             1)
          0))

(defun bump-version--major (version)
  (format bump-version-format-string
          (+ (bump-version--major-version version)
             1)
          0 0))

(defun bump-version--version-to-list (version)
  (mapcar
   (lambda (n)
     (string-to-int n))
   (split-string version "\\.")))

(defun bump-version--major-version (version)
  (bump-version--version version 0))

(defun bump-version--minor-version (version)
  (bump-version--version version 1))

(defun bump-version--patch-level (version)
  (bump-version--version version 2))

(defun bump-version--version (version idx)
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
    (setq file (concat default-directory file))
    (with-temp-file file
      (insert-file-contents file)
      (while (search-forward current-version nil t)
        (replace-match next-version nil t))))))

(defun bump-version--read-config ()
  (with-temp-buffer
    (insert-file-contents (concat default-directory "/" bump-version-config-file))
    (read (buffer-string))))

(defun bump-version--config-property (property)
  (let ((config (bump-version--read-config)))
    (car (cdr (assoc-string property config)))))

(defun bump-version--files-to-bump ()
  (let ((files (bump-version--config-property ":files")))
    (cons bump-version-config-file files)))

(defun bump-version--current-version ()
  (bump-version--config-property ":current-version"))


(provide 'bump-version)

;;; bump-version.el ends here
