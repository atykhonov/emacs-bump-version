;;; bump-version.el --- Emacs package which helps bump version.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/emacs-bump-version
;; Version: 0.1.0
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


(defun bump-version--patch (version)
  (format "%s.%s.%s"
          (bump-version--major-num version)
          (bump-version--minor-num version)
          (+ (bump-version--patch-num version)
             1)))

(defun bump-version--minor (version)
  (format "%s.%s.%s"
          (bump-version--major-num version)
          (+ (bump-version--minor-num version)
             1)
          0))

(defun bump-version--major (version)
  (format "%s.%s.%s"
          (+ (bump-version--major-num version)
             1)
          0 0))

(defun bump-version--version-to-list (version)
  (mapcar
   (lambda (n)
     (string-to-int n))
   (split-string version "\\.")))

(defun bump-version--major-num (version)
  (nth 0 (bump-version--version-to-list version)))

(defun bump-version--minor-num (version)
  (nth 1 (bump-version--version-to-list version)))

(defun bump-version--patch-num (version)
  (nth 2 (bump-version--version-to-list version)))
  

(provide 'bump-version)
