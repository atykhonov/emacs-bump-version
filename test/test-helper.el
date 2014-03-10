(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)


(defvar bump-version-test/test-path
  (if (null load-file-name)
      (f-dirname (buffer-file-name))
    (f-dirname load-file-name)))

(defvar bump-version-test/fixtures-path
  (f-expand "fixtures" bump-version-test/test-path))

(defvar bump-version-test/root-path
  (f-parent bump-version-test/test-path))

(setq debug-on-entry t)
(setq debug-on-error t)

(add-to-list 'load-path bump-version-test/root-path)

(require 'bump-version
         (f-expand "bump-version"
                   bump-version-test/root-path))

(defun load-fixture-config ()
  (with-temp-buffer
    (insert-file-contents (concat bump-version-test/fixtures-path
                                  "/"
                                  bump-version-config-file))
    (read (buffer-string))))

(defun reset-fixtures ()
  (copy-file (concat bump-version-test/fixtures-path
                     "/.bump-version-draft.el")
             (concat bump-version-test/fixtures-path
                     "/.bump-version.el")
             t)
  (copy-file (concat bump-version-test/fixtures-path
                     "/Cask-draft.el")
             (concat bump-version-test/fixtures-path
                     "/Cask")
             t)
  (copy-file (concat bump-version-test/fixtures-path
                     "/emacs-lisp-draft.el")
             (concat bump-version-test/fixtures-path
                     "/emacs-lisp.el")
             t)
  (copy-file (concat bump-version-test/fixtures-path
                     "/pkg-draft.el")
             (concat bump-version-test/fixtures-path
                     "/pkg.el")
             t))
