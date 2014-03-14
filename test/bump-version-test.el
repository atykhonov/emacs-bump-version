(ert-deftest test-bump-version--major-zero ()
  (should (equal
           0
           (bump-version--major-version "0.20.44"))))

(ert-deftest test-bump-version--major-none-zero ()
  (should (equal
           27
           (bump-version--major-version "27.20.44"))))

(ert-deftest test-bump-version--minor-zero ()
  (should (equal
           0
           (bump-version--minor-version "27.0.44"))))

(ert-deftest test-bump-version--minor-none-zero ()
  (should (equal
           61
           (bump-version--minor-version "27.61.44"))))

(ert-deftest test-bump-version--patch-zero ()
  (should (equal
           0
           (bump-version--patch-level "27.61.0"))))

(ert-deftest test-bump-version--patch-none-zero ()
  (should (equal
           129
           (bump-version--patch-level "27.61.129"))))

(ert-deftest test-bump-version--patch-1 ()
  (should (string-equal
           "0.0.2"
           (bump-version--patch "0.0.1"))))

(ert-deftest test-bump-version--patch-2 ()
  (should (string-equal
           "0.0.204"
           (bump-version--patch "0.0.203"))))

(ert-deftest test-bump-version--minor-1 ()
  (should (string-equal
           "0.1.0"
           (bump-version--minor "0.0.20"))))

(ert-deftest test-bump-version--minor-2 ()
  (should (string-equal
           "20.221.0"
           (bump-version--minor "20.220.20"))))

(ert-deftest test-bump-version--major-1 ()
  (should (string-equal
           "21.0.0"
           (bump-version--major "20.221.20"))))

(ert-deftest test-bump-version--major-2 ()
  (should (string-equal
           "1.0.0"
           (bump-version--major "0.221.20"))))

(ert-deftest test-bump-version--version-to-list ()
  (should (equal
           (list 1 2 3)
           (bump-version--version-to-list "1.2.3"))))

(ert-deftest test-bump-version--read-config ()
  (reset-fixtures)
  (with-mock
   (stub bump-version--get-default-directory =>
         (concat bump-version-test/fixtures-path))
   (should (equal
            (bump-version--read-config)
            '((:files
               ("Cask"
                "emacs-lisp.el"
                "pkg.el"))
              (:current-version "1.6.9"))))))

(ert-deftest test-bump-version--config-property ()
  (with-mock
   (stub bump-version--read-config => '((:files ("foo" "bar" "baz"))))
   (should (equal
            (bump-version--config-property ":files")
            '("foo" "bar" "baz")))))

(ert-deftest test-bump-version--files-to-bump ()
  (reset-fixtures)
  (with-mock
   (stub bump-version--get-default-directory =>
         (concat bump-version-test/fixtures-path))
   (should
    (equal
     (bump-version--files-to-bump)
     '(".bump-version.el"
       "Cask"
       "emacs-lisp.el"
       "pkg.el")))))

(ert-deftest test-bump-version--current-version ()
  (reset-fixtures)
  (with-mock
   (stub bump-version--get-default-directory =>
         (concat bump-version-test/fixtures-path))
   (should
    (equal
     (bump-version--current-version)
     "1.6.9"))))

(ert-deftest test-bump-version-with-config ()
  (reset-fixtures)
  (setq current-version (bump-version--current-version))
  (setq next-version (bump-version--minor current-version))
  (bump-version-with-config 'bump-version--minor)
  (with-temp-buffer
    (insert-file-contents (concat default-directory "/Cask"))
    (goto-char (point-min))
    (search-forward next-version)))

(ert-deftest test-bump-version--find-config-base-dir/child-dir ()
  (with-mock
   (stub bump-version--get-default-directory =>
         (concat bump-version-test/fixtures-path "/" "test" "/"))
   (should (equal (bump-version--find-config-base-dir)
                  (concat bump-version-test/fixtures-path "/")))))

;;; bump-version-test.el ends here
