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
  (setq default-dir default-directory)
  (with-mock
   (stub default-directory => (concat bump-version-test/config-fixtures-path))
   (stub bump-version-config-file => ".bump-version-1.el")
   (should (equal
            (bump-version--read-config)
            '((:files
               ("Cask"
                "bump-version.el"))
              (:current-version "0.2.1"))))))

(ert-deftest test-bump-version--config-property ()
  (with-mock
   (stub bump-version--read-config => '((:files ("foo" "bar" "baz"))))
   (should (equal
            (bump-version--config-property ":files")
            '("foo" "bar" "baz")))))

(ert-deftest test-bump-version--files-to-bump ()
  (with-mock
   (stub default-directory => (concat bump-version-test/config-fixtures-path))
   (stub bump-version-config-file => ".bump-version-1.el")
   (equal
    (bump-version--files-to-bump)
    '((:files
       ("Cask"
        "bump-version.el"))
      (:current-version "0.2.1")))))

(ert-deftest test-bump-version--current-version ()
  (with-mock
   (stub default-directory => (concat bump-version-test/config-fixtures-path))
   (stub bump-version-config-file => ".bump-version-1.el")
   (equal
    (bump-version--current-version)
    "0.2.1")))
