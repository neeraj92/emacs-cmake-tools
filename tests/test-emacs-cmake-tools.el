(require 'ert)
;; Add parent directory to load-path to find emacs-cmake-tools.el
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'emacs-cmake-tools)
(require 'cl-lib) ;; For cl-every

;; --- Helper Functions ---
(defun command-contains-all (cmd substrings)
  "Return t if CMD contains all SUBSTRINGS."
  (cl-every (lambda (s) (string-match-p (regexp-quote s) cmd)) substrings))

(defun command-contains-none (cmd substrings)
  "Return t if CMD contains none of SUBSTRINGS."
  (cl-every (lambda (s) (not (string-match-p (regexp-quote s) cmd))) substrings))

(defun create-temp-file (dir name &optional content)
  "Create a temporary file in DIR with NAME and optional CONTENT."
  (let ((file-path (expand-file-name name dir)))
    (with-temp-file file-path
      (when content
        (insert content)))
    file-path))

;; --- Test Suites ---

;; Test suite for ect/find-preset-file
(ert-deftest test-find-preset-only-cmake-presets ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (progn
          (create-temp-file temp-dir ect/cmake-preset-file-name "{}")
          (should (string-equal (file-name-nondirectory (ect/find-preset-file temp-dir)) ect/cmake-preset-file-name)))
      (delete-directory temp-dir t))))

(ert-deftest test-find-preset-only-cmake-user-presets ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (progn
          (create-temp-file temp-dir ect/cmake-user-preset-file-name "{}")
          (should (string-equal (file-name-nondirectory (ect/find-preset-file temp-dir)) ect/cmake-user-preset-file-name)))
      (delete-directory temp-dir t))))

(ert-deftest test-find-preset-both-prefer-user ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (progn
          (create-temp-file temp-dir ect/cmake-preset-file-name "{}")
          (create-temp-file temp-dir ect/cmake-user-preset-file-name "{}")
          (should (string-equal (file-name-nondirectory (ect/find-preset-file temp-dir)) ect/cmake-user-preset-file-name)))
      (delete-directory temp-dir t))))

(ert-deftest test-find-preset-none ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (should (null (ect/find-preset-file temp-dir)))
      (delete-directory temp-dir t))))

;; Test suite for ect/parse-presets
(ert-deftest test-parse-presets-valid ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (let ((temp-file (create-temp-file temp-dir "CMakePresets.json" "{\"version\": 3, \"configurePresets\": [{\"name\": \"preset1\"}, {\"name\": \"preset2\"}]}")))
          (should (equal (ect/parse-presets temp-file) '("preset1" "preset2"))))
      (delete-directory temp-dir t))))

(ert-deftest test-parse-presets-missing-key ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (let ((temp-file (create-temp-file temp-dir "CMakePresets.json" "{\"version\": 3}")))
          (should (null (ect/parse-presets temp-file))))
      (delete-directory temp-dir t))))

(ert-deftest test-parse-presets-malformed-json ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (let ((temp-file (create-temp-file temp-dir "CMakePresets.json" "{\"version\": 3, \"configurePresets\": [")))
          (should (null (ect/parse-presets temp-file))))
      (delete-directory temp-dir t))))

(ert-deftest test-parse-presets-empty-list ()
  (let ((temp-dir (make-temp-directory "ect-test-")))
    (unwind-protect
        (let ((temp-file (create-temp-file temp-dir "CMakePresets.json" "{\"version\": 3, \"configurePresets\": []}")))
          (should (equal (ect/parse-presets temp-file) '())))
      (delete-directory temp-dir t))))

(ert-deftest test-parse-presets-nil-input ()
  (should (null (ect/parse-presets nil))))

;; Test suite for ect/cmake-generate-configure-command
(ert-deftest test-generate-command-preset-mode ()
  (let ((ect/cmake-use-presets t)
        (ect/local-cmake-build-type "my-preset")
        (ect/cmake-binary "cmake_bin")
        (ect/cmake-source-directory "/path/to/source")
        (ect/cmake-build-directory-prefix "build") ; Used by ect/cmake-configure-project, not directly by generate
        (ect/project-cmake-configure-args "--extra-arg"))
    (let ((cmd (ect/cmake-generate-configure-command "build-my-preset"))) ; build-directory is passed as arg
      (should (command-contains-all cmd '("cmake_bin" "--preset my-preset" "-S /path/to/source" "-B build-my-preset" "--extra-arg")))
      (should (command-contains-none cmd '("-DCMAKE_BUILD_TYPE=" "-G "))))))

(ert-deftest test-generate-command-normal-mode ()
  (let ((ect/cmake-use-presets nil)
        (ect/local-cmake-build-type "Debug")
        (ect/cmake-current-generator "Ninja")
        (ect/cmake-binary "cmake_exec")
        (ect/cmake-source-directory "/src")
        (ect/cmake-build-directory-prefix "build") ; Used by ect/cmake-configure-project
        (ect/project-cmake-configure-args "-DVAR=ON"))
    (let ((cmd (ect/cmake-generate-configure-command "build-debug"))) ; build-directory is passed as arg
      (should (command-contains-all cmd '("cmake_exec" "-S /src" "-B build-debug" "-DCMAKE_BUILD_TYPE=Debug" "-G Ninja" "-DVAR=ON")))
      (should (command-contains-none cmd '("--preset"))))))

;; End of tests
