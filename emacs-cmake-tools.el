(require 'projectile)
(require 'json)

;;; Code:
;;; Path to cmake binary
(defcustom ect/cmake-binary "cmake"
  "Path of the binary used for cmake."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the build directory prefix
;;; Currently only supports a relative path inside the project directory
(defcustom ect/cmake-build-directory-prefix "build"
  "Prefix to the relative path inside the project directory."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Toggle to enable generating compilation database
(defcustom ect/cmake-generate-compilation-database nil
  "Toggle to generate compilation database."
  :type 'boolean
  :group 'emacs-cmake-tools)

;;; Variable to set configuration arguments for cmake
(defcustom ect/cmake-configure-arguments ""
  "Variable to override the cmake configure arguments."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the build arguments for cmake
(defcustom ect/cmake-build-arguments ""
  "Variable to override the cmake build arguments."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the generator for cmake
(defcustom ect/cmake-generator "Makefile"
  "Variable to set the generator for cmake."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to represent the different build types
(defcustom ect/cmake-build-types '("Release" "Debug" "RelWithDebInfo")
  "List of available build types."
  :type '(repeat strings)
  :group 'emacs-cmake-tools)

(defcustom ect/cmake-targets '("All")
  "List of targets for the project."
  :type '(repeat strings)
  :group 'emacs-cmake-tools)

(defcustom ect/cmake-current-target "All"
  "Current target to build."
  :type 'string
  :group 'emacs-cmake-tools)

(defcustom  ect/local-cmake-build-type "Release"
  "Current build type."
  :type 'string
  :group 'emacs-cmake-tools
  )

(defun ect/cmake-generate-configure-command (build-directory)
  "Helper function for generating the configure command."
  (setq configure_cmd (concat ect/cmake-binary " -S " (projectile-project-root) " -B " build-directory " -DCMAKE_BUILD_TYPE=" ect/local-cmake-build-type))
  configure_cmd)

(defun ect/cmake-generate-build-command (build-directory)
  "Helper function to gernrate the build command."
  (setq build_cmd (concat ect/cmake-binary " --build " build-directory))
  build_cmd)

(defun ect/cmake-choose-build-type ()
  "Helper function to choose the build type."
  (interactive)
  (setq ect/local-cmake-build-type (completing-read "Choose build type :" ect/cmake-build-types))
  )

(defun ect/cmake-build-target ()
  "Helper function to narrow down the build to a specific target."
  (interactive)
  (setq ect/cmake-current-target (completing-read "Choose targbet :" ect/cmake-targets))
  )

(defun ect/cmake-generate-write-query-file (build-directory)
  "Helper function which will generate the query file for cmake."
  (setq cmake-query-directory (concat build-directory "/.cmake/api/v1/query/client-emacs-cmake-tools/"))
  (make-directory cmake-query-directory t)
  (setq cmake-query-file (concat cmake-query-directory "/query.json"))
  (setq query '(("requests" . (
                               (
                                ("kind" . "codemodel")
                                ("version" . 2))
                               ))))
  (setq json-cmake-query-string (json-encode query))
  (message "writing query file to : %s" cmake-query-file)
  (write-region json-cmake-query-string nil cmake-query-file)
  )

(defun ect/cmake-configure-project ()
  "Configure cmake project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (setq build-directory-suffix (downcase ect/local-cmake-build-type))
    (setq build-directory (concat ect/cmake-build-directory-prefix "-" build-directory-suffix))
    (setq cmake-build-directory (concat (projectile-project-root) "/" build-directory))
    (setq configure_cmd (ect/cmake-generate-configure-command build-directory))
    (ect/cmake-generate-write-query-file cmake-build-directory)
    (compile configure_cmd)
    (ect/cmake-api-output build-directory)))


(defun ect/find-latest-file-by-pattern (directory pattern)
  "Find the latest file in DIRECTORY matching the given pattern."
  (let ((files (directory-files directory t pattern))
        (latest-file nil)
        (latest-time 0))
    (dolist (file files)
      (when (and (file-readable-p file)
                 (not (file-directory-p file)))
        (let ((mtime (nth 5 (file-attributes file))))
          (when mtime
            (let* ((mtime-list (decode-time mtime))
                   (mtime-sec (nth 0 mtime-list)))
              (when (> mtime-sec latest-time)
                (setq latest-time mtime-sec)
                (setq latest-file file)))))))
    latest-file))

(defun ect/cmake-api-output (build-directory)
  "Function to parse the output of cmake api"
  (setq api_file (ect/find-latest-file-by-pattern (concat build-directory "/.cmake/api/v1/reply") "^index.*\\.json$"))
  (with-temp-buffer
    (insert-file-contents api_file)
    (setq cmake_index (json-parse-buffer :object-type 'hash-table)))

  (setq ect_cm (gethash "jsonFile" (aref (gethash "responses"
                                                  (gethash "query.json"
                                                           (gethash "client-emacs-cmake-tools"
                                                                    (gethash "reply" cmake_index)))) 0)))


  (setq ect_cm_path (concat build-directory "/.cmake/api/v1/reply/" ect_cm))
  (with-temp-buffer
    (insert-file-contents ect_cm_path)
    (setq ect_cm_contents (json-parse-buffer :object-type 'hash-table)))

  (setq ect_targets (gethash "targets" (aref (gethash "configurations" ect_cm_contents) 0)))
  (setq targets '("All"))
  (dotimes (i (length ect_targets))
    (setq target (aref ect_targets i))
    (setq targets (append targets (list (gethash "name" target))))
    )

  (setq ect/cmake-targets targets)
  (message "Cmake targets : %s" ect/cmake-targets)
  )

(defun ect/cmake-build-project ()
  "build the project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (setq build-directory-suffix (downcase ect/local-cmake-build-type))
    (setq build-directory (concat ect/cmake-build-directory-prefix "-" build-directory-suffix))
    (setq cmake-build-directory (concat (projectile-project-root) "/" build-directory))
    (setq build_cmd (ect/cmake-generate-build-command build-directory))
    (if (not (string-equal ect/cmake-current-target "All"))
        (progn
          (setq build_cmd (concat build_cmd " --target " ect/cmake-current-target))
          (message "Updated target : %s " build_cmd))
      )
    (compile build_cmd))
  )


(provide 'emacs-cmake-tools)
;;; emacs-cmake-tools ends here
