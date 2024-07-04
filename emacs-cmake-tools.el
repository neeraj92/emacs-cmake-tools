(require 'projectile)
(require 'json)
(require 'lsp)

;;; Code:
;;; Path to cmake binary
(defcustom ect/cmake-binary "cmake"
  "Path of the binary used for cmake."
  :type 'string
  :group 'emacs-cmake-tools)

(defcustom ect/ctest-binary "ctest"
  "Path of the binary used for ctest"
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
(defcustom ect/cmake-build-types '("Release" "Debug" "RelWithDebInfo" "ReleaseShared" "DebugShared")
  "List of available build types."
  :type '(repeat strings)
  :group 'emacs-cmake-tools)

(defcustom ect/cmake-generators '("Ninja" "Unix Makefiles")
  "List of supported generators."
  :type '(repeat strings)
  :group 'emacs-cmake-tools)

(defcustom ect/cmake-current-generator "Ninja"
  "Current cmake generator."
  :type 'string
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

(defcustom ect/cmake-source-directory nil
  "Source directory to use for the build."
  :type 'string
  :group 'emacs-cmake-tools)

(defcustom ect/lsp-command-args '("-j=10" "--pch-storage=memory" "--enable-config" "--clang-tidy" "--background-index")
  "lsp command args"
  :type '(repeat strings)
  :group 'emacs-cmake-tools
  )

(defcustom ect/lsp-clangd-binary-path "clangd"
  "lsp binary path"
  :type 'string
  :group 'emacs-cmake-tools)

(defcustom ect/ctest-args '()
  "args for ctest"
  :type '(repeat strings)
  :group 'emacs-cmake-tools
  )

(defvar ect/project-cmake-configure-args ""
  "Custom configuration for cmake configure on the project level.")

(defvar ect/project-cmake-build-args ""
  "Custom configuration for cmake build on the project level.")


(defvar ect/project-config-file ".ect.json"
  "Custom config file for the project which is expected in the project root. If not present in project root, this will be created.")

(defvar ect/project-settings (make-hash-table :test 'equal)
  "Local project settings so that it doesnt have to be configured every time.")

(defun ect/cmake-generate-configure-command (build-directory)
  "Helper function for generating the configure command."

  (setq configure_cmd (concat ect/cmake-binary " -S " ect/cmake-source-directory " -B " build-directory " -DCMAKE_BUILD_TYPE=" ect/local-cmake-build-type " -G " ect/cmake-current-generator " " ect/project-cmake-configure-args))
  configure_cmd)

(defun ect/cmake-generate-build-command (build-directory)
  "Helper function to gernrate the build command."
  (setq build_cmd (concat ect/cmake-binary " --build " build-directory ))
  build_cmd)

(defun ect/ctest-generate-command (build-directory)
  "Helpers function to generate ctest command"
  (setq test_cmd (concat ect/ctest-binary " --test-dir " build-directory  " "  ect/ctest-args))
  test_cmd)

(defun ect/project-settings-file-path ()
  "Function to get the project settings file path"
  (setq path-to-return (concat (projectile-project-root) "/" ect/project-config-file))
  (message "Returning path : %s" path-to-return)
  path-to-return)

(defun ect/cmake-choose-build-type ()
  "Helper function to choose the build type."
  (interactive)
  (setq ect/local-cmake-build-type (completing-read "Choose build type :" ect/cmake-build-types))
  (ect/save-project-settings)
  )

(defun ect/cmake-build-target ()
  "Helper function to narrow down the build to a specific target."
  (interactive)
  (setq ect/cmake-current-target (completing-read "Choose target :" ect/cmake-targets))
  (ect/save-project-settings)
  )

(defun ect/cmake-generator ()
  "Helper function to set the cmake generator value."
  (interactive)
  (setq ect/cmake-current-generator (completing-read "Choose generator :" ect/cmake-generators))
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
    (setq lt-clangd-compilation-commands-dir (concat "--compile-commands-dir=" cmake-build-directory))
    (setq lsp-clients-clangd-args (cons lt-clangd-compilation-commands-dir ect/lsp-command-args))
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
  (message "Opening api file : %s" api_file)
  (with-temp-buffer
    (insert-file-contents api_file)
    (setq cmake_index (json-parse-buffer :object-type 'hash-table)))

  (setq ect_cm (gethash "jsonFile" (aref (gethash "responses"
                                                  (gethash "query.json"
                                                           (gethash "client-emacs-cmake-tools"
                                                                    (gethash "reply" cmake_index)))) 0)))


  (setq ect_cm_path (concat build-directory "/.cmake/api/v1/reply/" ect_cm))
  (message "Opening codemodels file : %s" ect_cm_path)
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
  ;;  (message "Cmake targets : %s" ect/cmake-targets)
  )

(defun ect/cmake-build-project ()
  "Build the project."
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
    (setq build_cmd (concat build_cmd  " " ect/project-cmake-build-args))
    (compile build_cmd))
  )

(defun ect/save-project-settings ()
  "Save the project level settings."
  (interactive)
  (puthash "project-source-directory" ect/cmake-source-directory ect/project-settings)
  (puthash "current-target" ect/cmake-current-target ect/project-settings)
  (puthash "generator" ect/cmake-current-generator ect/project-settings)
  (puthash "build-type" ect/local-cmake-build-type ect/project-settings)
  (puthash "project-cmake-configure-args" ect/project-cmake-configure-args ect/project-settings)
  (puthash "project-cmake-build-args" ect/project-cmake-build-args ect/project-settings)
  (let ((json-project-settings (json-encode ect/project-settings))
        (path-to-save (ect/project-settings-file-path)))
    (message "Writing settings file to path %s" path-to-save)
    (write-region json-project-settings nil path-to-save)
    )
  )

(defun ect/load-project-settings ()
  "Load the project level settings."
  (interactive)
  (let ((path-to-load (ect/project-settings-file-path)))
    (message "Loading project settings from path %s" path-to-load)
    (with-temp-buffer
      (insert-file-contents path-to-load)
      (setq ect/project-settings (json-parse-buffer :object-type 'hash-table)))
    )
  (setq ect/cmake-source-directory (gethash "project-source-directory" ect/project-settings))
  (let ((value (gethash "current-target" ect/project-settings)))
    (when value
      (setq ect/cmake-current-target value)
      t))

  (let ((value (gethash "generator" ect/project-settings)))
    (when value
      (setq ect/cmake-current-generator value)
      t))
  (let ((value (gethash "project-cmake-configure-args" ect/project-settings)))
    (when value
      (setq ect/project-cmake-configure-args value)
      t))
  (let ((value (gethash "project-cmake-build-args" ect/project-settings)))
    (when value
      (setq ect/project-cmake-build-args value)
      t))

  (let ((value (gethash "build-type" ect/project-settings)))
    (when value
      (setq ect/local-cmake-build-type value)
      t))

  )

(defun ect/initialize-ect ()
  "Initialize the project level settings."
  (interactive)
  (let ((file-path (ect/project-settings-file-path)))
    (unless (file-exists-p file-path)
      (message "Creating empty file at path %s" file-path)
      (setq empty-settings (json-encode (make-hash-table :test 'equal)))
      (write-region empty-settings nil file-path)
      ))

  (ect/load-project-settings)
  (if (or (eq ect/cmake-source-directory 'unbound) (null ect/cmake-source-directory))
      (progn
        (message "initializing the project with projectile-project-root")
        (setq ect/cmake-source-directory (projectile-project-root))))
  (setq lsp-clangd-binary-path ect/lsp-clangd-binary-path)
  (message "Setting clangd binary path : %s" lsp-clangd-binary-path)
  )

(defun ect/run-ctest ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (setq build-directory-suffix (downcase ect/local-cmake-build-type))
    (setq build-directory (concat ect/cmake-build-directory-prefix "-" build-directory-suffix))
    (setq cmake-build-directory (concat (projectile-project-root) "/" build-directory))
    (setq test_cmd (ect/ctest-generate-command build-directory))
    (setq test_cmd (concat test_cmd " "  ect/ctest-args ))
    (compile test_cmd))
  )

(defun ect/set-ctest-args (args)
  "Set the arguments to be used for ctest run"
  (interactive "sEnter the arguments: ")
  (setq ect/ctest-args args)
  )

(defun ect/set-project-cmake-configure-args (args)
  "Set the extra arguments to be used for project level configuration."
  (interactive "sCmake Configure Arguments: ")
  (setq ect/project-cmake-configure-args args)
  (ect/save-project-settings)
  )

(defun ect/set-project-cmake-build-args (args)
  "Set the extra arguments to be used for project level configuration."
  (interactive "sCmake Build Arguments: ")
  (setq ect/project-cmake-build-args args)
  (ect/save-project-settings)
  )


(provide 'emacs-cmake-tools)
;;; emacs-cmake-tools ends here
