(require 'projectile)
(require 'json)
(require 'lsp)

;;; Commentary:
;;; emacs-cmake-tools is a package to provide helper functions to run cmake and ctest commands.
;;; Recent additions include support for CMake Presets (CMakePresets.json / CMakeUserPresets.json) via the `ect/cmake-use-presets` toggle.

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

(defcustom ect/cmake-use-presets nil
  "Enable CMake preset mode.
When t, emacs-cmake-tools searches for `CMakePresets.json` or
`CMakeUserPresets.json` in the project root. Found presets populate
build configuration choices, overriding `ect/cmake-build-types`.
The selected preset name is then used with `cmake --preset <name>`."
  :type 'boolean
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
  "List of available build types.
Used when `ect/cmake-use-presets` is `nil` or if no valid CMake
preset file is found in the project."
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

(defconst ect/cmake-user-preset-file-name "CMakeUserPresets.json"
  "Default name for CMake user presets file.")

(defconst ect/cmake-preset-file-name "CMakePresets.json"
  "Default name for CMake presets file.")

(defun ect/find-preset-file (project-root)
  "Find the CMake preset file in PROJECT-ROOT.
Checks for `CMakeUserPresets.json` first, then `CMakePresets.json`.
Returns the full path to the file if found, otherwise nil."
  (let ((user-preset-file (concat project-root "/" ect/cmake-user-preset-file-name))
        (preset-file (concat project-root "/" ect/cmake-preset-file-name)))
    (cond
     ((file-exists-p user-preset-file) user-preset-file)
     ((file-exists-p preset-file) preset-file)
     (t nil))))

(defun ect/parse-presets (preset-file-path)
  "Parse CMake preset file PRESET-FILE-PATH and return a list of preset names.
Returns nil if the file does not exist, JSON parsing fails, or the
expected structure is not found."
  (unless (and preset-file-path (file-exists-p preset-file-path))
    (message "Preset file does not exist: %s" preset-file-path)
    (cl-return-from ect/parse-presets nil))

  (let ((json-object nil)
        (preset-names nil))
    (with-temp-buffer
      (insert-file-contents preset-file-path)
      (condition-case err
          (setq json-object (json-parse-buffer :object-type 'hash-table))
        (error
         (message "Error parsing CMake preset file: %s. Error: %s" preset-file-path err)
         (cl-return-from ect/parse-presets nil))))

    (unless (hash-table-p json-object)
      (message "Invalid JSON structure in preset file: %s. Root is not an object." preset-file-path)
      (cl-return-from ect/parse-presets nil))

    (let ((configure-presets (gethash "configurePresets" json-object)))
      (unless configure-presets
        (message "No 'configurePresets' key found in CMake preset file: %s" preset-file-path)
        (cl-return-from ect/parse-presets nil))

      (unless (vectorp configure-presets)
        (message "'configurePresets' is not a JSON array in preset file: %s" preset-file-path)
        (cl-return-from ect/parse-presets nil))

      (dotimes (i (length configure-presets))
        (let* ((preset (aref configure-presets i))
               (name (gethash "name" preset)))
          (when (stringp name)
            (add-to-list 'preset-names name))))
      (nreverse preset-names))))

(defun ect/remove-tramp-prefix (path)
  "Remove TRAMP prefix from PATH if it exists."
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname
       (tramp-dissect-file-name path))
    path))

(defun ect/cmake-generate-configure-command (build-directory)
  "Helper function for generating the CMake configure command.
If `ect/cmake-use-presets` is `t` and `ect/local-cmake-build-type`
contains a selected preset name, the command will use `cmake --preset <preset-name>`.
In this mode, `-DCMAKE_BUILD_TYPE` and `-G` are omitted from the command line
as they are expected to be defined by the preset.
Otherwise, constructs the command using `ect/local-cmake-build-type` for `-DCMAKE_BUILD_TYPE`
and `ect/cmake-current-generator` for `-G`."
  (let ((configure_cmd ""))
    (if ect/cmake-use-presets
        (setq configure_cmd (concat ect/cmake-binary " --preset " ect/local-cmake-build-type
                                    " -S " ect/cmake-source-directory
                                    " -B " build-directory
                                    " " ect/project-cmake-configure-args))
      (setq configure_cmd (concat ect/cmake-binary " -S " ect/cmake-source-directory
                                    " -B " build-directory
                                    " -DCMAKE_BUILD_TYPE=" ect/local-cmake-build-type
                                    " -G " ect/cmake-current-generator
                                    " " ect/project-cmake-configure-args)))
    configure_cmd))

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
  "Helper function to choose the build configuration.
If `ect/cmake-use-presets` is enabled and a valid CMake preset file
(CMakePresets.json or CMakeUserPresets.json) is found in the project root,
this function will offer a choice from the discovered preset names.
The selected preset name is stored in `ect/local-cmake-build-type`.
Otherwise, it offers a choice from the `ect/cmake-build-types` list."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if ect/cmake-use-presets
        (let ((preset-file-path (ect/find-preset-file project-root)))
          (if preset-file-path
              (let ((preset-names (ect/parse-presets preset-file-path)))
                (if (and preset-names (not (eq (length preset-names) 0)))
                    (setq ect/local-cmake-build-type (completing-read "Choose preset: " preset-names nil t nil))
                  (progn
                    (message "No presets found or error parsing preset file. Falling back to default build types.")
                    (setq ect/local-cmake-build-type (completing-read "Choose build type: " ect/cmake-build-types nil t nil)))))
            (progn
              (message "No CMake preset file found in project root. Falling back to default build types.")
              (setq ect/local-cmake-build-type (completing-read "Choose build type: " ect/cmake-build-types nil t nil)))))
      (setq ect/local-cmake-build-type (completing-read "Choose build type: " ect/cmake-build-types nil t nil))))
  (ect/save-project-settings))

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
  (message "Generating the query in directory : %s and query direcotry : %s" build-directory cmake-query-directory)
  (make-directory cmake-query-directory t)
  (message "generated directory")
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
    (setq project-root (ect/remove-tramp-prefix (projectile-project-root)))
    (message "project root : %s" project-root)
    (setq cmake-build-directory (concat (projectile-project-root) "/" build-directory))
    (setq remote-build-directory (concat project-root "/" build-directory))
    (message "cmake build directory : %s" cmake-build-directory)
    (setq configure_cmd (ect/cmake-generate-configure-command build-directory))
    (ect/cmake-generate-write-query-file cmake-build-directory)
    (setq lt-clangd-compilation-commands-dir (concat "--compile-commands-dir=" remote-build-directory))
    (setq lsp-sonarlint-cfamily-compile-commands-path (concat remote-build-directory "/compile_commands.json"))
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
  '  (let ((file-path (ect/project-settings-file-path)))
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
