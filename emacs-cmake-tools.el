(require 'projectile)
(require 'json)

;;; Code:
;;; Path to cmake binary
(defcustom ect/cmake-binary "cmake"
  "Path of the binary used for cmake."
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the current build type
(defcustom ect/cmake-build-type "Debug"
  "Build type to be used for configuring and building projects."
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

(defun ect/cmake-generate-configure-command ()
  "Helper function for generating the configure command."
  (setq configure_cmd (concat ect/cmake-binary " -S " (projectile-project-root) " -B build -DCMAKE_BUILD_TYPE=Debug"))
  configure_cmd)

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
    (setq cmake-build-directory (concat (projectile-project-root) "/build"))
  ;;  (message "cmake build dir : %s" cmake-build-directory)
    (setq configure_cmd (ect/cmake-generate-configure-command))
    (ect/cmake-generate-write-query-file cmake-build-directory)
    (compile configure_cmd)))


(provide 'emacs-cmake-tools)
;;; emacs-cmake-tools ends here
