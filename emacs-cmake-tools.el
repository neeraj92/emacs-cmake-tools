(require 'projectile)
(require 'json)

;;; Path to cmake binary
(defcustom ect/cmake-binary "cmake"
  "Path of the binary used for cmake"
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the current build type
(defcustom ect/cmake-build-type "Debug"
  "Build type to be used for configuring and building projects"
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the build directory prefix
;;; Currently only supports a relative path inside the project directory
(defcustom ect/cmake-build-directory-prefix "build"
  "Prefix to the relative path inside the project directory"
  :type 'string
  :group 'emacs-cmake-tools)

;;; Toggle to enable generating compilation database
(defcustom ect/cmake-generate-compilation-database nil
  "Toggle to generate compilation database"
  :type 'boolean
  :group 'emacs-cmake-tools)

;;; Variable to set configuration arguments for cmake
(defcustom ect/cmake-configure-arguments ""
  "Variable to override the cmake configure arguments"
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the build arguments for cmake
(defcustom ect/cmake-build-arguments ""
  "Variable to override the cmake build arguments"
  :type 'string
  :group 'emacs-cmake-tools)

;;; Variable to set the generator for cmake
(defcustom ect/cmake-generator "Makefile"
  "Variable to set the generator for cmake"
  :type 'string
  :group 'emacs-cmake-tools)
