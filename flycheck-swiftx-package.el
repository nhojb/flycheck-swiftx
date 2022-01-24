;;; flycheck-swiftx-package.el --- A package for reading Swift package files.

;; Copyright (c) 2022 John Buckley <nhoj.buckley@gmail.com>

;; Author: John Buckley <nhoj.buckley@gmail.com>
;; URL: https://github.com/nhojb/flycheck-swiftx
;; Version: 1.1
;; Keywords: convenience, languages, tools
;; Package-Requires: ((emacs "26.1") (flycheck "26") (xcode-project "1.0"))

;; This file is not part of GNU Emacs.

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package supports reading Swift package files.
;;

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'flycheck-swiftx-utils)

(defun flycheck-swiftx-package-path (file-name)
  "Return the Package.swift path for FILE-NAME."
  (when-let (package-root (flycheck-swiftx-package-root file-name))
    (file-name-concat package-root "Package.swift")))

(defun flycheck-swiftx-package-root (file-name)
  "Return the directory containing Package.swift relative to FILE-NAME."
  (flycheck-swiftx--locate-dominating-file file-name "Package.swift"))

(defun flycheck-swiftx-load-package (package-path &optional swift-path)
  "Load the Swift Package found at PACKAGE-PATH.
Optional SWIFT-PATH specifies the path to the `swift` executable."
  (when (and (stringp package-path) (file-exists-p package-path))
    (json-parse-string
     (shell-command-to-string
      (format "%s package --package-path %s dump-package"
              (or swift-path "swift")
              (if (file-directory-p package-path) package-path
                (file-name-directory package-path)))))))

(defun flycheck-swiftx-package-source-files (package-path target-path &optional file-name)
  "Return swift source files for the specified Swift PACKAGE-PATH and TARGET-PATH.
If FILE-NAME is non-nil it is removed from the list of source files."
  ;; TODO: Support "Test" files also
  (when-let ((sources-dir (file-name-concat (file-name-directory package-path) target-path)))
    (remove file-name (flycheck-swiftx-package-list-swift-files sources-dir))))

(defun flycheck-swiftx-package-list-swift-files (directory)
  "Return list of full paths to swift files in the specified DIRECTORY."
  (seq-filter
   (lambda (elt) (eq 0 (string-match-p "[^\\.].*" (file-name-nondirectory elt))))
   (directory-files-recursively directory ".*\\.swift$")))

(defun flycheck-swiftx-package-target (package file-name)
  "Return the target name for FILE-NAME found in PACKAGE."
  (when-let ((targets (flycheck-swiftx-package-targets package)))
    (seq-find (lambda (target)
                (when-let ((target-path (gethash "path" target)))
                  (locate-dominating-file file-name (lambda (name)
                                                      (string-equal (file-name-nondirectory (directory-file-name name)) target-path)))))
              targets)))

(defun flycheck-swiftx-package-build-dir (package-path build-config)
  "Return full path to build dir to PACKAGE-PATH and BUILD-CONFIG."
  (let* ((build-dir (file-name-concat (file-name-directory package-path) ".build"))
         (build-config-dir (if (equal "Debug" build-config)
                               (file-name-concat build-dir "debug")
                             (file-name-concat build-dir "release"))))
    (when (file-directory-p build-config-dir)
      build-config-dir)))

;; Package Properties

(defun flycheck-swiftx-package-name (package)
  "Return name for PACKAGE."
  (gethash "name" package))

(defun flycheck-swiftx-package-kind (package)
  "Return kind for PACKAGE."
  (gethash "packageKind" package))

(defun flycheck-swiftx-package-c-lang-standard (package)
  "Return cLanguageStandard for PACKAGE."
  (gethash "cLanguageStandard" package))

(defun flycheck-swiftx-package-cxx-lang-standard (package)
  "Return cxxLanguageStandard for PACKAGE."
  (gethash "cxxLanguageStandard" package))

(defun flycheck-swiftx-package-config (package)
  "Return package config for PACKAGE."
  (gethash "pkgConfig" package))

(defun flycheck-swiftx-package-dependencies (package)
  "Return package config for PACKAGE."
  (gethash "dependencies" package))

(defun flycheck-swiftx-package-platforms (package)
  "Return platforms for PACKAGE."
  (gethash "platforms" package))

(defun flycheck-swiftx-package-products (package)
  "Return products for PACKAGE."
  (gethash "products" package))

(defun flycheck-swiftx-package-providers (package)
  "Return providers for PACKAGE."
  (gethash "providers" package))

(defun flycheck-swiftx-package-swift-versions (package)
  "Return swift language versions for PACKAGE."
  (gethash "swiftLanguageVersions" package))

(defun flycheck-swiftx-package-swift-latest-version (package)
  "Return swift language versions for PACKAGE."
  (seq-first (seq-sort 'string-greaterp (gethash "swiftLanguageVersions" package))))

(defun flycheck-swiftx-package-targets (package)
  "Return targets for PACKAGE."
  (gethash "targets" package))

(defun flycheck-swiftx-package-tools-version (package)
  "Return tools version for PACKAGE."
  (gethash "_version" (gethash "toolsVersion" package)))

;; Platform

(defun flycheck-swiftx-package-platform-name (platform)
  "Return the name for PLATFORM."
  (gethash "platformName" platform))

(defun flycheck-swiftx-package-platform-version (platform)
  "Return the version for PLATFORM."
  (gethash "version" platform))

(defun flycheck-swiftx-package-preferred-platform (package name)
  "Return first platform matching NAME in PACKAGE."
  (when-let ((platforms (flycheck-swiftx-package-platforms package)))
    (or (seq-find
         (lambda (p) (equal (gethash "platformName" p) name))
         platforms)
        (seq-first platforms))))

(defun flycheck-swiftx-package-deployment-target (platform)
  "Return the build target for PLATFORM."
  (cond ((equal "macos" (flycheck-swiftx-package-platform-name platform))
         (format "x86_64-apple-macosx%s" (flycheck-swiftx-package-platform-version platform)))
        ((equal "ios" (flycheck-swiftx-package-platform-name platform))
         (format "x86_64-apple-ios%s" (flycheck-swiftx-package-platform-version platform)))))

;; Target

(defun flycheck-swiftx-package-target-name (target)
  "Return name for TARGET."
  (gethash "name" target))

(defun flycheck-swiftx-package-target-path (target)
  "Return path for TARGET."
  (gethash "path" target))

(defun flycheck-swiftx-package-target-type (target)
  "Return type for TARGET."
  (gethash "type" target))

(defun flycheck-swiftx-package-target-dependencies (target)
  "Return dependencies for TARGET."
  (gethash "dependencies" target))

;; Build Settings

(defun flycheck-swiftx-package-defines (package build-config)
  "Return compiler definitions for PACKAGE and BUILD-CONFIG."
  (if (equal "Debug" build-config)
      '("SWIFT_PACKAGE" "DEBUG")
    '("SWIFT_PACKAGE")))

(defun flycheck-swiftx-package-includes (package)
  "Return compiler definitions for PACKAGE."
  nil)

(defun flycheck-swiftx-package-module-map-files (build-dir package-name)
  "Return paths of modulemap files found in BUILD-DIR excluding PACKAGE-NAME."
  (when (file-directory-p build-dir)
    (let ((package-re (format "%s\\.\\(build\\|modulemap\\)" package-name)))
      (mapcar (lambda(f) (concat "-fmodule-map-file=" f))
              (seq-filter (lambda (path) (not (string-match-p package-re path)))
                          (directory-files-recursively build-dir ".*\\.modulemap$"))))))

;; Helpers

(defun flycheck-swiftx--map-null (value)
  (if (eq :null-object value)
      value))

(provide 'flycheck-swiftx-package)
;;; flycheck-swiftx-package.el ends here
