;;; flycheck-swiftx.el --- Flycheck: Swift backend -*- lexical-binding: t; -*-

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

;; A Swift backend for Flycheck with support for Xcode projects.
;;
;; Features:
;;
;; - Apple Swift 5
;;
;; - Packages
;;   flycheck-swiftx has basic support for parsing Swift packages.
;;   It will attempt to infer the correct build settings and will also import dependencies
;;   from either the `.build` directory or the Xcode project build directory if found.
;;
;; - Xcode projects
;;   flycheck-swiftx can parse Xcode projects and use the build settings for the project.
;;   This means that complex projects, which may include various dependencies, can be
;;   typechecked automatically with swiftc.
;;
;; - As a fallback you can provide your own configuration e.g. via `flycheck-swiftx-build-options` and `flycheck-swiftx-sources`.
;;
;; - `xcrun` command support (only on macOS)
;;
;; Installation:
;;
;; In your `init.el`
;;
;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-swiftx))
;;
;; or with `use-package`:
;;
;; (use-package flycheck-swiftx
;;  :after flycheck)
;;

;;; Code:

(require 'flycheck)
(require 'flycheck-swiftx-package)
(require 'flycheck-swiftx-xcode)
(require 'flycheck-swiftx-utils)

(flycheck-def-option-var flycheck-swiftx-project-type 'automatic swiftx
  "Specify the project type.

Determines how project settings (SDK, compilation flags, source files etc)
will be obtained.

1. When `automatic' flycheck-swift will search for Package.swift in the buffer's
directory or parent directories and use `package' mode if found.
Otherwise it will fallback to `xcode' mode.

2. When `package' flycheck-swiftx will search for Package.swift in the current
buffer's directory or parent directories.

3. When `xcode' flycheck-swiftx will search for a Xcode project in the current
buffer's directory or parent directories.

4. If no project is found, falls back to using `flycheck-swiftx-build-options'
and `flycheck-swiftx-sources' (which may be specified via .dir-local.el in the
project's root directory).

In the first two cases the project's root directory is that containing
the Xcode project or .dir-locals.el."
  :type '(choice (const :tag "Automatic" automatic)
                 (const :tag "Swift package" package)
                 (const :tag "Xcode project" xcode)
                 (const :tag "None" nil))
  :safe #'symbolp)

(flycheck-def-option-var flycheck-swiftx-build-config "Debug" swiftx
  "Build configuration to use when extracting build settings from the
Xcode project."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swiftx-xcrun-toolchain nil swiftx
  "Specify which toolchain to use to perform the lookup.

When non-nil, set the toolchain identifier or name to use to
perform the lookup, via `--toolchain'.
The option is available only on macOS."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swiftx-sdk nil swift
  "Specify which SDK to use when typechecking (macos, iphoneos etc).

Ignored if the sdk is obtained from an Xcode project.

When non-nil, set the SDK name to find the tools, via `xcrun --sdk'.
The option is available only on macOS.

Use `xcodebuild -showsdks' to list the available SDK names."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swiftx-build-options nil swiftx
  "An list of swiftc build options.

If an Xcode project is found, these build options
are additional to the Xcode project's options.

May be specified as a dir local variable in the project's root."
  :type '(repeat (string :tag "Build option"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swiftx-sources nil swiftx
  "Specify sources files/directory to parse.

Ignored if source files are obtained from an Xcode project.

- When `flycheck-swiftx-sources' is a single directory, flycheck-swiftx will
  recursively include all .swift files found in the directory.
- When `flycheck-swiftx-sources' is a list of file paths, include these as-is.

Paths may be absolute or specified relative to the project's root directory.
May be specified as a dir local variable in the project's root."
  :type '(repeat (string :tag "Source directory or file list"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swiftx-project-target nil swiftx
  "Specify the target to use when extracting build settings from a project.

Ignored if no project is found or the target is not valid.
May be specified as a dir local variable in the project's root."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swiftx-build-env
    '((LOCAL_DEVELOPER_DIR . "/Library/Developer")
      (LOCAL_LIBRARY_DIR . "/Library"))
    swiftx
  "An alist of environment variables to apply to build settings.

For example: LOCAL_DEVELOPER_DIR, LOCAL_LIBRARY_DIR, etc

A default set of standard env variables are provided.

May be specified as a dir local variable in the project's root."
  :type '(repeat (string :tag "Environment var"))
  :safe #'listp)

;; Clean-up cache directories
(add-hook 'kill-emacs-hook #'flycheck-swiftx--cache-cleanup)

(defun flycheck-swiftx--sdk-path (&optional sdk-name)
  "Return the sdk path for SDK-NAME.
If no valid sdk is found, return the default sdk path."
  (flycheck-swiftx--xcrun-sdk-path 'sdk
                                   (flycheck-swiftx--sdk-name (or sdk-name flycheck-swiftx-sdk))))

(defun flycheck-swiftx--project-root (file-name)
  "Return the project root directory for FILE-NAME.

Searches for a Xcode project or Package.swift in FILE-NAME's
directory or parent directories.
Otherwise return FILE-NAME's directory."
  (cond ((or (eq flycheck-swiftx-project-type 'package)
             (and (eq flycheck-swiftx-project-type 'automatic) (flycheck-swiftx-package-root file-name)))
         (or (flycheck-swiftx-package-root file-name) (flycheck-swiftx--project-root-fallback file-name)))
        ((or (eq flycheck-swiftx-project-type 'xcode)
             (and (eq flycheck-swiftx-project-type 'automatic) (xcode-project-find-xcodeproj file-name)))
         (or (xcode-project-find-xcodeproj file-name) (flycheck-swiftx--project-root-fallback file-name)))
        (t (flycheck-swiftx--project-root-fallback file-name))))

(defun flycheck-swiftx--project-root-fallback (file-name)
  "Return the fallback project root directory for FILE-NAME."
  (or (flycheck-swiftx--locate-dominating-file file-name ".git")
      (file-name-directory file-name)))

(defun flycheck-swiftx--source-files (&optional file-name)
  "Return the swift source files specified by `flycheck-swiftx-sources'.

If FILE-NAME is non-nil it is removed from the list of source files."
  (cond ((and (stringp flycheck-swiftx-sources)
              (file-directory-p flycheck-swiftx-sources))
         (remove file-name (flycheck-swiftx-package-list-swift-files flycheck-swiftx-sources)))
        (flycheck-swiftx-sources
         (let ((inputs flycheck-swiftx-sources)
               (directory-name (file-name-directory file-name)))
           (unless (listp inputs)
             (setq inputs (list inputs)))
           (remove file-name (flycheck-swiftx--expand-inputs inputs directory-name))))))

(defun flycheck-swiftx--expand-inputs (inputs &optional directory)
  "Return the expanded inputs.

If input files `INPUTS' is not nil, return the list of expanded
input files using `DIRECTORY' as the default directory."
  (let (expanded-inputs)
    (dolist (input inputs expanded-inputs)
      (if (file-name-absolute-p input)
          (setq expanded-inputs
                (append expanded-inputs (file-expand-wildcards input t)))
        (setq expanded-inputs
              (append expanded-inputs
                      (file-expand-wildcards
                       (expand-file-name input directory) t)))))))

(defun flycheck-swiftx--append-options (prefix options)
  "Append OPTIONS to PREFIX.
If options is a list or vector, maps each option to prefix.
Return nil if options is nil."
  (cond ((stringp options)
         (list prefix options))
        ((listp options)
         (mapcan (lambda(opt) (list prefix opt)) (remq nil options)))
        ((vectorp options)
         (mapcan (lambda(opt) (list prefix opt)) (remq nil (append options nil))))))

(defun flycheck-swiftx--swiftc-options ()
  "Return a list of swift command line options.

When `flycheck-swiftx-project-type' is `xcode' then use the
associated Xcode project's build settings to determine command line options.

Otherwise fall back to the flycheck-swiftx custom options."
  (let* ((file-name (or load-file-name buffer-file-name))
         (package-opts (when (or (eq flycheck-swiftx-project-type 'package)
                                 (and (eq flycheck-swiftx-project-type 'automatic) (flycheck-swiftx-package-path file-name)))
                         (flycheck-swiftx--package-options file-name)))
         (xcode-opts (when (or (eq flycheck-swiftx-project-type 'xcode)
                               (and (eq flycheck-swiftx-project-type 'automatic) (flycheck-swiftx-find-xcproj-path file-name)))
                       (flycheck-swiftx--xcode-options file-name))))
    (cond (xcode-opts
           xcode-opts)
          (package-opts
           package-opts)
          (t
           ;; ensure -sdk is present in flycheck-swiftx-build-options
           (let ((user-build-options (if (seq-contains-p flycheck-swiftx-build-options "-sdk")
                                         flycheck-swiftx-build-options
                                       (append flycheck-swiftx-build-options `("-sdk" ,(flycheck-swiftx--sdk-path))))))
             `(,@user-build-options
               ;; Associated source files, ignoring the file currently being checked.
               ,@(flycheck-swiftx--source-files file-name)))))))

(defun flycheck-swiftx--package-options (file-name)
  "Return a list of swiftc options obtained from the Swift package for FILE-NAME."
  (when-let* ((package-path (flycheck-swiftx-package-path file-name))
              (package (flycheck-swiftx-load-package package-path))
              (platform (flycheck-swiftx-package-preferred-platform package "macos"))
              (target (flycheck-swiftx-package-target package file-name)))
    (let* ((build-dir (flycheck-swiftx-package-build-dir package-path flycheck-swiftx-build-config))
           (xcproj-path (flycheck-swiftx-find-xcproj-path file-name))
           (xcode-products-dir (when xcproj-path (flycheck-swiftx-xcode-build-products-dir xcproj-path flycheck-swiftx-build-config)))
           (xcode-generated-modulemaps-dir (when xcproj-path (flycheck-swiftx-xcode-generated-modulemaps-dir xcproj-path))))
      `(
        ,@(flycheck-swiftx--append-options "-module-name" (flycheck-swiftx-package-target-name target))
        ,@(flycheck-swiftx--append-options "-target" (flycheck-swiftx-package-deployment-target platform))
        ,@(flycheck-swiftx--append-options "-swift-version" (flycheck-swiftx-package-swift-latest-version package))
        ;; ,@(flycheck-swiftx--append-options "-new-driver-path" (flycheck-swiftx-xcrun-find "swift-driver"))
        ,@(flycheck-swiftx--append-options "-sdk" (flycheck-swiftx--sdk-path (flycheck-swiftx-package-platform-name platform)))
        ,@(flycheck-swiftx--append-options "-D" (flycheck-swiftx-package-defines package flycheck-swiftx-build-config))
        ,@(flycheck-swiftx--append-options "-I" (flycheck-swiftx-package-includes package))
        ;; Add target build dir to ensure that any framework dependencies are found
        ,@(flycheck-swiftx--append-options "-F" `(,build-dir ,xcode-products-dir))
        ,@(flycheck-swiftx--append-options "-I" `(,build-dir ,xcode-products-dir))
        ,@(flycheck-swiftx--append-options "-Xcc" (flycheck-swiftx-package-module-map-files
                                                   (flycheck-swiftx--path-latest build-dir xcode-generated-modulemaps-dir)
                                                   (flycheck-swiftx-package-name package)))
        ;; Optional, additional build options
        ,@flycheck-swiftx-build-options
        ;; Associated source files, ignoring the file currently being checked.
        ,@(flycheck-swiftx-package-source-files package-path (flycheck-swiftx-package-target-path target) file-name)
        )
      )))

(defun flycheck-swiftx--xcode-options (file-name)
  "Return a list of swiftc options obtained from an Xcode project.

FILE-NAME should be part of the project and is used to determine
the current target.  Only the first target found is used."
  (when-let* ((xcproj-path (flycheck-swiftx-find-xcproj-path file-name))
              (xcproj (flycheck-swiftx-load-xcode-project xcproj-path))
              (target-name (flycheck-swiftx-xcode-target-name file-name xcproj flycheck-swiftx-project-target)))
    ;; build-settings are optional (though should usually be present).
    (let ((build-env (cons '("PROJECT_DIR" . (directory-file-name (file-name-directory xcproj-path))) flycheck-swiftx-build-env))
          (build-settings (flycheck-swiftx-xcode-build-settings xcproj target-name flycheck-swiftx-build-config))
          (build-products-dir (flycheck-swiftx-xcode-build-products-dir xcproj-path flycheck-swiftx-build-config target-name))
          (enable-testing (flycheck-swiftx--maybe-test file-name)))
      `(
        ,@(flycheck-swiftx--append-options "-module-name" target-name)
        ,@(flycheck-swiftx--append-options "-target"
                                           (flycheck-swiftx-xcode-deployment-target build-settings))
        ,@(flycheck-swiftx--append-options "-swift-version"
                                           (flycheck-swiftx-xcode-swift-version build-settings))
        ,@(flycheck-swiftx--append-options "-sdk"
                                           (flycheck-swiftx--sdk-path (flycheck-swiftx-xcode-sdk-root build-settings)))
        ,@(flycheck-swiftx--append-options "-import-objc-header"
                                           (flycheck-swiftx-xcode-objc-bridging-header build-settings xcproj-path))
        ,@(flycheck-swiftx-xcode-objc-inference build-settings)
        ,@(flycheck-swiftx--append-options "-D"
                                           (flycheck-swiftx-xcode-gcc-compilation-flags build-settings))
        ;; Other compiler flags
        ,@(flycheck-swiftx-xcode-list-option 'OTHER_SWIFT_FLAGS build-settings)
        ;; Tests
        ,@(when enable-testing (flycheck-swiftx-xcode-options-testing build-settings))
        ;; Search paths
        ;; Note: SDK search path should come before other framework search paths.
        ,@(flycheck-swiftx--append-options "-F"
                                           (seq-map (lambda (path) (concat (file-name-as-directory path) "Library/Frameworks"))
                                                    (flycheck-swiftx-xcode-path-option 'ADDITIONAL_SDKS
                                                                                  build-settings
                                                                                  build-env)))
        ,@(flycheck-swiftx--append-options "-F"
                                           (flycheck-swiftx-xcode-path-option 'FRAMEWORK_SEARCH_PATHS
                                                                                build-settings
                                                                                build-env))
        ,@(flycheck-swiftx--append-options "-I"
                                           (flycheck-swiftx-xcode-path-option 'HEADER_SEARCH_PATHS
                                                                                build-settings
                                                                                build-env))
        ,@(flycheck-swiftx--append-options "-I"
                                           (flycheck-swiftx-xcode-path-option 'USER_HEADER_SEARCH_PATHS
                                                                                build-settings
                                                                                build-env))
        ,@(flycheck-swiftx--append-options "-I"
                                           (flycheck-swiftx-xcode-path-option 'SYSTEM_HEADER_SEARCH_PATHS
                                                                                build-settings
                                                                                build-env))
        ,@(flycheck-swiftx--append-options "-I"
                                           (flycheck-swiftx-xcode-path-option 'SWIFT_INCLUDE_PATHS
                                                                                build-settings
                                                                                build-env))
        ;; Add target build dir to ensure that any framework dependencies are found
        ,@(flycheck-swiftx--append-options "-F" build-products-dir)
        ,@(flycheck-swiftx--append-options "-I" build-products-dir)
        ;; Optional, additional build options
        ,@flycheck-swiftx-build-options
        ;; Associated source files, ignoring the file currently being checked.
        ,@(flycheck-swiftx-xcode-source-files xcproj target-name file-name)))))

(defun flycheck-swiftx--command ()
  "Return the command to run for Swift syntax checking.

Note: `flycheck-swiftx--command' is evaluated only once
when flycheck-swiftx is initialized."
  (if-let ((xcrun-path (executable-find "xcrun")))
      `(,xcrun-path
        (option "--toolchain" flycheck-swiftx-xcrun-toolchain)
        "swiftc")
    '("swiftc")))

(defun flycheck-swiftx--error-filter (errors)
  "Return filtered ERRORS.
swiftc v5.1.2 now returns errors for all inputs, even though we
specify a -primary-file.  We don't want to see errors for other
files in the current buffer, so we discard them here."
  (let* ((file-name (or load-file-name buffer-file-name)))
    (seq-filter
     (lambda (err)
       (or (equal "<unknown>" (flycheck-error-filename err))
           (equal file-name (flycheck-error-filename err))))
     errors)))

(flycheck-define-command-checker 'swiftx
  "A Swift syntax checker using Swift compiler frontend.

See URL `https://swift.org/'."
  :command `(,@(flycheck-swiftx--command)
             "-frontend"
             "-typecheck"
             (eval (flycheck-swiftx--swiftc-options))
             "-primary-file" source)
  :error-patterns '((error line-start "<unknown>:" line
                           ": " "error: " (optional (message)) line-end)
                    (info line-start (or "<stdin>" (file-name)) ":" line ":" column
                          ": " "note: " (optional (message)) line-end)
                    (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
                             ": " "warning: " (optional (message)) line-end)
                    (error line-start (or "<stdin>" (file-name)) ":" line ":" column
                           ": " "error: " (optional (message)) line-end))
  :error-filter 'flycheck-swiftx--error-filter
  ;; Ensure paths (e.g flycheck-swiftx-sources) are interpreted relative to project's root directory.
  :working-directory (lambda (_) (flycheck-swiftx--project-root (or load-file-name buffer-file-name)))
  :modes 'swift-mode)

;; Set up Flycheck for Swift.
;;
;; Debug:
;;
;; In flycheck.el:flycheck-start-command-checker, add:
;; (when (equal checker 'swiftx) (message "%s %s" checker command))

(add-to-list 'flycheck-checkers 'swiftx)

(add-hook 'swift-mode-hook (lambda () (flycheck-mode)))

(provide 'flycheck-swiftx)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swiftx.el ends here
