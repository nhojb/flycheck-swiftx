;;; flycheck-swiftx.el --- Flycheck: Swift backend -*- lexical-binding: t; -*-

;; Copyright (c) 2019 John Buckley <john@olivetoast.com>

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/flycheck-swiftx
;; Version: 1.0.0
;; Keywords: convenience, languages, tools
;; Package-Requires: ((emacs "24.4") (flycheck "26"))

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

;; Add Swift support to Flycheck using Swift compiler frontend.
;;
;; Features:
;;
;; - Apple Swift 5 support.
;; - Integration with Xcode projects.
;; - The `xcrun` command support (only on macOS)
;;
;; Usage:
;;
;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-swiftx))
;;
;; Debug:
;; In flycheck.el:flycheck-start-command-checker, add:
;; (when (equal checker 'swiftx) (message "%s %s" checker args))

;;; Code:

(require 'flycheck)
(require 'xcode-project)

(flycheck-def-option-var flycheck-swiftx-project-type 'xcode swiftx
  "Specify the project type.

When non-nil, specifies how project settings (SDK, compilation flags, source files etc)
will be obtained.
"
  :type '(choice (const :tag "Xcode project" xcode)
                 (const :tag "None" nil))
  :safe #'symbolp)

(flycheck-def-option-var flycheck-swiftx-xcode-build-config "Debug" swiftx
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

(defvar flycheck-swiftx--cache-directory nil
  "The cache directory for `flycheck-swiftx'.")

(defun flycheck-swiftx--cache-location ()
  "Get the cache location for `flycheck-swiftx'.

If no cache directory exists yet, create one and return it.
Otherwise return the previously used cache directory."
  (setq flycheck-swiftx--cache-directory
        (or flycheck-swiftx--cache-directory
            ;; Note: we can't use flycheck-temp-dir-system because the temp dir
            ;; will be destroyed after each run of the checker...
            (make-temp-file flycheck-temp-prefix 'directory))))

(defun flycheck-swiftx--cache-cleanup ()
  "Cleanup `flycheck-swiftx' cache directory."
  (when (and flycheck-swiftx--cache-directory
             (file-directory-p flycheck-swiftx--cache-directory))
    (flycheck-safe-delete flycheck-swiftx--cache-directory))
  (setq flycheck-swiftx--cache-directory nil))

;; Clean-up cache directories
(add-hook 'kill-emacs-hook #'flycheck-swiftx--cache-cleanup)

;; Xcode project support

(defun flycheck-swiftx--xcodeproj-modtime (xcproj-path)
  "Return the modification time for XCPROJ-PATH."
  (setq xcproj-path (expand-file-name xcproj-path))
  (if (and xcproj-path (file-directory-p xcproj-path))
      (nth 5 (file-attributes (xcode-project-concat-path xcproj-path "project.pbxproj")))))

;; TODO: Would be better to cache the filelist and only rebuild when modtime changes ;-)
(defun flycheck-swiftx--xcodeproj-cache-path (xcproj-path)
  "Return the cache path for the specified XCPROJ-PATH."
  (when (and xcproj-path (file-directory-p xcproj-path))
    (format "%s%s.%s" (file-name-as-directory (flycheck-swiftx--cache-location))
            (file-name-nondirectory xcproj-path)
            (time-to-seconds (flycheck-swiftx--xcodeproj-modtime xcproj-path)))))

(defun flycheck-swiftx--read-xcode-project-cache (xcproj-path)
  "Return the project cache for XCPROJ-PATH.
Return nil if the cache is not found, or the modification time of the project
has changed."
  (when-let (cache-path (flycheck-swiftx--xcodeproj-cache-path xcproj-path))
    (when (file-exists-p cache-path)
      (xcode-project-deserialize cache-path))))

(defun flycheck-swiftx--write-xcode-project-cache (proj xcproj-path)
  "Cache the parsed project PROJ for XCPROJ-PATH."
  (when-let (cache-path (flycheck-swiftx--xcodeproj-cache-path xcproj-path))
    (ignore-errors (xcode-project-serialize proj cache-path))))

(defun flycheck-swiftx--load-xcode-project (xcproj-path)
  "Load and return the Xcode project found at XCPROJ-PATH.

If the parsed Xcode project is found in our cache, return that.
Otherwise read from disk, cache and return the project."
  (setq xcproj-path (expand-file-name xcproj-path))
  (when (and xcproj-path (file-directory-p xcproj-path))
    (or (flycheck-swiftx--read-xcode-project-cache xcproj-path)
        (when-let (proj (xcode-project-read xcproj-path))
          (flycheck-swiftx--write-xcode-project-cache proj xcproj-path)
          proj))))

(defun flycheck-swiftx--xcode-build-settings (xcproj target-name)
  "Return the build settings for the specified XCPROJ and TARGET-NAME."
  (let ((config-name (or flycheck-swiftx-xcode-build-config "Debug")))
    (when xcproj
      (alist-get 'buildSettings (xcode-project-build-config xcproj
                                                            config-name
                                                            target-name)))))
(defun flycheck-swiftx--target (build-settings)
  "Return the platform target for BUILD-SETTINGS."
  (when build-settings
      (let ((macos-target (alist-get 'MACOSX_DEPLOYMENT_TARGET build-settings))
            (iphoneos-target (alist-get 'IPHONEOS_DEPLOYMENT_TARGET build-settings)))
        (cond (macos-target
               (format "x86_64-apple-macosx%s" macos-target))
              (iphoneos-target
               ;; We never want "arm*" for flycheck.
               (format "x86_64-apple-ios%s" iphoneos-target))))))

(defun flycheck-swiftx--swift-version (build-settings)
  "Return the swift version for BUILD-SETTINGS."
  (when-let (swift-version (alist-get 'SWIFT_VERSION build-settings))
    ;; -swift-version appears to require integers (4 not 4.0 etc).
    ;; Major versions, such as 4.2, are however valid.
    (if (equal (fround swift-version) swift-version)
        (number-to-string (truncate swift-version))
      (number-to-string swift-version))))

(defun flycheck-swiftx--target-build-dir (target-name)
  "Return the target build dir for TARGET-NAME.
Uses heuristics to locate the build dir in ~/Library/Developer/Xcode/DerivedData/."
  (let* ((build-root (expand-file-name "~/Library/Developer/Xcode/DerivedData/"))
         (results (directory-files-and-attributes build-root t (concat target-name "\\-[a-z]+"))))
    (car (seq-reduce (lambda (result item)
                       (if (time-less-p (nth 6 result) (nth 6 item))
                           item
                         result))
                     results (car results)))))

(defun flycheck-swiftx--xcrun-sdk-path (xcrun-path &optional xcrun-sdk)
  "Return the swift SDK path using `${XCRUN-PATH} --sdk ${XCRUN-SDK} --show-sdk-path'."
  (when xcrun-path
    (let ((command
           (if xcrun-sdk
               (mapconcat #'identity `(,xcrun-path "--sdk" ,xcrun-sdk "--show-sdk-path") " ")
             (mapconcat #'identity `(,xcrun-path "--show-sdk-path") " "))))
      (string-trim (shell-command-to-string command)))))

(defun flycheck-swiftx--sdk-path (build-settings xcrun-path)
  "Return the platform sdk for BUILD-SETTINGS.
If no valid sdk is found, return flycheck-swiftx--xcrun-sdk-path using
XCRUN-PATH.

If BUILD-SETTINGS is nil return flycheck-swiftx--xcrun-sdk-path."
  (if build-settings
      (let ((sdk-root (alist-get 'SDKROOT build-settings)))
        (when (equal sdk-root "iphoneos")
          (setq sdk-root "iphonesimulator"))
        (flycheck-swiftx--xcrun-sdk-path xcrun-path sdk-root))
    (flycheck-swiftx--xcrun-sdk-path xcrun-path)))

(defun flycheck-swiftx--list-swift-files (directory)
  "Return list of full paths to swift files in the specified DIRECTORY."
  (seq-filter
   (lambda (elt) (eq 0 (string-match-p "[^\.].*" (file-name-nondirectory elt))))
   (directory-files directory t ".*\.swift$")))

(defun flycheck-swiftx--source-files (xcproj target-name)
  "Return the swift source files associated with the current buffer.

If XCPROJ and TARGET-NAME are non-nil then returns the source files
for the specified Xcode target.

Otherwise returns all .swift file found in the current buffer's directory."
  (if xcproj
      (xcode-project-build-file-paths xcproj
                                      target-name
                                      "PBXSourcesBuildPhase"
                                      (lambda (file)
                                        (xcode-project-file-ref-extension-p file "swift"))
                                      'absolute)
    (let* ((file-name (or load-file-name buffer-file-name))
           (directory-name (file-name-directory file-name)))
      (flycheck-swiftx--list-swift-files directory-name))))

(defun flycheck-swiftx--objc-bridging-header (xcproj-path build-settings)
  "Return path to Objc bridging header if found in BUILD-SETTINGS."
  (when-let (header (flycheck-swiftx--string-option 'SWIFT_OBJC_BRIDGING_HEADER build-settings))
    (concat (file-name-directory xcproj-path) header)))

(defun flycheck-swiftx--objc-inference (build-settings)
  "Return objc inference compiler flags for BUILD-SETTINGS."
  (when-let (objc-inference (alist-get 'SWIFT_SWIFT3_OBJC_INFERENCE build-settings))
    (cond ((equal objc-inference "On")
           '("-enable-swift3-objc-inference"
             "-warn-swift3-objc-inference-minimal"))
          ((equal objc-inference "Off")
           "-disable-swift3-objc-inference"))))

(defun flycheck-swiftx--gcc-compilation-flags (build-settings)
  "Return a list of GCC conditional compilation flags found in BUILD-SETTINGS."
  (when-let (preprocessor-defs (alist-get 'GCC_PREPROCESSOR_DEFINITIONS build-settings))
    (when (stringp preprocessor-defs)
      (setq preprocessor-defs (vector preprocessor-defs)))
    (seq-concatenate 'vector
                     preprocessor-defs
                     (alist-get 'GCC_PREPROCESSOR_DEFINITIONS_NOT_USED_IN_PRECOMPS build-settings))))

(defun flycheck-swiftx--string-option (key build-settings)
  "Return the string value for KEY in BUILD-SETTINGS."
  (when-let* ((value (alist-get key build-settings))
              (stringp value))
    value))

(defun flycheck-swiftx--list-option (key build-settings)
  "Return a list of options for KEY in BUILD-SETTINGS."
  (when-let (values (alist-get key build-settings))
    (if (stringp values)
        (seq-into (split-string values) 'vector)
      values)))

(defun flycheck-swiftx--append-options (prefix options)
  "Append OPTIONS to PREFIX.
If options is a list or vector, maps each option to prefix.
Return nil if options is nil."
  (cond ((stringp options)
         (list prefix options))
        ((listp options)
         (mapcan (lambda(opt) (list prefix opt)) options))
        ((vectorp options)
         (mapcan (lambda(opt) (list prefix opt)) (append options nil)))))

(defun flycheck-swiftx--swiftc-options (file-name xcrun-path)
  "Return a list of swiftc command line options for FILE-NAME.

If `flycheck-swiftx-project-type' is `xcode' then use the associated
Xcode project's build settings to determine command line options.

The XCRUN-PATH is used to locate tools and sdks.

Otherwise fall back to the flycheck-swiftx custom options."
  (let* ((xcproj-path (xcode-project-find-xcodeproj file-name))
         (xcproj (when (eq flycheck-swiftx-project-type 'xcode)
                   (flycheck-swiftx--load-xcode-project xcproj-path)))
         (target-name (when xcproj
                        (car (xcode-project-target-names-for-file xcproj file-name "PBXSourcesBuildPhase"))))
         (build-settings (when target-name
                           (flycheck-swiftx--xcode-build-settings xcproj target-name)))
         (build-products-dir (when target-name
                               (xcode-project-concat-path (flycheck-swiftx--target-build-dir target-name)
                                                          "Build/Products"
                                                          (or flycheck-swiftx-xcode-build-config "Debug")))))
    `(
      ,@(flycheck-swiftx--append-options "-module-name" target-name)
      ,@(flycheck-swiftx--append-options "-target"
                                              (flycheck-swiftx--target build-settings))
      ,@(flycheck-swiftx--append-options "-swift-version"
                                              (flycheck-swiftx--swift-version build-settings))
      ,@(flycheck-swiftx--append-options "-sdk"
                                         (flycheck-swiftx--sdk-path build-settings xcrun-path))
      ,@(flycheck-swiftx--append-options "-import-objc-header"
                                         (flycheck-swiftx--objc-bridging-header xcproj-path build-settings))
      ,@(flycheck-swiftx--objc-inference build-settings)
      ,@(flycheck-swiftx--append-options "-D"
                                         (flycheck-swiftx--gcc-compilation-flags build-settings))
      ;; Other compiler flags
      ,@(flycheck-swiftx--list-option 'OTHER_SWIFT_FLAGS build-settings)
      ;; Search paths
      ,@(flycheck-swiftx--append-options "-F"
                                      (flycheck-swiftx--list-option 'FRAMEWORK_SEARCH_PATHS
                                                                     build-settings))
      ,@(flycheck-swiftx--append-options "-I"
                                      (flycheck-swiftx--list-option 'HEADER_SEARCH_PATHS
                                                                     build-settings))
      ,@(flycheck-swiftx--append-options "-I"
                                      (flycheck-swiftx--list-option 'USER_HEADER_SEARCH_PATHS
                                                                     build-settings))
      ,@(flycheck-swiftx--append-options "-I"
                                      (flycheck-swiftx--list-option 'SYSTEM_HEADER_SEARCH_PATHS
                                                                     build-settings))
      ,@(flycheck-swiftx--append-options "-I"
                                      (flycheck-swiftx--list-option 'SWIFT_INCLUDE_PATHS
                                                                     build-settings))
      ;; Add target build dir to ensure that any framework dependencies are found
      ,@(flycheck-swiftx--append-options "-F" build-products-dir)
      ,@(flycheck-swiftx--append-options "-I" build-products-dir)
      ;; Associated source files, ignoring the file currently being checked.
      ,@(when-let (source-files (flycheck-swiftx--source-files xcproj target-name))
          (remove file-name source-files)))))

(defun flycheck-swiftx--syntax-checking-command ()
  "Return the command to run for Swift syntax checking."
  (let* ((xcrun-path (executable-find "xcrun"))
         (command
    `("swiftc"
      "-frontend"
      "-typecheck"
      ;; Options which require an Xcode project are evaluated together to avoid
      ;; loading the project more than once during a check.
      (eval (let* ((file-name (or load-file-name buffer-file-name)))
              (flycheck-swiftx--swiftc-options file-name ,xcrun-path)))
      ;; Read 'source' file provided by flycheck. Contains current buffer contents.
      ;; This ensures we have a valid filename in errors (see flycheck-swiftx--error-filter).
      "-primary-file" source)))
    (if xcrun-path
        (let ((xcrun-command
               `(,xcrun-path
                 (option "--toolchain" flycheck-swiftx-xcrun-toolchain))))
          (append xcrun-command command))
      command)))

(defun flycheck-swiftx--error-filter (errors)
  "Return filtered ERRORS.
swiftc v5.1.2 now returns errors for all inputs, even though we
specify a -primary-file.  We don't want to see errors for other
files in the current buffer, so we discard them here."
  (let* ((file-name (or load-file-name buffer-file-name)))
    (seq-filter
     (lambda (err)
       (equal file-name (flycheck-error-filename err)))
     errors)))

(flycheck-define-command-checker 'swiftx
  "A Swift syntax checker using Swift compiler frontend.

See URL `https://swift.org/'."
  :command (flycheck-swiftx--syntax-checking-command)
  :standard-input t
  :error-patterns
  '((error line-start "<unknown>:" line
           ": " "error: " (optional (message)) line-end)
    (info line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " "note: " (optional (message)) line-end)
    (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": " "warning: " (optional (message)) line-end)
    (error line-start (or "<stdin>" (file-name)) ":" line ":" column
           ": " "error: " (optional (message)) line-end))
  :error-filter 'flycheck-swiftx--error-filter
  :modes 'swift-mode)

;; Set up Flycheck for Swift.
(add-to-list 'flycheck-checkers 'swiftx)

(add-hook 'swift-mode-hook (lambda () (flycheck-mode)))

(provide 'flycheck-swiftx)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swiftx.el ends here
