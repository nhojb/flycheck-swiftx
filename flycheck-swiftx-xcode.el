;;; flycheck-swiftx-xcode.el --- A package for reading Swift package files.

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

;; This package supports reading Xcode projects.
;;

;;; Code:

(require 'flycheck-swiftx-utils)
(require 'xcode-project)

;; Project

(defun flycheck-swiftx-find-xcproj-path (file-name)
  "Search FILE-NAME directory and parent directories for a Xcode project file.

Prefers an Xcode project with the same name as FILE-NAME's parent directory."
  (let* (projects
         (directory (file-name-directory file-name))
         (base-name (format "%s.xcodeproj" (directory-file-name directory))))
    (when (file-directory-p directory)
      (while (and (not projects) (not (equal directory "/")))
        (setq projects (directory-files directory t ".*\\.xcodeproj$" t))
        (setq directory (file-name-directory (directory-file-name directory)))))
    ;; Prefer project matching "<base-name>.xcodeproj"
    (or (seq-find (lambda (path) (equal path base-name)) projects)
        (car projects))))

(defun flycheck-swiftx-load-xcode-project (xcproj-path)
  "Load and return the Xcode project found at XCPROJ-PATH.

If the parsed Xcode project is found in our cache, return that.
Otherwise read from disk, cache and return the project."
  (setq xcproj-path (expand-file-name xcproj-path))
  (when (and xcproj-path (file-directory-p xcproj-path))
    (or (flycheck-swiftx--read-xcode-project-cache xcproj-path)
        (when-let (proj (xcode-project-read xcproj-path))
          (flycheck-swiftx--write-xcode-project-cache proj xcproj-path)
          proj))))

(defun flycheck-swiftx-xcode-build-settings (xcproj target-name build-config)
  "Return build settings for the specified XCPROJ, TARGET-NAME and BUILD-CONFIG."
  (let ((config-name (or build-config "Debug")))
    (when xcproj
      (alist-get 'buildSettings (xcode-project-build-config xcproj
                                                            config-name
                                                            target-name)))))

(defun flycheck-swiftx-find-xcworkspace-path (directory-or-file)
  "Search DIRECTORY-OR-FILE and parent directories for an Xcode workspace file.
Returns the path to the Xcode workspace, or nil if not found."
  (when directory-or-file
    (let (xcworkspace
          (directory (if (file-directory-p directory-or-file)
                         directory-or-file
                       (file-name-directory directory-or-file))))
      (setq directory (expand-file-name directory))
      (while (and (not xcworkspace) (not (equal directory "/")))
        (setq xcworkspace (directory-files directory t ".*\\.xcworkspace$" nil))
        (setq directory (file-name-directory (directory-file-name directory))))
      (car xcworkspace))))

(defun flycheck-swiftx-xcode-source-files (xcproj target-name &optional file-name)
  "Return the swift source files for the specified XCPROJ TARGET-NAME.

If FILE-NAME is non-nil it is removed from the list of source files."
  (remove file-name (xcode-project-build-file-paths xcproj
                                                    target-name
                                                    "PBXSourcesBuildPhase"
                                                    (lambda (file)
                                                      (xcode-project-file-ref-extension-p file "swift"))
                                                    'absolute)))

(defun flycheck-swiftx-xcode-target-name (file-name xcproj suggested-target)
  "Return the most appropriate XCPROJ target for FILE-NAME.
If SUGGESTED-TARGET is a valid target, return that.
Otherwise returns the first target found."
  (let* ((targets (xcode-project-target-names-for-file xcproj file-name "PBXSourcesBuildPhase"))
         (prefer-tests (flycheck-swiftx--maybe-test file-name)))
    (if (member suggested-target targets)
        suggested-target
      (seq-find (lambda (target) (if prefer-tests
                                     (string-match-p flycheck-swiftx--test-re target)
                                   (not (string-match-p flycheck-swiftx--test-re target))))
                targets
                (car targets)))))

(defun flycheck-swiftx-xcode-build-dir (xcproj-path &optional target-name)
  "Return the DerivedData directory for XCPROJ-PATH and TARGET-NAME.
Uses heuristics to locate the build dir in
 ~/Library/Developer/Xcode/DerivedData/."
  (when xcproj-path
    (let* ((build-root (expand-file-name "~/Library/Developer/Xcode/DerivedData/"))
           (build-prefix (or target-name (file-name-base xcproj-path)))
           (results (directory-files-and-attributes build-root t (concat build-prefix "\\-[a-z]+"))))
      (unless results
        ;; Check for a xcworkspace in current/parent directory:
        (when-let (xcworkspace (flycheck-swiftx-find-xcworkspace-path (file-name-directory xcproj-path)))
          (setq build-prefix (file-name-base xcworkspace))
          (setq results (directory-files-and-attributes build-root (concat build-prefix "\\-[a-z]+")))))
      (when results
        (file-name-concat (car (seq-reduce (lambda (result item)
                                             (if (time-less-p (nth 6 result) (nth 6 item))
                                                 item
                                               result))
                                           results (car results))) "Build")))))

(defun flycheck-swiftx-xcode-build-products-dir (xcproj-path build-config &optional target-name)
  "Return build Products directory for XCPROJ-PATH, BUILD-CONFIG and TARGET-NAME."
  (when-let ((derived-data-dir (flycheck-swiftx-xcode-build-dir xcproj-path target-name)))
    (file-name-concat derived-data-dir "Products" (or build-config "Debug"))))

(defun flycheck-swiftx-xcode-generated-modulemaps-dir (xcproj-path &optional target-name)
  "Return generated modulemaps directory for XCPROJ-PATH and TARGET-NAME."
  (when-let ((derived-data-dir (flycheck-swiftx-xcode-build-dir xcproj-path target-name)))
    (file-name-concat derived-data-dir "Intermediates.noindex/GeneratedModuleMaps")))

;; Build Settings

(defun flycheck-swiftx-xcode-deployment-target (build-settings)
  "Return the build platform target for BUILD-SETTINGS."
  (when build-settings
      (let ((macos-target (alist-get 'MACOSX_DEPLOYMENT_TARGET build-settings))
            (iphoneos-target (alist-get 'IPHONEOS_DEPLOYMENT_TARGET build-settings)))
        (cond (macos-target
               (format "x86_64-apple-macosx%s" macos-target))
              (iphoneos-target
               ;; We never want "arm*" for flycheck.
               (format "x86_64-apple-ios%s" iphoneos-target))))))

(defun flycheck-swiftx-xcode-swift-version (build-settings)
  "Return the swift version for BUILD-SETTINGS."
  (when-let (swift-version (alist-get 'SWIFT_VERSION build-settings))
    ;; -swift-version appears to require integers (4 not 4.0 etc).
    ;; Major versions, such as 4.2, are however valid.
    (if (equal (fround swift-version) swift-version)
        (number-to-string (truncate swift-version))
      (number-to-string swift-version))))

(defun flycheck-swiftx-xcode-sdk-root (build-settings)
  "Return SDK root name for BUILD-SETTINGS."
  (alist-get 'SDKROOT build-settings))

(defun flycheck-swiftx-xcode-objc-bridging-header (build-settings xcproj-path)
  "Return path to Objc bridging header if found in BUILD-SETTINGS.

Header path is interpreted relative to XCPROJ-PATH."
  (when-let (header (flycheck-swiftx-xcode-string-option 'SWIFT_OBJC_BRIDGING_HEADER build-settings))
    (expand-file-name header (file-name-directory xcproj-path))))

(defun flycheck-swiftx-xcode-objc-inference (build-settings)
  "Return objc inference compiler flags for BUILD-SETTINGS."
  (when-let (objc-inference (alist-get 'SWIFT_SWIFT3_OBJC_INFERENCE build-settings))
    (cond ((equal objc-inference "On")
           '("-enable-swift3-objc-inference"
             "-warn-swift3-objc-inference-minimal"))
          ((equal objc-inference "Off")
           '("-disable-swift3-objc-inference")))))

(defun flycheck-swiftx-xcode-gcc-compilation-flags (build-settings)
  "Return a list of GCC conditional compilation flags found in BUILD-SETTINGS."
  (when-let (preprocessor-defs (alist-get 'GCC_PREPROCESSOR_DEFINITIONS build-settings))
    (when (stringp preprocessor-defs)
      (setq preprocessor-defs (vector preprocessor-defs)))
    (seq-concatenate 'vector
                     preprocessor-defs
                     (alist-get 'GCC_PREPROCESSOR_DEFINITIONS_NOT_USED_IN_PRECOMPS build-settings))))

(defun flycheck-swiftx-xcode-options-testing (build-settings)
  "Return swiftc options suitable for a test target using BUILD-SETTINGS."
  (if-let ((platform-path (flycheck-swiftx--sdk-platform-path build-settings)))
      (list "-enable-testing" "-F" (file-name-concat platform-path "Developer/Library/Frameworks"))
    '("-enable-testing")))

(defun flycheck-swiftx-xcode-string-option (key build-settings)
  "Return the string value for KEY in BUILD-SETTINGS."
  (when-let* ((value (alist-get key build-settings))
              (stringp value))
    value))

(defun flycheck-swiftx-xcode-list-option (key build-settings)
  "Return a list of options for KEY in BUILD-SETTINGS."
  (when-let (values (alist-get key build-settings))
    (if (stringp values)
        (seq-into (split-string values) 'vector)
      values)))

(defun flycheck-swiftx-xcode-path-option (key build-settings build-env)
  "Return list of options for KEY in BUILD-SETTINGS expanding with BUILD-ENV."
  (when-let ((values (flycheck-swiftx-xcode-list-option key build-settings)))
    (seq-map (lambda (value)
               (seq-reduce (lambda (result env)
                             (replace-regexp-in-string (format "$(?%s)?" (car env)) (cdr env) result t t))
                           build-env
                           value))
             values)))

;; Private

(defun flycheck-swiftx--xcodeproj-modtime (xcproj-path)
  "Return the modification time for XCPROJ-PATH."
  (setq xcproj-path (expand-file-name xcproj-path))
  (if (and xcproj-path (file-directory-p xcproj-path))
      (file-attribute-modification-time (file-attributes (xcode-project-concat-path xcproj-path "project.pbxproj")))))

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

(defun flycheck-swiftx--sdk-platform-path (&optional build-settings)
  "Return the sdk platform path for BUILD-SETTINGS.
If no valid sdk is found, return the default sdk platform path."
  (flycheck-swiftx--xcrun-sdk-path 'platform
                                   (flycheck-swiftx--sdk-name (flycheck-swiftx-xcode-sdk-root build-settings))))

(provide 'flycheck-swiftx-xcode)
;;; flycheck-swiftx-xcode.el ends here
