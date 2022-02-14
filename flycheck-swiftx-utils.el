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

(require 'flycheck)
(require 's)

(defun flycheck-swiftx--locate-dominating-file (file name)
  "Starting at FILE, look up directory hierarchy for directory containing NAME."
  (when-let (path (locate-dominating-file file name))
    (expand-file-name path)))

(defun flycheck-swiftx--sdk-name (sdk-name)
  "Return the actual sdk name for SDK-NAME."
  (if (equal sdk-name "iphoneos") "iphonesimulator" sdk-name))

(defun flycheck-swiftx--xcrun-sdk-path (path-type &optional sdk-name)
  "Return a path from `xcrun --sdk ${SDK-NAME} ${PATH-TYPE}'."
  (when-let ((xcrun-path (executable-find "xcrun"))
             (xcrun-cmd (if (eq path-type 'platform)
                            "--show-sdk-platform-path"
                          "--show-sdk-path"))
             (xcrun-sdk (if (equal sdk-name "macos") "macosx" sdk-name)))
      (string-trim (shell-command-to-string
                    (format "%s %s %s" xcrun-path xcrun-cmd
                            (if xcrun-sdk (format "--sdk %s" xcrun-sdk) ""))))))

(defun flycheck-swiftx-arch ()
    "Return the current CPU architecture as a string."
  (if (s-contains-p "aarch64" system-configuration) "arm64" "x86_64"))

;; Cache

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

;; Tests

(defconst flycheck-swiftx--test-re "[tT]ests?$"
  "Regular expression used to match test files or targets.")

(defun flycheck-swiftx--maybe-test (file-name)
  "Return t if FILE-NAME is probably a test file."
  (or (string-match-p flycheck-swiftx--test-re (file-name-base file-name))
      (string-match-p flycheck-swiftx--test-re (directory-file-name (file-name-directory file-name)))))

(defun flycheck-swiftx--path-latest (path1 path2)
  "Return the path from PATH1 and PATH2 with the latest modification date."
  (let ((modtime1 (file-attribute-modification-time (file-attributes path1)))
        (modtime2 (file-attribute-modification-time (file-attributes path2))))
    (cond ((and modtime1 modtime2)
           (if (time-less-p modtime1 modtime2) path2 path1))
          (modtime1 path1)
          (modtime2 path2))))

(provide 'flycheck-swiftx-utils)
;;; flycheck-swiftx-utils.el ends here
