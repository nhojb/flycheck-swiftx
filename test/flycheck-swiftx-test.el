;;; flycheck-swiftx-test.el --- Flycheck Swift: Test cases

;; Copyright (c) 2019 John Buckley <john@olivetoast.com>

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/flycheck-swiftx

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

;; Test cases Flycheck Swift.

;;; Code:

(require 'flycheck-ert)
(require 'flycheck-swiftx)

(message "Running tests on Emacs %s" emacs-version)

(defconst flycheck-swiftx-test-directory
  (let ((filename (if load-in-progress load-file-name (buffer-file-name))))
    (expand-file-name "test/" (locate-dominating-file filename "Cask")))
  "Test suite directory, for resource loading.")

(flycheck-ert-def-checker-test swiftx swift error
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-sdk "macosx")
        (flycheck-swiftx-sources '("A.swift")))
    (flycheck-ert-should-syntax-check
     "broken.swift" 'swift-mode
     '(1 11 error "use of undeclared type 'X'" :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift error-info
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     "A.swift" 'swift-mode
     '(5 18 info "protocol requires nested type 'Assoc'; do you want to add it?"
         :checker swiftx)
     '(8 8 error "type 'A' does not conform to protocol 'P'" :checker swiftx)
     '(9 13 info "possibly intended match 'A.Assoc' (aka 'Int') does not conform to 'PHelper'"
         :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift error-unknown
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-build-options '("-import-objc-header" "hello-bridge-header.h")))
    (flycheck-ert-should-syntax-check
     "hello.swift" 'swift-mode
     '(0 nil error "failed to import bridging header 'hello-bridge-header.h'"
         :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift error-warning-info
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     "unknowable.swift" 'swift-mode
     '(8 3 warning "result of 'Int' initializer is unused" :checker swiftx)
     '(17 6 info "found this candidate" :checker swiftx)
     '(18 6 info "found this candidate" :checker swiftx)
     '(22 29 error "ambiguous use of 'ovlLitB'" :checker swiftx)
     '(46 12 error "argument type 'Double' does not conform to expected type 'CanWibble'"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift warning
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     "strange-characters.swift" 'swift-mode
     '(4 5 warning "nul character embedded in middle of file" :checker swiftx)
     '(5 5 warning "nul character embedded in middle of file" :checker swiftx)
     '(6 15 warning "nul character embedded in middle of file"
         :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift warning-info
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     "diag_unreachable_after_return.swift" 'swift-mode
     '(7 3 warning "expression following 'return' is treated as an argument of the 'return'"
         :checker swiftx)
     '(7 3 info "indent the expression to silence this warning"
         :checker swiftx)
     '(13 3 warning "expression following 'return' is treated as an argument of the 'return'"
          :checker swiftx)
     '(13 3 info "indent the expression to silence this warning"
          :checker swiftx)
     '(19 5 warning "expression following 'return' is treated as an argument of the 'return'"
          :checker swiftx)
     '(19 5 info "indent the expression to silence this warning"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift swift-version
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-build-options '("-swift-version" "3")))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(0 nil error "invalid value '3' in '-swift-version 3'" :checker swiftx)
     '(6 10 info "add '@objc' to make this declaration overridable"
         :checker swiftx)
     '(10 19 error "overriding non-@objc declarations from extensions is not supported"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift swiftx-objc-inference-default
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to make this declaration overridable"
         :checker swiftx)
     '(10 19 error "overriding non-@objc declarations from extensions is not supported"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift swiftx-objc-inference-on
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-build-options '("-enable-swift3-objc-inference" "-warn-swift3-objc-inference-minimal")))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to expose this instance method to Objective-C"
         :checker swiftx)
     '(10 19 warning "override of instance method 'extMethod()' from extension of 'MySuperclass' depends on deprecated inference of '@objc'"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift swiftx-objc-inference-off
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-build-options '("-disable-swift3-objc-inference")))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to make this declaration overridable"
         :checker swiftx)
     '(10 19 error "overriding non-@objc declarations from extensions is not supported"
          :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift warn-implicit-overrides
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-build-options '("-warn-implicit-overrides")))
    (flycheck-ert-should-syntax-check
     "warn_override.swift" 'swift-mode
     '(2 18 info "'A' declared here" :checker swiftx)
     '(4 8 info "overridden declaration is here" :checker swiftx)
     '(6 7 info "overridden declaration is here" :checker swiftx)
     '(10 18 warning "redeclaration of associated type 'A' from protocol 'P0' is better expressed as a 'where' clause on the protocol"
          :checker swiftx)
     '(12 8 warning "implicit override should be marked with 'override' or suppressed with '@_nonoverride'"
          :checker swiftx)
     '(14 7 warning "implicit override should be marked with 'override' or suppressed with '@_nonoverride'"
          :checker swiftx))))

(defun flycheck-swiftx-test--sdk-version ()
  "Return SDK version obtained via xcrun."
  (string-trim (shell-command-to-string
                (format "%s --show-sdk-version" (executable-find "xcrun")))))

(flycheck-ert-def-checker-test swiftx swift invalid-sdk
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic)
        (flycheck-swiftx-sdk "windows"))
    (flycheck-ert-should-syntax-check
     "A.swift" 'swift-mode
     `(0 nil error ,(format "unable to load standard library for target 'x86_64-apple-macosx%s'"
                            (flycheck-swiftx-test--sdk-version))
         :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift no-xcode-project
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'xcode))
    (flycheck-ert-should-syntax-check
     "A.swift" 'swift-mode
     `(0 nil error ,(format "unable to load standard library for target 'x86_64-apple-macosx%s'"
                            (flycheck-swiftx-test--sdk-version))
         :checker swiftx))))

(flycheck-ert-def-checker-test swiftx swift swiftx-appdelegate-no-project
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'nil))
    (flycheck-ert-should-syntax-check
     "TestApp/TestApp/AppDelegate.swift" 'swift-mode
     '(1 1 info "top-level code defined in this source file"
         :checker swiftx)
     '(11 1 error "'NSApplicationMain' attribute cannot be used in a module that contains top-level code"
          :checker swiftx)
     '(14 25 error "use of undeclared type 'ViewController'"
          :checker swiftx))))

;; Also timing out in emacs...

(flycheck-ert-def-checker-test swiftx swift swiftx-appdelegate
  """Verify that source files are imported from input file's directory."""
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'automatic))
    (flycheck-ert-should-syntax-check
     ;; No error because we did not limit inputs to "AppDelegate.swift"
     "TestApp/TestApp/AppDelegate.swift" 'swift-mode)))

(flycheck-ert-def-checker-test swiftx swift swiftx-viewcontroller
  (let ((flycheck-checkers '(swiftx))
        (flycheck-swiftx-project-type 'xcode))
    (flycheck-ert-should-syntax-check
     "TestApp/TestApp/ViewController.swift" 'swift-mode
     '(13 15 error "use of unresolved identifier 'bar'"
          :checker swiftx))))

(flycheck-ert-initialize flycheck-swiftx-test-directory)

(provide 'flycheck-swiftx-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swiftx-test.el ends here