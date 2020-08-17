[![MELPA](https://melpa.org/packages/flycheck-swiftx-badge.svg)](https://melpa.org/#/flycheck-swiftx)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# flycheck-swiftx

A Swift syntax checker using Swift compiler frontend, with support for Xcode projects.

## Features

* Apple Swift 5 support.
  If you use the toolchain option, you can use the old version of Swift.

* Xcode projects

  Flycheck-swiftx can parse Xcode projects and use the build settings for the project.
  This means that complex projects, which may include various dependencies, can still be
  typechecked with swiftc.

* For non-Xcode projects provide your own configuration via `flycheck-swiftx-build-options` and `flycheck-swiftx-sources`.

* `xcrun` command support (only on macOS)

## Requirements

* [Flycheck](http://www.flycheck.org/)

## Installation

You can install `flycheck-swiftx.el` from the [MELPA](https://melpa.org/) or the [MELPA Stable](https://stable.melpa.org/) repository with `package.el`.

In your `init.el`

```elisp
;; Not necessary if using MELPA package
(with-eval-after-load 'flycheck
  (require 'flycheck-swiftx))
```

or with `use-package`:

```elisp
(use-package flycheck-swiftx
  :after flycheck)
```

## Configuration

* `flycheck-swiftx-project-type` is set to `'automatic` by default.
  In this mode flycheck-swiftx will search for an Xcode project in the current buffer's directory, or a parent directory.
  If not found it will fall back to using the current flycheck-swiftx configuration.

  Other options are:
  - `'xcode`, which will cause swiftc to fail if no Xcode project is found.
  - `nil` which forces flycheck-swiftx to use the current flycheck-swiftx configuration only.

* `flycheck-swiftx-build-options` can be used to provide command line arguments for swiftc.

* `flycheck-swiftx-sources` can be used to specify a single source directory or list of source files for the project.

*  Configuration variables can be specified via a `.dir-locals.el` placed in the project's root directory.

## FAQ

* Why is there no support for Swift Packages?

  If your project uses a swift packages you should probably use [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with a sourcekit-lsp backend.
  However, for many complex/existing projects, Xcode is still necessary - and swift packages are often not sufficient - hence flycheck-swiftx.

* What about flycheck-swift3?

  Flymake-swiftx is a hard fork of [flycheck-swift3](https://github.com/GyazSquare/flycheck-swift3). The latter does not support Xcode projects.
  I submitted a PR, which has not been merged, so in the end decided to create my own fork and submit it to MELPA.

## License

This software is licensed under the MIT License.

See the LICENSE file for details.
