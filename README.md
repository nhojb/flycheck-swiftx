# flycheck-swiftx

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/flycheck-swift3-badge.svg)](https://melpa.org/#/flycheck-swiftx)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-swift3-badge.svg)](https://stable.melpa.org/#/flycheck-swiftx)

A Swift syntax checker using Swift compiler frontend, with support for Xcode projects.

## Features

* Apple Swift 5 support.
  If you use the toolchain option, you can use the old version of Swift.

* Xcode projects
  Flycheck-swiftx can parse Xcode projects and use the build settings for the project.
  This means that complex projects, which include various dependencies, can still be typechecked successfully
  with the swiftc compiler.
  
* For non-Xcode projects provide your own build settings via a .flycheck-swiftx file in the project's root.

* The `xcrun` command support (only on macOS)

## Requirements

* [Flycheck](http://www.flycheck.org/)

## Installation

You can install `flycheck-swiftx.el` from the [MELPA](https://melpa.org/) or the [MELPA Stable](https://stable.melpa.org/) repository with `package.el`.

In your `init.el`:

```elisp
(require 'flycheck-swiftx) ; Not necessary if using ELPA package
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))
```

or with `use-package`:

```elisp
(use-package flycheck-swiftx
  :after flycheck)
```

## FAQ

* Why is there no support for Swift Packages?
If your project uses a swift packages you should probably use [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with a sourcekit-lsp backend.
However, for many complex/existing projects, Xcode is still necessary - and swift packages are often not sufficient - hence flycheck-swiftx.

* What about flycheck-swift3?
flymake-swiftx is a hard fork of [flycheck-swift3](https://github.com/GyazSquare/flycheck-swift3). The latter does not support Xcode projects.
I submitted a PR, which has not been merged, so in the end I decided to create my own fork and submit it to MELPA.

## License

This software is licensed under the MIT License.

See the LICENSE file for details.
