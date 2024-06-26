Puppet Mode
===========

[![License GPL 3][badge-license]][copying]
[![Build Status](https://github.com/smoeding/puppet-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/puppet-mode/actions/workflows/CI.yaml)

Puppet Mode lets you edit [Puppet][] manifests with [GNU Emacs][].

Puppet Mode is a major mode for [GNU Emacs][] which adds support for the
[Puppet][] language.  Puppet is a system provisioning and configuration tool by
Puppetlabs Inc.  This mode supports Puppet 3 and later.  Puppet 2 is not
explicitly supported anymore, but should mostly work.

The official [Puppet Mode](https://github.com/voxpupuli/puppet-mode) is
maintained by Vox Pupuli.  I created this fork to implement features that
depend on a more current Emacs release.

This mode needs GNU Emacs 26.2.  It will **not** work with GNU Emacs 26.1 or
below, or with other flavors of Emacs (e.g. XEmacs).

Features
--------

1. Syntax highlighting
2. Indentation and alignment of parameter lists, expressions and statements
3. Tag navigation (aka `imenu`)
4. Cross-reference navigation (aka `xref`) to classes, defined types, data
   types or functions defined in other modules
5. Manual validation and linting of manifests (see [Flycheck][] for on-the-fly
   validation and linting)
6. Skeletons for many standard Puppet statements and resource declarations
7. Integration with [Puppet Debugger][]

Installation
------------

Copy `puppet-mode.el` into your `load-path` and byte compile it.

Usage
-----

Just visit Puppet manifests.  The major mode is enabled automatically for Puppet
manifests with the extension `.pp`.

The following key bindings are available in Puppet Mode:

Key                  | Command
---------------------|--------------------------------------------------
<kbd>C-M-a</kbd>     | Move to the beginning of the current block
<kbd>C-M-e</kbd>     | Move to the end of the current block
<kbd>C-c C-a</kbd>   | Align parameters in the current block
<kbd>C-c C-'</kbd>   | Toggle string quoting between single and double
<kbd>C-c C-;</kbd>   | Blank the string at point
<kbd>C-c C-j</kbd>   | Jump to a `class`, `define`, variable or resource
<kbd>C-c C-c</kbd>   | Apply the current manifest in dry-run mode
<kbd>C-c C-v</kbd>   | Validate the syntax of the current manifest
<kbd>C-c C-l</kbd>   | Check the current manifest for semantic issues
<kbd>C-c C-z</kbd>   | Launch a puppet-debugger REPL
<kbd>C-c C-r</kbd>   | Send the currently marked region to the REPL
<kbd>C-c C-b</kbd>   | Send the current buffer to the REPL
<kbd>C-c C-k c</kbd> | Insert `class` definition skeleton
<kbd>C-c C-k d</kbd> | Insert `define` definition skeleton
<kbd>C-c C-k n</kbd> | Insert `node` definition skeleton
<kbd>C-c C-k i</kbd> | Insert `if` statement skeleton
<kbd>C-c C-k e</kbd> | Insert `elsif` statement skeleton
<kbd>C-c C-k o</kbd> | Insert `else` statement skeleton
<kbd>C-c C-k u</kbd> | Insert `unless` statement skeleton
<kbd>C-c C-k s</kbd> | Insert `case` statement skeleton
<kbd>C-c C-k ?</kbd> | Insert `selector` statement skeleton
<kbd>C-c C-t a</kbd> | Insert `anchor` resource skeleton
<kbd>C-c C-t c</kbd> | Insert `class` resource skeleton
<kbd>C-c C-t e</kbd> | Insert `exec` resource skeleton
<kbd>C-c C-t f</kbd> | Insert `file` resource skeleton
<kbd>C-c C-t g</kbd> | Insert `group` resource skeleton
<kbd>C-c C-t h</kbd> | Insert `host` resource skeleton
<kbd>C-c C-t n</kbd> | Insert `notify` resource skeleton
<kbd>C-c C-t p</kbd> | Insert `package` resource skeleton
<kbd>C-c C-t s</kbd> | Insert `service` resource skeleton
<kbd>C-c C-t u</kbd> | Insert `user` resource skeleton
<kbd>M-.</kbd>       | Jump to the resource definition at point
<kbd>M-,</kbd>       | Jump back after visiting a resource definition


For the integration with puppet-debugger to work, the puppet-debugger gem needs
to be installed and available in your `$PATH`.
See [the instructions][puppet debugger install instructions] on
puppet-debugger's repository on how to install it.

Use `M-x customize-group RET puppet` to customize Puppet Mode.

License
-------

Puppet Mode is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Puppet Mode is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

See [`COPYING`][copying] for the complete license.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[COPYING]: https://github.com/voxpupuli/puppet-mode/blob/master/COPYING
[travis]: https://travis-ci.org/voxpupuli/puppet-mode
[badge-travis]: https://travis-ci.org/voxpupuli/puppet-mode.svg?branch=master
[Puppet]: http://docs.puppetlabs.com/
[GNU Emacs]: https://www.gnu.org/software/emacs/
[Flycheck]: http://wwww.flycheck.org
[Puppet Debugger]: https://github.com/nwops/puppet-debugger
[MELPA]: https://melpa.org/
[MELPA Stable]: https://stable.melpa.org
[Cask]: http://cask.github.io/
[puppet debugger install instructions]: https://github.com/nwops/puppet-debugger/#installation
[Issue tracker]: https://github.com/voxpupuli/puppet-mode/issues
[Github]: https://github.com/voxpupuli/puppet-mode
