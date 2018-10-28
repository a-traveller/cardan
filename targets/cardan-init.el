;;; cardan-init.el --- Initialize cardan   -*- lexical-binding: t; -*-
;; Version: 0.1.0

;; Copyright (C) 2018 Atraveller

;; Author: Atraveller <atraveller@protonmail.com>
;; Homepage: https://gitlab.com/atraveller

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;;; Code:

(add-to-list 'load-path default-directory)
(require 'cardan)
(mapc #'byte-compile-file '("cardan.el" "cardan-saved-addresses.el" "cardan-slots.el" "cardan-utils.el"))
;;; cardan-init.el ends here
