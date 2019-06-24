;;; cardan.el --- Define cardan commands   -*- lexical-binding: t; -*-
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

;; Define cardan commands.

;;; Code:

(require 'cardan-saved-addresses)
(require 'cardan-slots)
(require 'cardan-utils)

(defun cardan-slots ()
  "Action for viewing slow."
  (interactive)
  (cardan-view-slots))

(defun cardan-saved-addresses ()
  "ACtion for saved-addresses.  CONFIG-FILE-PATH is a json file."
  (interactive)
  (cardan-view-saved-addresses (read-file-name "Config file:")))

(defun cardan-search-address ()
  "Action for searching address."
  (interactive)
  (cardan-view-address (read-string "Enter address: ")))

(global-set-key (kbd "C-c c d o") 'cardan-saved-addresses)
(global-set-key (kbd "C-c c d f") 'cardan-search-address)
(global-set-key (kbd "C-c c d s") 'cardan-slots)

;;; _
(provide 'cardan)
;; End:
;;; cardan.el ends here
