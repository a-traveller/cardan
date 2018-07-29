;;; cardan-saved-addresses.el --- Define saved address view.   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Atraveller

;; Author: Atraveller <atraveller@protonmail.com>

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

(require 'cardan-utils)

(define-derived-mode cardan-saved-addresses-mode fundamental-mode "cardan-saved-addresses"
  "Major mode for saved addresses.")

(defun cardan-format-saved-address-row (address-hash label)
  "Format saved address row FROM ADDRESS-HASH and LABEL."
  (concat
    (cardan-format-address-hash address-hash)
    (format "[%s]" label)))

(defun cardan-view-saved-addresses ()
  "View saved addresses."
  (let* ((config (cardan-read-sensitive-config))
         (addresses (gethash "addresses" config))
         (buffer-name "*Saved Addresses*")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (read-only-mode 0)
      (cardan-saved-addresses-mode)
      (insert (propertize "Saved Addresses" 'face 'header-line))
      (insert "\n\n\n")
      (dolist (address-entry addresses)
        (insert (cardan-format-saved-address-row
                  (gethash "hash" address-entry)
                  (gethash "label" address-entry)))
        (insert "\n"))
      (read-only-mode 1))))

;;; _
(provide 'cardan-saved-addresses)
;; End:
;;; cardan-saved-addresses.el ends here
