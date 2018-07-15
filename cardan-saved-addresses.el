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

(defvar cardan-saved-addresses-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'view-address-from-point)
    map)
  "Keymap for `cardan-saved-addresses-mode'.")

(defun view-address-from-point ()
  "View view address detail by reading text-properties at point."
  (interactive)
  (let* ((pos (point))
         (address-hash (get-text-property pos 'address-hash)))
    (view-address address-hash)))

(defun format-saved-address-row (address-hash label)
  "Format saved address row FROM ADDRESS-HASH and LABEL."
  (concat
    (propertize
      (format "%s" address-hash)
      'keymap cardan-saved-addresses-mode-map
      'face 'cardan-address-hash-face
      'address-hash address-hash)
    (format "[%s]" label)))

(defun view-saved-addresses ()
  "View saved addresses."
  (let* ((config (read-sensitive-config))
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
        (insert (format-saved-address-row
                  (gethash "hash" address-entry)
                  (gethash "label" address-entry)))
        (insert "\n"))
      (read-only-mode 1))))

;;; _
(provide 'cardan-saved-addresses)
;; End:
;;; cardan-saved-addresses.el ends here
