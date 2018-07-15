;;; cardan-address.el --- Define cardan commands   -*- lexical-binding: t; -*-

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

;; Define a single address detail view.

;;; Code:

(require 'cardan-utils)

(defun prompt-for-search-address ()
  "Prompt user to enter address."
  (interactive)
  (view-address (read-string "Enter address: ")))

(defun format-address-summary (address-response)
  "Format summary from ADDRESS-RESPONSE."
  (let* ((address-data (gethash "Right" address-response))
         (address-hash (gethash "caAddress" address-data))
         (tx-num (gethash "caTxNum"  address-data))
         (balance (string-to-number (get-in-hash '("caBalance" "getCoin") address-data)))
         (transactions (gethash "caTxList" address-data)))
    (concat
      (format "%-16s: " "Address")
      (propertize
        (format "%s" address-hash)
        'face 'cardan-address-hash-face)
      "\n"
      (format "%-16s: " "Final Balance")
      (propertize
        (format "%s" (format-ada (lovelace-to-ada balance)))
        'face 'cardan-total-sent-face)
      "\n\n"
      (propertize (format "Transactions (%d)" tx-num) 'face 'header-line)
      "\n\n"
      (format-transactions transactions))))

(defun view-address (address-hash)
  "View address detail from ADDRESS-HASH."
  (read-address
    address-hash
    (lambda (response)
      (let* ((buffer-name (concat "*Address-" address-hash "*"))
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (read-only-mode 0)
          (erase-buffer)
          (insert (propertize "Address" 'face 'header-line))
          (insert "\n\n\n")
          (insert (format-address-summary response))
          (highlight-regexp address-hash 'cardan-address-hash-highlight-face)
          (read-only-mode 1))))))

;;; _
(provide 'cardan-address)
;; End:
;;; cardan-address.el ends here
