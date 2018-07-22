;;; cardan-slot.el --- Define cardan commands   -*- lexical-binding: t; -*-

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

;; Define a single slot detail view.

;;; Code:

(require 'cardan-utils)

(defun format-output (output)
  "Format OUTPUT data."
  (let ((address (car output))
         (sent (string-to-number (gethash "getCoin" (car (cdr output))))))
    (concat
      (propertize
        (format "%20s" (format-ada (lovelace-to-ada sent)))
        'face 'cardan-total-sent-face)
      " => "
      (propertize address 'face 'cardan-address-hash-face))))

(defun format-slot-summary (slot-response)
  "Format slot summary from blocks/summary api SLOT-RESPONSE."
  (let* ((slot-data (get-in-hash '("Right" "cbsEntry") slot-response))
         (epoch-num (get-epoch-num slot-data))
         (slot-num (get-slot-num slot-data))
         (total-sent (get-total-sent slot-data))
         (fee (get-fee slot-data))
         (block-hash (get-block-hash slot-data))
         (previous-block-hash (get-in-hash '("Right" "cbsPrevHash") slot-response))
         (next-block-hash (get-in-hash '("Right" "cbsNextHash") slot-response))
         (merkle-root (get-in-hash '("Right" "cbsMerkleRoot") slot-response)))
    (concat
      (format "%-16s: " "Epoch")
      (propertize (number-to-string epoch-num) 'face 'cardan-epoch-slot-face)
      "\n"
      (format "%-16s: " "Slot")
      (propertize (number-to-string slot-num) 'face 'cardan-epoch-slot-face)
      "\n"
      (format "%-16s: " "Total Output")
      (propertize
        (format "%s" (format-ada (lovelace-to-ada total-sent)))
        'face 'cardan-total-sent-face)
      "\n"
      (format "%-16s: " "Fee")
      (propertize
        (format "%s" (format-ada (lovelace-to-ada fee)))
        'face 'cardan-fee-face)
      "\n"
      (format "%-16s: " "Hash")
      (propertize block-hash 'face 'cardan-hash-face)
      "\n"
      (format "%-16s: " "Previous slot")
      (propertize previous-block-hash 'face 'cardan-hash-face)
      "\n"
      (format "%-16s: " "Next slot")
      (propertize next-block-hash 'face 'cardan-hash-face)
      "\n"
      (format "%-16s: " "Merkle root")
      (propertize merkle-root 'face 'cardan-hash-face))))

(defun load-block-summary-and-transactions (block-hash callback)
  "Load the block summary and its transactions from BLOCK-HASH and CALLBACK."
  (read-block-summary
    block-hash
    (lambda (slot-response)
      (list-block-transactions
        block-hash
        (lambda (transactions-response)
          (funcall callback slot-response transactions-response))))))

(defun view-slot (block-hash)
  "View detail from BLOCK-HASH."
  (load-block-summary-and-transactions
    block-hash
    (lambda (slot-response transactions-response)
      (let* ((slot-data (get-in-hash '("Right" "cbsEntry") slot-response))
             (transactions (gethash "Right" transactions-response))
             (tx-num (gethash "cbeTxNum" slot-data))
             (buffer-name (concat "*Slot-" block-hash "*"))
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (read-only-mode 0)
          (erase-buffer)
          (insert (propertize "Slot" 'face 'header-line))
          (insert "\n\n\n")
          (insert (format-slot-summary slot-response))
          (insert "\n\n\n")
          (insert (propertize (format "Transactions (%d)" tx-num) 'face 'header-line))
          (insert "\n\n\n")
          (insert (format-transactions transactions))
          (read-only-mode 1))))))

;;; _
(provide 'cardan-slot)
;; End:
;;; cardan-slot.el ends here
