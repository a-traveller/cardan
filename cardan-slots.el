;;; cardan-slots.el --- Define cardan commands   -*- lexical-binding: t; -*-

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

;; Define slots view.

;;; Code:

(require 'cardan-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; slot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cardan-format-slot-summary (slot-response)
  "Format slot summary from blocks/summary api SLOT-RESPONSE."
  (let* ((slot-data (cardan-get-in-hash '("Right" "cbsEntry") slot-response))
         (epoch-num (cardan-get-epoch-num slot-data))
         (slot-num (cardan-get-slot-num slot-data))
         (total-sent (cardan-get-total-sent slot-data))
         (fee (cardan-get-fee slot-data))
         (block-hash (cardan-get-block-hash slot-data))
         (previous-block-hash (cardan-get-in-hash '("Right" "cbsPrevHash") slot-response))
         (next-block-hash (cardan-get-in-hash '("Right" "cbsNextHash") slot-response))
         (merkle-root (cardan-get-in-hash '("Right" "cbsMerkleRoot") slot-response)))
    (concat
      (format "%-16s: " "Epoch")
      (propertize (number-to-string epoch-num) 'face 'cardan-epoch-slot-face)
      "\n"
      (format "%-16s: " "Slot")
      (propertize (number-to-string slot-num) 'face 'cardan-epoch-slot-face)
      "\n"
      (format "%-16s: " "Total Output")
      (propertize
        (format "%s" (cardan-format-ada (cardan-lovelace-to-ada total-sent)))
        'face 'cardan-total-sent-face)
      "\n"
      (format "%-16s: " "Fee")
      (propertize
        (format "%s" (cardan-format-ada (cardan-lovelace-to-ada fee)))
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

(defun cardan-load-block-summary-and-transactions (block-hash callback)
  "Load the block summary and its transactions from BLOCK-HASH and CALLBACK."
  (cardan-read-block-summary
    block-hash
    (lambda (slot-response)
      (cardan-list-block-transactions
        block-hash
        (lambda (transactions-response)
          (funcall callback slot-response transactions-response))))))

(defun cardan-view-slot (block-hash)
  "View detail from BLOCK-HASH."
  (cardan-load-block-summary-and-transactions
    block-hash
    (lambda (slot-response transactions-response)
      (let* ((slot-data (cardan-get-in-hash '("Right" "cbsEntry") slot-response))
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
          (insert (cardan-format-slot-summary slot-response))
          (insert "\n\n\n")
          (insert (propertize (format "Transactions (%d)" tx-num) 'face 'header-line))
          (insert "\n\n\n")
          (insert (cardan-format-transactions transactions))
          (read-only-mode 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar cardan-slots-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'cardan-do-load-more-slots)
    (define-key map (kbd "q") 'kill-current-buffer)
    map)
  "Keymap for `cardan-slots-mode'.")

(defvar cardan-slot-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cardan-view-slot-from-point)
    map)
  "Keymap for `cardan-slot-row'.")

(defun cardan-do-load-more-slots ()
  "Click on load more button programmatically."
  (interactive)
  (push-button (- (point-max) 1)))

(define-derived-mode cardan-slots-mode fundamental-mode "cardan-slots"
  "Major mode for slots.")

(defun cardan-view-slot-from-point ()
  "View slot by reading text-properties at point."
  (interactive)
  (let* ((pos (point))
         (block-hash (get-text-property pos 'block-hash)))
    (cardan-view-slot block-hash)))

(defun cardan-format-slot (slot-data)
  "Format hash-table SLOT-DATA."
  (let* ((epoch-num (cardan-get-epoch-num slot-data))
         (slot-num (cardan-get-slot-num slot-data))
         (time-issued (gethash "cbeTimeIssued" slot-data))
         (tx-num (gethash "cbeTxNum" slot-data))
         (total-sent (string-to-number (cardan-get-in-hash '("cbeTotalSent" "getCoin") slot-data)))
         (slot-leader (substring (gethash "cbeBlockLead" slot-data) 0 8))
         (bytes (gethash "cbeSize" slot-data))
         (column-gap "  "))
    (concat
      (propertize
        (format "[%d, %d]" epoch-num slot-num)
        'face 'cardan-epoch-slot-face)
      column-gap
      (propertize
        (format "<%s>" (cardan-format-unix-timestamp time-issued))
        'face 'cardan-time-issued-face)
      column-gap
      (propertize
        (format "%5sTx" tx-num)
        'face 'cardan-tx-num-face)
      column-gap
      (propertize
        (format "%20s" (cardan-format-ada (cardan-lovelace-to-ada total-sent)))
        'face 'cardan-total-sent-face)
      column-gap
      (propertize
        slot-leader
        'face 'cardan-slot-leader-face)
      column-gap
      (propertize
        (format "%8sB" bytes)
        'face 'cardan-bytes-face))))

(defun cardan-load-more-slots (button page-num)
  "Load slots in the given PAGE-NUM when load previous BUTTON is clicked."
  (cardan-list-block-pages
    page-num
    (lambda (response)
      (let* ((slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*"))
        (with-current-buffer buffer-name
          (read-only-mode 0)
          (goto-char (button-start button))
          (dolist (slot-data slots)
            (insert (cardan-slot-row slot-data))
            (insert "\n"))
          (button-put button
            'action (lambda (_button)
                      (funcall 'cardan-load-more-slots _button (- page-num 1))))
          (read-only-mode 1))))))

(defun cardan-slot-row (slot-data)
  "Generate slot row from SLOT-DATA."
  (propertize (cardan-format-slot slot-data)
    'keymap cardan-slot-row-map
    'epoch-num (cardan-get-epoch-num slot-data)
    'slot-num (cardan-get-slot-num slot-data)
    'block-hash (cardan-get-block-hash slot-data)))

(defun cardan-view-slots ()
  "View slots."
  (cardan-list-block-pages
    nil
    (lambda (response)
      (let* ((current-page-num (car (gethash "Right" response)))
             (slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*")
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (erase-buffer)
          (cardan-slots-mode)
          (dolist (slot-data slots)
            (insert (cardan-slot-row slot-data))
            (insert "\n"))
          (insert-text-button
            (substitute-command-keys
              (format "Type \\<%s>\\[%s] to previous slots"
                'cardan-slots-mode-map
                'cardan-do-load-more-slots))
            'action (lambda (_button)
                      (funcall 'cardan-load-more-slots _button (- current-page-num 1)))
            'follow-link t)
          (read-only-mode 1))))))

;;; _
(provide 'cardan-slots)
;; End:
;;; cardan-slots.el ends here
