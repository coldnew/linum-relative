;;; linum-relative.el --- display relative line number in emacs.

;; Copyright (c) 2013 - 2016 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: converience
;; X-URL: http://github.com/coldnew/linum-relative
;; Version: 0.6

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; [![MELPA](http://melpa.org/packages/linum-relative-badge.svg)](http://melpa.org/#/linum-relative)
;; [![MELPA Stable](http://stable.melpa.org/packages/linum-relative-badge.svg)](http://stable.melpa.org/#/linum-relative)

;; ![Screenshot](https://github.com/coldnew/linum-relative/raw/master/screenshot/screenshot1.jpg)
;;
;; linum-relative lets you display relative line numbers for current buffer.
;;

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;     M-x package-install linum-relative
;;
;; And add the following to your .emacs
;;
;;     (require 'linum-relative)

;;; Setup & Tips:

;; The non-interactive function *linum-on* (which should already be built into recent GNU Emacs distributions), turns on side-bar line numbering:
;;
;;     (linum-on)
;;
;; and alternatively, by using command:
;;
;;     M-x linum-relative-mode
;;
;; Relative line numbering should already be enabled by default (by installing this package), following *linum-on* or enabling *linum-mode*. One can also use the *linum-relative-toggle* interactive function to switch between relative and non-relative line numbering:
;;
;;     M-x linum-relative-toggle
;;

;;; Backends

;; By default, linum-relative use *linum-mode* as backend, since linum-mode is based on emacs-lisp, you may have performance issue on large file.
;;
;; Since linum-relative 0.6, if you also use emacs version 26.1 or above, you can setup `linum-relative-backend' to make linum-relative-mode use `display-line-number-mode' as backend, which is implement in C so the performance is really nice.
;;
;; However some linum-relative's customize function may not work propely.
;;
;; Here's how to use `display-line-number-mode' as backend:
;;
;; ```elisp
;;      ;; Use `display-line-number-mode' as linum-mode's backend for smooth performance
;;	(setq linum-relative-backend 'display-line-numbers-mode)
;; ```


;;; Code:

(eval-when-compile (require 'cl))

;; display-line-numbers is shipped with emacs 26.0.50
(when (version<= "26.0.50" emacs-version)
  (require 'display-line-numbers))
(require 'linum)


(defgroup linum-relative nil
  "Show relative line numbers on fringe."
  :group 'convenience)

;;;; Faces
;; NOTE: can't work on `display-line-numbers-mode'
(defface linum-relative-current-face
  '((t :inherit linum :foreground "#CAE682" :background "#444444" :weight bold))
  "Face for displaying current line.

This won't take effect if you choose `display-line-numbers-mode' backend."
  :group 'linum-relative)

;;;; Customize Variables

;; NOTE: can't work on `display-line-numbers-mode'
(defcustom linum-relative-current-symbol "0"
  "The symbol you want to show on the current line, by default it is 0.
   You can use any string like \"->\". If this variable is empty string,
linum-releative will show the real line number at current line.

This won't take effect if you choose `display-line-numbers-mode' backend."
  :type 'string
  :group 'linum-relative)

;; NOTE: can't work on `display-line-numbers-mode'
(defcustom linum-relative-plusp-offset 0
  "Offset to use for positive relative line numbers.

This won't take effect if you choose `display-line-numbers-mode' backend."
  :type 'integer
  :group 'linum-relative)

;; NOTE: can't work on `display-line-numbers-mode'
(defcustom linum-relative-format "%3s"
  "Format for each line. Good for adding spaces/paddings like so: \" %3s \"

This won't take effect if you choose `display-line-numbers-mode' backend."
  :type 'string
  :group 'linum-relative)

(defcustom linum-relative-lighter " LR"
  "Lighter of linum-relative-mode"
  :type 'string
  :group 'linum-relative)

(defcustom linum-relative-backend 'linum-mode
  "The default backend for `linum-relative', by default we use
`linum-mode' (slow), you can switch to `display-line-numbers-mode' if
you has emacs-version greater than 26.0.50."
  :group 'linum-relative
  :type '(choice (const :tag "Use display-line-numbers-mode as backend" display-line-numbers-mode)
                 (other :tag "Use linum-mode as backend" linum-mode)))

;;;; Internal Variables

;; NOTE: can't work on `display-line-numbers-mode'
(defvar linum-relative-last-pos 0
  "Store last position.")

;; NOTE: can't work on `display-line-numbers-mode'
(defvar linum-relative-user-format linum-format
  "Store the users linum-format")

(defvar linum-relative-user-type (bound-and-true-p display-line-numbers-type)
  "Store the user's `display-line-number-type' value")

;;;; helm support
(defvar helm-buffer)
(defvar helm-candidate-separator)
(defvar helm-alive-p)
(declare-function with-helm-buffer "ext:helm-lib.el" (&rest body))
(declare-function helm-candidate-number-at-point "ext:helm.el")
(declare-function helm-pos-header-line-p "ext:helm.el")

(defmacro linum-relative-with-helm-buffer (&rest body)
  (when (fboundp 'with-helm-buffer)
    `(with-helm-buffer ,@body)))

(defun linum-relative-in-helm-p ()
  "Return non nil when in an helm session."
  (bound-and-true-p helm-alive-p))

(defun linum-relative-for-helm ()
  (linum-relative-with-helm-buffer
   (make-local-variable 'linum-relative-last-pos))
  (linum-update helm-buffer))

;;;; Advices
(defadvice linum-update (before relative-linum-update activate)
  "This advice get the last position of linum."
  (if (linum-relative-in-helm-p)
      (setq linum-relative-last-pos (helm-candidate-number-at-point))
      (setq linum-relative-last-pos (line-number-at-pos))))

;;;; Functions
(defun linum-relative (line-number)
  (when (linum-relative-in-helm-p)
    (linum-relative-with-helm-buffer
     (if (looking-at helm-candidate-separator)
         (setq line-number (save-excursion
                             (forward-line 1) (helm-candidate-number-at-point)))
         (setq line-number (helm-candidate-number-at-point)))))
  (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
         (diff (if (minusp diff1)
                   diff1
                   (+ diff1 linum-relative-plusp-offset)))
         (current-p (= diff linum-relative-plusp-offset))
         (current-symbol (if (and linum-relative-current-symbol current-p)
                             (if (string= "" linum-relative-current-symbol)
                                 (number-to-string line-number)
                                 linum-relative-current-symbol)
                             (number-to-string diff)))
         (face (if current-p 'linum-relative-current-face 'linum)))
    (if (and (linum-relative-in-helm-p)
             (linum-relative-with-helm-buffer
              (or (looking-at helm-candidate-separator)
                  (eq (point-at-bol) (point-at-eol))
                  (helm-pos-header-line-p))))
        (propertize (format linum-relative-format current-symbol) 'invisible t)
        (propertize (format linum-relative-format current-symbol) 'face face))))


(defun linum-relative-on ()
  "Turn ON linum-relative."
  (cond
   ;; if use `display-line-numbers-mode'
   ((eq linum-relative-backend 'display-line-numbers-mode)
    (unless (eq linum-relative-user-type 'relative)
      (setq linum-relative-user-type display-line-numbers-type)
      (setq display-line-numbers-type 'relative))
    (display-line-numbers-mode 1))
   ;; default for linum-mode backend
   (t (unless (eq linum-format 'linum-relative)
        (setq linum-relative-user-format linum-format)
        (setq linum-format 'linum-relative))
      (linum-mode 1))))

(defun linum-relative-off ()
  "Turn OFF linum-relative."
  (cond
   ;; if use `display-line-numbers-mode'
   ((eq linum-relative-backend 'display-line-numbers-mode)
    (setq display-line-numbers-type linum-relative-user-type)
    (display-line-numbers-mode -1))
   ;; default for linum-mode backend
   (t (setq linum-format linum-relative-user-format)
      (linum-mode -1))))

;;;###autoload
(defun linum-relative-toggle ()
  "Toggle between linum-relative and linum."
  (interactive)
  (cond
   ;; if use `display-line-numbers-mode'
   ((eq linum-relative-backend 'display-line-numbers-mode)
    (if (eq display-line-numbers-type 'relative)
        (linum-relative-off)
        (linum-relative-on)))
   ;; default for linum-mode backend
   (t (if (eq linum-format 'linum-relative)
          (linum-relative-off)
          (linum-relative-on)))))

;;;###autoload
(define-minor-mode linum-relative-mode
  "Display relative line numbers for current buffer."
  :group 'linum-relative
  :lighter linum-relative-lighter
  (if linum-relative-mode
      (linum-relative-on)
      (linum-relative-off)))

;;;###autoload
(define-global-minor-mode linum-relative-global-mode
  linum-relative-mode (lambda () (unless (linum-relative-in-helm-p)
                              (linum-relative-mode 1))))

;;;; Interaction of helm with linum-relative

(defun helm--turn-on-linum-relative ()
  (with-helm-buffer (linum-relative-mode 1)))

;;;###autoload
(define-minor-mode helm-linum-relative-mode
  "Turn on `linum-relative-mode' in helm."
  :group 'helm
  (if helm-linum-relative-mode
      (progn
        (add-hook 'helm-move-selection-after-hook 'linum-relative-for-helm)
        (add-hook 'helm-after-initialize-hook 'helm--turn-on-linum-relative)
        (add-hook 'helm-after-preselection-hook 'linum-relative-for-helm))
      (remove-hook 'helm-move-selection-after-hook 'linum-relative-for-helm)
      (remove-hook 'helm-after-initialize-hook 'helm--turn-on-linum-relative)
      (remove-hook 'helm-after-preselection-hook 'linum-relative-for-helm)))


(provide 'linum-relative)
;;; linum-relative.el ends here.
