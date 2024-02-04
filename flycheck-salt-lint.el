;;; flycheck-salt-lint.el --- Support for salt-lint in flycheck -*- lexical-binding: t -*-

;; Adds support for the checker salt-lint to flycheck.
;; Copyright (C) 2024 Henrik Jürges <ratzeputz@rtzptz.xyz>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Henrik Jürges <ratzeputz@rtzptz.xyz>
;; Version: 2024.02.04
;; Package-Requires: ((emacs "27") (flycheck "29"))
;; URL: https://github.com/santifa/flycheck-salt-lint

;;; Commentary;

;; This package adds support for linting salt files (.sls) to flycheck.
;; To use it download this file and add the following section to your init.el file:

;; (require flycheck)
;; (require flycheck-salt-lint)
;; (eval-after-load 'flycheck
;;  '(flycheck-salt-lint-setup))
;; (add-hook 'salt-mode-hook 'flycheck-mode)

;;; Code:

(require 'flycheck)

(flycheck-define-checker salt-lint
  "A salt linter for common best practices.

See URL `https://salt-lint.readthedocs.io/en/latest/'."
  :command ("python" "-m" "saltlint" "--json")
  :standard-input t
  :error-parser salt-lint-parser
  :error-filter (lambda (errors) (flycheck-sanitize-errors errors))
  :modes salt-mode)

(defun salt-lint-parser (output checker buffer)
  "Parse salt lint JSON errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked repectively."
  (condition-case nil
      (let* ((json-array-type 'list)
             (json-object-type 'plist)
            (filename (buffer-file-name buffer))
            (errors (json-read-from-string output)))
        ;(message "%s errors in  %s" errors filename)
        (mapcar (lambda (e)
                  (flycheck-error-new
                   :checker checker
                   :buffer buffer
                   :filename filename
                   :level (pcase (plist-get e :severity)
                            ("HIGH" 'error)
                            ("MEDIUM" 'warning)
                            ("LOW" 'warning)
                            ("INFO" 'info)
                            (_ 'info))
                   :line (plist-get e :linenumber)
                   :column 0
                   :message (concat (plist-get e :message) (plist-get e :line))
                   :id (plist-get e :id))) errors))
    (json-error nil)))

;;;###autoload
(defun flycheck-salt-lint-setup ()
  "Setup Flycheck with salt-lint."
  (interactive)
  (add-to-list 'flycheck-checkers 'salt-lint))

(provide 'flycheck-salt-lint)
;;; flycheck-salt-lint.el ends here
