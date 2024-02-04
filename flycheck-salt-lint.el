;;; flycheck-salt-lint.el --- Support for salt-lint in flycheck -*- lexical-binding: t -*-

;;; Commentary;


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

(flycheck-add-mode 'yaml-yamllint 'salt-mode)
;flycheck-modes (yaml-mode yaml-ts-mode)

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
                   :id (plist-get e :id))) errors)
        )
    (json-error nil)))

(provide 'flycheck-salt-lint)
;;; flycheck-salt-lint.el ends here
