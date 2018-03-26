;;; jsfmt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "jsfmt" "jsfmt.el" (23071 3838 0 0))
;;; Generated autoloads from jsfmt.el

(autoload 'jsfmt "jsfmt" "\
Formats the current buffer according to the jsfmt tool.

\(fn)" t nil)

(autoload 'jsfmt-save-ast "jsfmt" "\
Formats the current buffer according to the jsfmt ast tool.

\(fn)" t nil)

(autoload 'jsfmt-load-ast "jsfmt" "\
Formats the current buffer according to the jsfmt ast tool.

\(fn)" t nil)

(autoload 'jsfmt-before-save "jsfmt" "\
Add this to .emacs to run jsfmt on the current buffer before saving:
 (add-hook 'before-save-hook 'jsfmt-before-save).

\(fn)" t nil)

(autoload 'jsfmt-ast-before-save "jsfmt" "\
Add this to .emacs to run 'jsfmt --save-ast' on the buffer before saving
 (add-hook 'before-save-hook 'jsfmt-ast-before-save).

\(fn)" t nil)

(autoload 'jsfmt-ast-find-file "jsfmt" "\
Add this to .emacs to run 'jsfmt --ast' on the file being loaded
 (add-hook 'find-file-hook 'jsfmt-ast-find-file).

\(fn)" t nil)

(autoload 'jsfmt-ast "jsfmt" "\
Add this to .emacs to run enabling jsfmt loading .ast files as javascript and saving
   the javascript back as ast
  (add-to-list 'auto-mode-alist '(\"\\.ast$\" . (lambda()
                                                  (jsfmt-ast)
                                                  (js-mode))))

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jsfmt-autoloads.el ends here
