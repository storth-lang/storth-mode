;;; storth-mode.el --- Major mode for the Storth language -*- lexical-binding: t -*-

(defgroup storth nil
  "Major mode for editing Storth source files."
  :group 'languages)

(defface storth-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Storth keywords.")

(defface storth-directive-face
  '((t :inherit storth-type-face))
  "Face for Storth directives (matches storth-type-face by default).")

(defface storth-type-face
  '((t :inherit font-lock-type-face))
  "Face for built-in type names.")

(defface storth-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names.")

(defconst storth-keywords
  '("fn" "return" "if" "else" "then" "while" "do" "for"
    "defer" "break" "continue"
    "struct" "enum" "pub" "extern" "using"
    "null" "true" "false"
    "sizeof" "type_of" "kind" "cstr" "case" "goto" "label" "default")
  "Storth keywords.")

(defconst storth-directives
  '("#import" "#load" "#template" "#static" "#as" "#fields"
    "#comptime" "#comptime_load" "#link" "#target"
    "#if" "#else" "#for" "#case" "#default" "#comp_error"
    "#end" "#asm")
  "Storth directives.")

(defconst storth-types
  '("i8" "i16" "i32" "i64"
    "u8" "u16" "u32" "u64"
    "f32" "f64"
    "bool" "char" "string" "void" "any"
    "va_list")
  "Storth built-in type names.")

(defvar storth-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23"   st)
    (modify-syntax-entry ?\n "> b"   st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "'" st)
    st)
  "Syntax table for `storth-mode'.")

(defconst storth-font-lock-keywords
  (let ((kw-re   (regexp-opt storth-keywords 'words))
        (type-re (regexp-opt storth-types 'words)))
    `(
      ("#[A-Za-z_][A-Za-z0-9_]*" . 'storth-type-face)
      ("\\bfn\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (1 'storth-function-name-face))
      (,type-re . 'storth-type-face)
      ("\\*+\\(?:i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f32\\|f64\\|f128\\|bool\\|char\\|string\\|void\\|any\\|va_list\\)\\b"
       (0 'storth-type-face))
      (,kw-re . 'storth-keyword-face)
    ))
  "Font-lock keywords for `storth-mode'.
Highlights keywords, directives, function names, and built-in
types; strings and comments are fontified automatically via the
syntax table.")

(defcustom storth-indent-offset 4
  "Number of spaces per indentation level in Storth."
  :type 'integer
  :group 'storth)

(defun storth--calculate-indent ()
  "Return the column the current line should be indented to.
Implements K&R style: opening braces on their own line increase the
next line's indent; closing braces align with the matching open level."
  (save-excursion
    (beginning-of-line)
    (let ((cur-line-start (point))
          (depth 0))
      (goto-char (point-min))
      (while (< (point) cur-line-start)
        (let ((ch (char-after)))
          (cond
           ((eq ch ?{)
            (setq depth (1+ depth)))
           ((eq ch ?})
            (setq depth (max 0 (1- depth))))
           ((eq ch ?\")
            (condition-case nil
                (progn (forward-sexp 1) (backward-char 1))
              (error nil)))
           ((and (eq ch ?/)
                 (eq (char-after (1+ (point))) ?/))
            (end-of-line))))
        (forward-char 1))
      (goto-char cur-line-start)
      (back-to-indentation)
      (let ((first-ch (char-after)))
        (when (eq first-ch ?})
          (setq depth (max 0 (1- depth))))
        (when (eq first-ch ?{)
          (setq depth (max 0 (1- depth)))))
      (* storth-indent-offset (max 0 depth)))))

(defun storth-indent-line ()
  "Indent current line for Storth using K&R style with 4-space offsets."
  (interactive)
  (let ((indent (storth--calculate-indent)))
    (when indent
      (save-excursion
        (back-to-indentation)
        (unless (= (current-column) indent)
          (delete-horizontal-space)
          (indent-to indent)))
      (when (< (current-column) indent)
        (back-to-indentation)))))

(defconst storth-imenu-generic-expression
  '(("Functions"
     "^\\(?:#template\\s-+\\)?\\(?:pub\\s-+\\)?fn\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Structs"
     "^\\(?:pub\\s-+\\)?struct\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Enums"
     "^\\(?:pub\\s-+\\)?enum\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1))
  "Imenu expressions for `storth-mode'.")

(defconst storth-compilation-error-regexp
  '(storth
    "^\\([^ \t\n][^\t\n]*\\.st\\):\\([0-9]+\\):\\([0-9]+\\): error:"
    1 2 3 2))

;;;###autoload
(define-derived-mode storth-mode prog-mode "Storth"
  "Major mode for editing Storth source files.

Keybindings:
  \\{storth-mode-map}"
  :syntax-table storth-mode-syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local comment-start-skip "//+\\s-*")

  (setq-local font-lock-defaults
              '(storth-font-lock-keywords
                nil
                nil
                nil
                nil))

  (setq-local indent-line-function #'storth-indent-line)
  (setq-local tab-width storth-indent-offset)
  (setq-local indent-tabs-mode nil)

  (setq-local imenu-generic-expression storth-imenu-generic-expression)
  (imenu-add-to-menubar "Storth")

  (setq-local compilation-error-regexp-alist '(storth))
  (add-to-list 'compilation-error-regexp-alist-alist
               storth-compilation-error-regexp))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.st\\'" . storth-mode))

(provide 'storth-mode)
;;; storth-mode.el ends here
