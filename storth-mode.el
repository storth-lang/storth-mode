;;; storth-mode.el -*- lexical-binding: t -*-

(defgroup storth nil
  "Major mode for editing Storth source files."
  :group 'languages)

(defface storth-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Storth keywords.")

(defface storth-type-face
  '((t :inherit font-lock-type-face))
  "Face for built-in type names.")

(defface storth-directive-face
  '((t :inherit storth-type-face))
  "Face for Storth directives.")

(defface storth-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names.")

(defface storth-attribute-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for '@attribute' names.")

(defface storth-builtin-attribute-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for attributes handled by the compiler's default plugins.")

(defface storth-format-face
  '((t :inherit font-lock-escape-face))
  "Face for '%' placeholders inside string literals.")

(defcustom storth-indent-offset 4
  "Number of spaces per indentation level in Storth."
  :type 'integer
  :group 'storth)

(defcustom storth-builtin-attributes '("format")
  "Attributes provided by the compiler's default plugins."
  :type '(repeat string)
  :group 'storth)

(defconst storth-keywords
  '("fn" "return" "if" "else" "then" "while" "for"
    "defer" "break" "continue" "static" "tag_union"
    "struct" "enum" "pub" "extern" "using" "trait" "const"
    "null" "true" "false" "enum_flag" "type_info" "noreturn" "cast"
    "sizeof" "align_of" "type_of" "kind" "cstr" "fields" "str_from_raw"
    "case" "goto" "label" "default" "where" "self" "as" "not" "and" "or")
  "Storth keywords.")

(defconst storth-directives
  '("#import" "#load" "#plugin" "#template" "#as" "#fields"
    "#comptime" "#comptime_load" "#link" "#target"
    "#if" "#else" "#for" "#case" "#default" "#comp_error"
    "#end" "#asm" "#code" "#pad" "#pack")
  "Storth directives.")

(defconst storth-types
  '("i8" "i16" "i32" "i64"
    "u8" "u16" "u32" "u64"
    "f32" "f64" "f128"
    "bool" "char" "string" "void" "any"
    "va_list")
  "Storth built-in type names.")

(defvar storth-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "'" st)
    (modify-syntax-entry ?@ "'" st)
    (modify-syntax-entry ?$ "'" st)
    st)
  "Syntax table for `storth-mode'.")

(defun storth--match-format-percent (limit)
  "Find the next '%' or '%%' before LIMIT that sits inside a string literal."
  (let (found)
    (while (and (not found) (re-search-forward "%%?" limit t))
      (when (nth 3 (save-excursion (syntax-ppss (match-beginning 0))))
        (setq found t)))
    found))

(defconst storth-font-lock-keywords
  (let ((kw-re (regexp-opt storth-keywords 'words))
        (type-re (regexp-opt storth-types 'words)))
    `((storth--match-format-percent (0 'storth-format-face t))
      (,(concat "@\\(" (regexp-opt storth-builtin-attributes) "\\)\\_>")
       (0 'storth-builtin-attribute-face))
      ("@[A-Za-z_][A-Za-z0-9_]*" . 'storth-attribute-face)
      ("#[A-Za-z_][A-Za-z0-9_]*" . 'storth-directive-face)
      ("\\_<fn\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (1 'storth-function-name-face))
      ("\\_<\\(?:struct\\|enum\\|enum_flag\\|tag_union\\|trait\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (1 'font-lock-type-face))
      (,type-re . 'storth-type-face)
      ("\\*+\\(?:i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f32\\|f64\\|f128\\|bool\\|char\\|string\\|void\\|any\\|va_list\\)\\_>"
       (0 'storth-type-face))
      ("\\$[A-Za-z_][A-Za-z0-9_]*" . 'storth-type-face)
      (,kw-re . 'storth-keyword-face)))
  "Font-lock keywords for `storth-mode'.")

(defconst storth--continuation-re
  "\\(?:[-+*/%=<>|&^]\\|\\_<then\\)"
  "A line whose code ends in this continues onto the next line.")

(defun storth--code-end ()
  "Return the position just after the last code character on the current line."
  (save-excursion
    (end-of-line)
    (let ((ppss (syntax-ppss)))
      (when (nth 4 ppss)
        (goto-char (nth 8 ppss))))
    (skip-chars-backward " \t")
    (max (point) (line-beginning-position))))

(defun storth--line-continues-p ()
  "Non-nil if the current line's statement carries on to the next line."
  (save-excursion
    (back-to-indentation)
    (unless (looking-at "@\\|#\\|$\\|//")
      (let ((beg (line-beginning-position))
            (end (storth--code-end)))
        (and (> end beg)
             (not (nth 4 (save-excursion (syntax-ppss (1- end)))))
             (not (nth 3 (save-excursion (syntax-ppss end))))
             (save-excursion
               (goto-char end)
               (looking-back storth--continuation-re beg)))))))

(defun storth--prev-code-line ()
  "Move to the previous line holding code. Return non-nil if one was found."
  (let (found)
    (while (and (not found) (= 0 (forward-line -1)))
      (back-to-indentation)
      (unless (or (looking-at "$")
                  (looking-at "//")
                  (nth 4 (syntax-ppss)))
        (setq found t)))
    found))

(defun storth--stmt-indent ()
  "Indentation of the statement the current line belongs to."
  (save-excursion
    (beginning-of-line)
    (let ((done nil))
      (while (not done)
        (let ((here (point)))
          (if (and (storth--prev-code-line) (storth--line-continues-p))
              (beginning-of-line)
            (goto-char here)
            (setq done t)))))
    (current-indentation)))

(defun storth--calculate-indent ()
  "Return the column the current line should be indented to, or nil to leave it."
  (save-excursion
    (back-to-indentation)
    (let* ((ppss (syntax-ppss))
           (open (nth 1 ppss)))
      (unless (or (nth 3 ppss) (nth 4 ppss))
        (let* ((closer (looking-at "[]})]"))
               (in-brace (or (null open) (eq (char-after open) ?{)))
               (base
                (cond
                 ((null open) 0)
                 ((eq (char-after open) ?{)
                  (save-excursion
                    (goto-char open)
                    (+ (storth--stmt-indent) (if closer 0 storth-indent-offset))))
                 (t
                  (save-excursion
                    (goto-char open)
                    (cond
                     (closer (storth--stmt-indent))
                     ((progn (forward-char 1)
                             (skip-chars-forward " \t")
                             (not (or (eolp) (looking-at "//"))))
                      (current-column))
                     (t (+ (storth--stmt-indent) storth-indent-offset))))))))
          (if (and in-brace
                   (not closer)
                   (not (looking-at "{\\|else\\_>\\|case\\_>\\|default\\_>\\|#else\\_>\\|#case\\_>"))
                   (save-excursion
                     (and (storth--prev-code-line)
                          (or (null open) (> (point) open))
                          (storth--line-continues-p))))
              (+ base storth-indent-offset)
            base))))))

(defun storth-indent-line ()
  "Indent the current line as Storth code."
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
     "^\\s-*\\(?:#template\\s-+\\)?\\(?:pub\\s-+\\)?fn\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Structs"
     "^\\(?:pub\\s-+\\)?struct\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Enums"
     "^\\(?:pub\\s-+\\)?\\(?:enum\\|enum_flag\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Tag unions"
     "^\\(?:pub\\s-+\\)?tag_union\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Traits"
     "^\\(?:pub\\s-+\\)?trait\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1))
  "Imenu expressions for `storth-mode'.")

(defconst storth-compilation-error-regexp
  '(storth
    "^\\([^ \t\n][^\t\n]*\\.st\\):\\([0-9]+\\):\\([0-9]+\\): error:"
    1 2 3 2))

(defconst storth-compilation-note-regexp
  '(storth-note
    "^\\([^ \t\n][^\t\n]*\\.st\\):\\([0-9]+\\):\\([0-9]+\\): note:"
    1 2 3 0))

(define-derived-mode storth-mode prog-mode "Storth"
  "Major mode for editing Storth source files.

\\{storth-mode-map}"
  :syntax-table storth-mode-syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  (setq-local font-lock-defaults '(storth-font-lock-keywords nil nil nil nil))

  (setq-local indent-line-function #'storth-indent-line)
  (setq-local electric-indent-chars (append '(?} ?\) ?\]) electric-indent-chars))
  (setq-local tab-width storth-indent-offset)
  (setq-local indent-tabs-mode nil)

  (setq-local imenu-generic-expression storth-imenu-generic-expression)
  (imenu-add-to-menubar "Storth")

  (add-to-list 'compilation-error-regexp-alist-alist storth-compilation-error-regexp)
  (add-to-list 'compilation-error-regexp-alist-alist storth-compilation-note-regexp)
  (add-to-list 'compilation-error-regexp-alist 'storth)
  (add-to-list 'compilation-error-regexp-alist 'storth-note))

(add-to-list 'auto-mode-alist '("\\.st\\'" . storth-mode))

(provide 'storth-mode)
