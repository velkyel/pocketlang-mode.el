;;; pocketlang-mode.el --- Major mode for editing Pocketlang files -*- lexical-binding: t -*-

(require 'cl-lib)

(defgroup pocketlang nil
  "Major mode for editing Pocketlang code."
  :prefix "pocketlang-"
  :group 'languages)

(defconst pocketlang-block-beg-keywords
  '("class" "def" "if" "while" "for" "do")
  "Keywords at the beginning of blocks.")

(defconst pocketlang-block-beg-re
  (regexp-opt pocketlang-block-beg-keywords)
  "Regexp to match the beginning of blocks.")

(defconst pocketlang-non-block-do-re
  (regexp-opt '("while" "for") 'symbols)
  "Regexp to match keywords that nest without blocks.")

(defconst pocketlang-indent-beg-re
  (concat "^\\(\\s *" (regexp-opt '("class" "def")) "\\|"
          (regexp-opt '("if" "while" "for"))
          "\\)\\_>")
  "Regexp to match where the indentation gets deeper.")

(defconst pocketlang-modifier-beg-keywords
  '("if" "while")
  "Modifiers that are the same as the beginning of blocks.")

(defconst pocketlang-modifier-beg-re
  (regexp-opt pocketlang-modifier-beg-keywords)
  "Regexp to match modifiers same as the beginning of blocks.")

(defconst pocketlang-modifier-re
  (regexp-opt pocketlang-modifier-beg-keywords)
  "Regexp to match modifiers.")

(defconst pocketlang-block-mid-keywords
  '("then" "else")
  "Keywords where the indentation gets shallower in middle of block statements.")

(defconst pocketlang-block-mid-re
  (regexp-opt pocketlang-block-mid-keywords)
  "Regexp for where the indentation gets shallower in middle of block statements.")

(defconst pocketlang-block-op-keywords
  '("and" "or" "not")
  "Regexp to match boolean keywords.")

(defconst pocketlang-block-hanging-re
  (regexp-opt (append pocketlang-modifier-beg-keywords pocketlang-block-op-keywords))
  "Regexp to match hanging block modifiers.")

(defconst pocketlang-block-end-re "\\_<end\\_>")

(defconst pocketlang-defun-beg-re
  '"\\(def\\|class\\)"
  "Regexp to match the beginning of a defun, in the general sense.")

(defconst pocketlang-singleton-class-re
  "class\\s *<<"
  "Regexp to match the beginning of a singleton class context.")

(eval-and-compile
  (defconst pocketlang-expression-expansion-re
    "#\\({[^}\n\\]*\\(\\\\.[^}\n\\]*\\)*}\\|\\(?:\\$\\|@\\|@@\\)\\(\\w\\|_\\)+\\|\\$[^a-zA-Z \n]\\)"))

(defconst pocketlang-delimiter
  (concat "[?$/%(){}#\"'`.:]\\|<<\\|\\[\\|\\]\\|\\_<\\("
          pocketlang-block-beg-re
          "\\)\\_>\\|" pocketlang-block-end-re))

(defconst pocketlang-negative
  (concat "^[ \t]*\\(\\(" pocketlang-block-mid-re "\\)\\>\\|"
          pocketlang-block-end-re "\\|}\\|\\]\\)")
  "Regexp to match where the indentation gets shallower.")

(defconst pocketlang-operator-re "[-,.+*/%&|^~=<>:]\\|\\\\$"
  "Regexp to match operators.")

(defconst pocketlang-symbol-chars "a-zA-Z0-9_"
  "List of characters that symbol names may contain.")

(defconst pocketlang-symbol-re (concat "[" pocketlang-symbol-chars "]")
  "Regexp to match symbols.")

(defvar pocketlang-use-smie t)
(make-obsolete-variable 'pocketlang-use-smie nil "28.1")

(defvar pocketlang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-C-d") 'smie-down-list)
    (define-key map (kbd "M-C-p") 'pocketlang-beginning-of-block)
    (define-key map (kbd "M-C-n") 'pocketlang-end-of-block)
    map)
  "Keymap used in Pocketlang mode.")

(defvar pocketlang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "'" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table to use in Pocketlang mode.")

(defcustom pocketlang-indent-level 2
  "Indentation of Pocketlang statements."
  :type 'integer
  :safe 'integerp)

(defcustom pocketlang-comment-column (default-value 'comment-column)
  "Indentation column of comments."
  :type 'integer
  :safe 'integerp)

(defconst pocketlang-alignable-keywords '(if while for def)
  "Keywords that can be used in `pocketlang-align-to-stmt-keywords'.")

(defcustom pocketlang-align-to-stmt-keywords '(def)
  "Keywords after which we align the expression body to statement.

When nil, an expression that begins with one these keywords is
indented to the column of the keyword.  Example:

  tee = if foo
          bar
        else
          qux
        end

If this value is t or contains a symbol with the name of given
keyword, the expression is indented to align to the beginning of
the statement:

  tee = if foo
    bar
  else
    qux
  end
"
  :type `(choice
          (const :tag "None" nil)
          (const :tag "All" t)
          (repeat :tag "User defined"
                  (choice ,@(mapcar
                             (lambda (kw) (list 'const kw))
                             pocketlang-alignable-keywords))))
  :safe 'listp
  :version "24.4")

(defcustom pocketlang-align-chained-calls nil
  "If non-nil, align chained method calls.

Each method call on a separate line will be aligned to the column
of its parent."
  :type 'boolean
  :safe 'booleanp
  :version "24.4")

(defcustom pocketlang-deep-arglist t
  "Deep indent lists in parenthesis when non-nil.
Also ignores spaces after parenthesis when `space'.
Only has effect when `pocketlang-use-smie' is nil."
  :type 'boolean
  :safe 'booleanp)

;; FIXME Woefully under documented.  What is the point of the last t?.
(defcustom pocketlang-deep-indent-paren '(?\( ?\[ ?\] t)
  "Deep indent lists in parenthesis when non-nil.
The value t means continuous line.
Also ignores spaces after parenthesis when `space'.
Only has effect when `pocketlang-use-smie' is nil."
  :type '(choice (const nil)
                 character
                 (repeat (choice character
                                 (cons character (choice (const nil)
                                                         (const t)))
                                 (const t) ; why?
                                 ))))

(defcustom pocketlang-deep-indent-paren-style 'space
  "Default deep indent style.
Only has effect when `pocketlang-use-smie' is nil."
  :type '(choice (const t) (const nil) (const space)))

;;; SMIE support

(require 'smie)

;; Here's a simplified BNF grammar, for reference:
;; https://www.cse.buffalo.edu/~regan/cse305/PocketlangBNF.pdf
(defconst pocketlang-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (insts (inst) (insts ";" insts))
       (inst (exp) (inst "iuwu-mod" exp)
             ;; Somewhat incorrect (both can be used multiple times),
             ;; but avoids lots of conflicts:
             (exp "and" exp) (exp "or" exp))
       (exp  (exp1) (exp "," exp) (exp "=" exp))
       ;; (exp1 (exp2) (exp2 "?" exp1 ":" exp1))
       (exp2 (exp3) (exp3 "." exp3))
       (exp3 ("def" insts "end")
             ("fn" insts "end")
             ("do" insts "end")
             ("class" insts "end")
             ("for" for-body "end")
             ("[" expseq "]")
             ("{" insts "}")
             ("while" insts "end")
             ("if" if-body "end")
             )
       (for-body (for-head ";" insts))
       (for-head (id "in" exp))
       (expseq (exp) );;(expseq "," expseq)
       (itheni (insts) (exp "then" insts))
       (ielsei (itheni) (itheni "else" insts))
       (if-body (ielsei) (if-body "elsif" if-body)))
     '((nonassoc "in") (assoc ";") (right " @ ")
       (assoc ",") (right "="))
     '((assoc "elsif"))
     '((assoc ",")))

    (smie-precs->prec2
     '((right "=")
       (right "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^="
              "<<=" ">>=" "&&=" "||=")
       (nonassoc "..")
       (left "&&" "||")
       (nonassoc "<=>")
       (nonassoc "==" "===" "!=")
       (nonassoc "=~" "!~")
       (nonassoc ">" ">=" "<" "<=")
       (left "^" "&" "|")
       (left "<<" ">>")
       (left "+" "-")
       (left "*" "/" "%")
       (left "**")
       (assoc "."))))))

(defun pocketlang-smie--bosp ()
  (save-excursion (skip-chars-backward " \t")
                  (or (and (bolp)
                           ;; Newline is escaped.
                           (not (eq (char-before (1- (point))) ?\\)))
                      (eq (char-before) ?\;)
                      (and (eq (char-before) ?=)
                           (equal (syntax-after (1- (point)))
                                  (string-to-syntax "."))))))

(defun pocketlang-smie--implicit-semi-p ()
  (save-excursion
    (skip-chars-backward " \t")
    (not (or (bolp)
             (memq (char-before) '(?\[ ?\())
             (and (memq (char-before)
                        '(?\; ?- ?+ ?* ?/ ?: ?. ?, ?\\ ?& ?> ?< ?% ?~ ?^ ?= ??))
                  ;; Not a binary operator symbol like :+ or :[]=.
                  ;; Or a (method or symbol) name ending with ?.
                  ;; Or the end of a regexp or a percent literal.
                  (not (memq (car (syntax-after (1- (point)))) '(3 7 15))))
             (and (eq (char-before) ?|)
                  (member (save-excursion (pocketlang-smie--backward-token))
                          '("|" "||")))
             (and (eq (car (syntax-after (1- (point)))) 2)
                  (member (save-excursion (pocketlang-smie--backward-token))
                          '("iuwu-mod" "and" "or")))
             (save-excursion
               (forward-comment (point-max))
               (looking-at "&?\\."))))))

(defun pocketlang-smie--redundant-do-p (&optional skip)
  (save-excursion
    (if skip (backward-word-strictly 1))
    (member (nth 2 (smie-backward-sexp ";")) '("while" "for"))))

(defun pocketlang-smie--opening-pipe-p ()
  (save-excursion
    (if (eq ?| (char-before)) (forward-char -1))
    (skip-chars-backward " \t\n")
    (or (eq ?\{ (char-before))
        (looking-back "\\_<do" (- (point) 2)))))

(defun pocketlang-smie--closing-pipe-p ()
  (save-excursion
    (if (eq ?| (char-before)) (forward-char -1))
    (and (re-search-backward "|" (line-beginning-position) t)
         (pocketlang-smie--opening-pipe-p))))

(defun pocketlang-smie--args-separator-p (pos)
  (and
   (< pos (line-end-position))
   (or (eq (char-syntax (preceding-char)) '?w)
       ;; FIXME: Check that the preceding token is not a keyword.
       ;; This isn't very important most of the time, though.
       (and (memq (preceding-char) '(?! ??))
            (eq (char-syntax (char-before (1- (point)))) '?w)))
   (save-excursion
     (goto-char pos)
     (or (and (eq (char-syntax (char-after)) ?w)
              (not (looking-at (regexp-opt '("if" "while" "or"
                                             "else" "do" "end" "and")
                                           'symbols))))
         (memq (car (syntax-after pos)) '(7 15))
         (looking-at "[([]\\|[-+!~:]\\(?:\\sw\\|\\s_\\)")))))

(defun pocketlang-smie--before-method-name ()
  ;; Only need to be accurate when method has keyword name.
  (and (eq ?w (char-syntax (following-char)))
       (or
        (and
         (eq (char-before) ?.)
         (not (eq (char-before (1- (point))) ?.)))
        (looking-back "^\\s *def\\s +\\=" (line-beginning-position)))))

(defun pocketlang-smie--forward-token ()
  (let ((pos (point)))
    (skip-chars-forward " \t")
    (cond
     ((and (looking-at "[\n#]")
           (pocketlang-smie--implicit-semi-p)) ;Only add implicit ; when needed.
      (if (eolp) (forward-char 1) (forward-comment 1))
      ";")
     (t
      (forward-comment (point-max))
      (cond
       ((and (< pos (point))
             (save-excursion
               (pocketlang-smie--args-separator-p (prog1 (point) (goto-char pos)))))
        " @ ")
       ((looking-at "\\s\"") "")                    ;A string.
       (t
        (let ((dot (pocketlang-smie--before-method-name))
              (tok (smie-default-forward-token)))
          (when dot
            (setq tok (concat "." tok)))
          (cond
           ((member tok '("if" "while"))
            (if (save-excursion (forward-word-strictly -1) (pocketlang-smie--bosp))
                tok "iuwu-mod"))
           ((string-match-p "\\`|[*&]?\\'" tok)
            (forward-char (- 1 (length tok)))
            (setq tok "|")
            (cond
             ((pocketlang-smie--opening-pipe-p) "opening-|")
             ((pocketlang-smie--closing-pipe-p) "closing-|")
             (t tok)))
           ((string-match "\\`[^|]+|\\'" tok)
            (forward-char -1)
            (substring tok 0 -1))
           ((and (equal tok "") (looking-at "\\\\\n"))
            (goto-char (match-end 0)) (pocketlang-smie--forward-token))
           ((equal tok "do")
            (cond
             ((not (pocketlang-smie--redundant-do-p 'skip)) tok)
             ((> (save-excursion (forward-comment (point-max)) (point))
                 (line-end-position))
              (pocketlang-smie--forward-token)) ;Fully redundant.
             (t ";")))
           ((equal tok "&.") ".")
           (t tok)))))))))

(defun pocketlang-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position)) (pocketlang-smie--implicit-semi-p))
      (skip-chars-forward " \t") ";")
     ((and (> pos (point)) (not (bolp))
           (pocketlang-smie--args-separator-p pos))
      ;; We have "ID SPC ID", which is a method call, but it binds less tightly
      ;; than commas, since a method call can also be "ID ARG1, ARG2, ARG3".
      ;; In some textbooks, "e1 @ e2" is used to mean "call e1 with arg e2".
      " @ ")
     (t
      (let ((tok (smie-default-backward-token))
            (dot (pocketlang-smie--before-method-name)))
        (when dot
          (setq tok (concat "." tok)))
        (cond
         ((member tok '("if" "while"))
          (if (pocketlang-smie--bosp)
              tok "iuwu-mod"))
         ((equal tok "|")
          (cond
           ((pocketlang-smie--opening-pipe-p) "opening-|")
           ((pocketlang-smie--closing-pipe-p) "closing-|")
           (t tok)))
         ((string-match-p "\\`[^|]+|\\'" tok) "closing-|")
         ((string-match-p "\\`|[*&]\\'" tok)
          (forward-char 1)
          (substring tok 1))
         ((and (equal tok "") (eq ?\\ (char-before)) (looking-at "\n"))
          (forward-char -1) (pocketlang-smie--backward-token))
         ((equal tok "do")
          (cond
           ((not (pocketlang-smie--redundant-do-p)) tok)
           ((> (save-excursion (forward-word-strictly 1)
                               (forward-comment (point-max)) (point))
               (line-end-position))
            (pocketlang-smie--backward-token)) ;Fully redundant.
           (t ";")))
         ((equal tok "&.") ".")
         (t tok)))))))

(defun pocketlang-smie--indent-to-stmt ()
  (save-excursion
    (smie-backward-sexp ";")
    (cons 'column (smie-indent-virtual))))

(defun pocketlang-smie--indent-to-stmt-p (keyword)
  (or (eq t pocketlang-align-to-stmt-keywords)
      (memq (intern keyword) pocketlang-align-to-stmt-keywords)))

(defun pocketlang-smie-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) pocketlang-indent-level)
    ;; "foo" "bar" is the concatenation of the two strings, so the second
    ;; should be aligned with the first.
    ('(:elem . args) (if (looking-at "\\s\"") 0))
    ;; (`(:after . ",") (smie-rule-separator kind))
    ('(:before . ";")
     (cond
      ((smie-rule-parent-p "def" "do" "class" "for"
                           "while"
                           "if" "then" "else"
                           "{")
       (smie-rule-parent pocketlang-indent-level))
      ;; For (invalid) code between switch and case.
      ;; (if (smie-parent-p "switch") 4)
      ))
    (`(:before . ,(or "(" "[" "{"))
     (cond
      ((and (equal token "{")
            (not (smie-rule-prev-p "(" "{" "[" "," "=>" "=" "return" ";" "do"))
            (save-excursion
              (forward-comment -1)
              (not (eq (preceding-char) ?:))))
       ;; Curly block opener.
       (pocketlang-smie--indent-to-stmt))
      ((smie-rule-hanging-p)
       ;; Treat purely syntactic block-constructs as being part of their parent,
       ;; when the opening token is hanging and the parent is not an
       ;; open-paren.
       (cond
        ((eq (car (smie-indent--parent)) t) nil)
        ;; When after `.', let's always de-indent,
        ;; because when `.' is inside the line, the
        ;; additional indentation from it looks out of place.
        ((smie-rule-parent-p ".")
         ;; Traverse up the call chain until the parent is not `.',
         ;; or `.' at indentation, or at eol.
         (while (and (not (pocketlang-smie--bosp))
                     (equal (nth 2 (smie-backward-sexp ".")) ".")
                     (not (pocketlang-smie--bosp)))
           (forward-char -1))
         (smie-indent-virtual))
        (t (smie-rule-parent))))))
    (`(:after . ,(or "(" "[" "{"))
     ;; FIXME: Shouldn't this be the default behavior of
     ;; `smie-indent-after-keyword'?
     (save-excursion
       (forward-char 1)
       (skip-chars-forward " \t")
       ;; `smie-rule-hanging-p' is not good enough here,
       ;; because we want to reject hanging tokens at bol, too.
       (unless (or (eolp) (forward-comment 1))
         (cons 'column (current-column)))))
    ('(:before . " @ ")
     (save-excursion
       (skip-chars-forward " \t")
       (cons 'column (current-column))))
    ('(:before . "do") (pocketlang-smie--indent-to-stmt))
    ('(:before . ".")
     (if (smie-rule-sibling-p)
         (when pocketlang-align-chained-calls
           (while
               (let ((pos (point))
                     (parent (smie-backward-sexp ".")))
                 (if (not (equal (nth 2 parent) "."))
                     (progn (goto-char pos) nil)
                   (goto-char (nth 1 parent))
                   (not (smie-rule-bolp)))))
           (cons 'column (current-column)))
       (smie-backward-sexp ".")
       (cons 'column (+ (current-column)
                        pocketlang-indent-level))))
    (`(:before . ,(or "else" "then"))
     (smie-rule-parent))
    (`(:after . ,(or "=" "+" "-" "*" "/" "&&" "||" "%" "**" "^" "&"
                     "<=>" ">" "<" ">=" "<=" "==" "===" "!=" "<<" ">>"
                     "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^=" "|"
                     "<<=" ">>=" "&&=" "||=" "and" "or"))
     (and (smie-rule-parent-p ";" nil)
          (smie-indent--hanging-p)
          pocketlang-indent-level))
    (`(:after . ,(or "?" ":")) pocketlang-indent-level)
    (`(:before . ,(guard (memq (intern-soft token) pocketlang-alignable-keywords)))
     (when (not (pocketlang--at-indentation-p))
       (if (pocketlang-smie--indent-to-stmt-p token)
           (pocketlang-smie--indent-to-stmt)
         (cons 'column (current-column)))))
    ('(:before . "iuwu-mod")
     (smie-rule-parent pocketlang-indent-level))
    ))

(defun pocketlang--at-indentation-p (&optional point)
  (save-excursion
    (unless point (setq point (point)))
    (forward-line 0)
    (skip-chars-forward " \t")
    (eq (point) point)))

(defun pocketlang-imenu-create-index-in-block (prefix beg end)
  "Create an imenu index of methods inside a block."
  (let ((index-alist '()) (case-fold-search nil)
        name next pos decl sing)
    (goto-char beg)
    (while (re-search-forward "^\\s *\\(\\(class\\s +\\|\\(class\\s *<<\\s *\\)\\|module\\s +\\)\\([^(<\n ]+\\)\\|\\(\\(?:\\(?:private\\|protected\\|public\\) +\\)?def\\)\\s +\\([^(\n ]+\\)\\)" end t)
      (setq sing (match-beginning 3))
      (setq decl (match-string 5))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6)))
      (setq pos (match-beginning 0))
      (cond
       ((not (null decl))
        (if prefix
            (setq name
                  (cond
                   ((string-match "^self\\." name)
                    (concat (substring prefix 0 -1) (substring name 4)))
                  (t (concat prefix name)))))
        (push (cons name pos) index-alist)
        (pocketlang-accurate-end-of-block end))
       (t
        (if (string= "self" name)
            (if prefix (setq name (substring prefix 0 -1)))
          (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
          (push (cons name pos) index-alist))
        (pocketlang-accurate-end-of-block end)
        (setq beg (point))
        (setq index-alist
              (nconc (pocketlang-imenu-create-index-in-block
                      (concat name (if sing "." "#"))
                      next beg) index-alist))
        (goto-char beg))))
    index-alist))

(defun pocketlang-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (nreverse (pocketlang-imenu-create-index-in-block nil (point-min) nil)))

(defun pocketlang-accurate-end-of-block (&optional end)
  "Jump to the end of the current block or END, whichever is closer."
  (let (state
        (end (or end (point-max))))
    (if pocketlang-use-smie
        (save-restriction
          (back-to-indentation)
          (narrow-to-region (point) end)
          (smie-forward-sexp))
      (while (and (setq state (apply #'pocketlang-parse-partial end state))
                    (>= (nth 2 state) 0) (< (point) end))))))

(defun pocketlang-mode-variables ()
  "Set up initial buffer-local variables for Pocketlang mode."
  (setq indent-tabs-mode nil)
  (smie-setup pocketlang-smie-grammar #'pocketlang-smie-rules
              :forward-token  #'pocketlang-smie--forward-token
              :backward-token #'pocketlang-smie--backward-token)
  (unless pocketlang-use-smie
    (setq-local indent-line-function #'pocketlang-indent-line))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-column pocketlang-comment-column)
  (setq-local comment-start-skip "#+ *")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t))

(defvar pocketlang--electric-indent-chars '(?. ?\) ?} ?\]))

(defun pocketlang--electric-indent-p (char)
  (cond
   ((memq char pocketlang--electric-indent-chars)
    ;; Reindent after typing a char affecting indentation.
    (pocketlang--at-indentation-p (1- (point))))
   ((memq (char-after) pocketlang--electric-indent-chars)
    ;; Reindent after inserting something in front of the above.
    (pocketlang--at-indentation-p (1- (point))))
   ((or (and (>= char ?a) (<= char ?z)) (memq char '(?_ ?? ?! ?:)))
    (let ((pt (point)))
      (save-excursion
        (skip-chars-backward "[:alpha:]:_?!")
        (and (pocketlang--at-indentation-p)
             (looking-at (regexp-opt (cons "end" pocketlang-block-mid-keywords)))
             ;; Outdent after typing a keyword.
             (or (eq (match-end 0) pt)
                 ;; Reindent if it wasn't a keyword after all.
                 (eq (match-end 0) (1- pt)))))))))

(defun pocketlang-indent-line (&optional ignored)
  "Correct the indentation of the current Pocketlang line."
  (interactive)
  (pocketlang-indent-to (pocketlang-calculate-indent)))

(defun pocketlang-indent-to (column)
  "Indent the current line to COLUMN."
  (when column
    (let (shift top beg)
      (and (< column 0) (error "Invalid nesting"))
      (setq shift (current-column))
      (beginning-of-line)
      (setq beg (point))
      (back-to-indentation)
      (setq top (current-column))
      (skip-chars-backward " \t")
      (if (>= shift top) (setq shift (- shift top))
        (setq shift 0))
      (if (and (bolp)
               (= column top))
          (move-to-column (+ column shift))
        (move-to-column top)
        (delete-region beg (point))
        (beginning-of-line)
        (indent-to column)
        (move-to-column (+ column shift))))))

(defun pocketlang-special-char-p (&optional pos)
  "Return t if the character before POS is a special character.
If omitted, POS defaults to the current point.
Special characters are `?', `$', `:' when preceded by whitespace,
and `\\' when preceded by `?'."
  (setq pos (or pos (point)))
  (let ((c (char-before pos)) (b (and (< (point-min) pos)
				      (char-before (1- pos)))))
    (cond ((or (eq c ??) (eq c ?$)))
          ((and (eq c ?:) (or (not b) (eq (char-syntax b) ? ))))
          ((eq c ?\\) (eq b ??)))))

(defun pocketlang-expr-beg (&optional option)
  "Check if point is possibly at the beginning of an expression.
OPTION specifies the type of the expression.
Can be one of `modifier', `expr-qstr', `expr-re'."
  (save-excursion
    (store-match-data nil)
    (let ((space (skip-chars-backward " \t"))
          (start (point)))
      (cond
       ((bolp) t)
       ((progn
          (forward-char -1)
          (and (looking-at "\\?")
               (or (eq (char-syntax (char-before (point))) ?w)
                   (pocketlang-special-char-p))))
        nil)
       ((looking-at pocketlang-operator-re))
       ((or (looking-at "[\\[({,;]")
            (and (looking-at "[!?]")
                 (or (not (eq option 'modifier))
                     (bolp)
                     (save-excursion (forward-char -1) (looking-at "\\Sw$"))))
            (and (looking-at pocketlang-symbol-re)
                 (skip-chars-backward pocketlang-symbol-chars)
                 (cond
                  ((looking-at (regexp-opt
                                (append pocketlang-block-beg-keywords
                                        pocketlang-block-op-keywords
                                        pocketlang-block-mid-keywords)
                                'words))
                   (goto-char (match-end 0))
                   (not (looking-at "\\s_")))
                  ((eq option 'expr-qstr)
                   (looking-at "[a-zA-Z][a-zA-Z0-9_]* +%[^ \t]"))
                  ((eq option 'expr-re)
                   (looking-at "[a-zA-Z][a-zA-Z0-9_]* +/[^ \t]"))
                  (t nil)))))))))

(defun pocketlang-forward-string (term &optional end no-error expand)
  "Move forward across one balanced pair of string delimiters.
Skips escaped delimiters.  If EXPAND is non-nil, also ignores
delimiters in interpolated strings.

TERM should be a string containing either a single, self-matching
delimiter (e.g. \"/\"), or a pair of matching delimiters with the
close delimiter first (e.g. \"][\").

When non-nil, search is bounded by position END.

Throws an error if a balanced match is not found, unless NO-ERROR
is non-nil, in which case nil will be returned.

This command assumes the character after point is an opening
delimiter."
  (let ((n 1) (c (string-to-char term))
        (re (concat "[^\\]\\(\\\\\\\\\\)*\\("
                    (if (string= term "^") ;[^] is not a valid regexp
                        "\\^"
                      (concat "[" term "]"))
                    (when expand "\\|\\(#{\\)")
                    "\\)")))
    (while (and (re-search-forward re end no-error)
                (if (match-beginning 3)
                    (pocketlang-forward-string "}{" end no-error nil)
                  (> (setq n (if (eq (char-before (point)) c)
                                     (1- n) (1+ n))) 0)))
      (forward-char -1))
    (cond ((zerop n))
          (no-error nil)
          ((error "Unterminated string")))))

(defun pocketlang-deep-indent-paren-p (c)
  "TODO: document."
  (cond ((listp pocketlang-deep-indent-paren)
         (let ((deep (assoc c pocketlang-deep-indent-paren)))
           (cond (deep
                  (or (cdr deep) pocketlang-deep-indent-paren-style))
                 ((memq c pocketlang-deep-indent-paren)
                  pocketlang-deep-indent-paren-style))))
        ((eq c pocketlang-deep-indent-paren) pocketlang-deep-indent-paren-style)
        ((eq c ?\( ) pocketlang-deep-arglist)))

(defun pocketlang-parse-partial (&optional end in-string nest depth pcol indent)
  ;; FIXME: Document why we can't just use parse-partial-sexp.
  "TODO: document throughout function body."
  (or depth (setq depth 0))
  (or indent (setq indent 0))
  (when (re-search-forward pocketlang-delimiter end 'move)
    (let ((pnt (point)) w re expand)
      (goto-char (match-beginning 0))
      (cond
       ((and (memq (char-before) '(?@ ?$)) (looking-at "\\sw"))
        (goto-char pnt))
       ((looking-at "[\"`]")            ;skip string
        (cond
         ((and (not (eobp))
               (pocketlang-forward-string (buffer-substring (point) (1+ (point)))
                                    end t t))
          nil)
         (t
          (setq in-string (point))
          (goto-char end))))
       ((looking-at "'")
        (cond
         ((and (not (eobp))
               (re-search-forward "[^\\]\\(\\\\\\\\\\)*'" end t))
          nil)
         (t
          (setq in-string (point))
          (goto-char end))))
       ((looking-at "/=")
        (goto-char pnt))
       ((looking-at "/")
        (cond
         ((and (not (eobp)) (pocketlang-expr-beg 'expr-re))
          (if (pocketlang-forward-string "/" end t t)
              nil
            (setq in-string (point))
            (goto-char end)))
         (t
          (goto-char pnt))))
       ((looking-at "%")
        (cond
         ((and (not (eobp))
               (pocketlang-expr-beg 'expr-qstr)
               (not (looking-at "%="))
               (looking-at "%[QqrxWw]?\\([^a-zA-Z0-9 \t\n]\\)"))
          (goto-char (match-beginning 1))
          (setq expand (not (memq (char-before) '(?q ?w))))
          (setq w (match-string 1))
          (cond
           ((string= w "[") (setq re "]["))
           ((string= w "{") (setq re "}{"))
           ((string= w "(") (setq re ")("))
           ((string= w "<") (setq re "><"))
           ((and expand (string= w "\\"))
            (setq w (concat "\\" w))))
          (unless (cond (re (pocketlang-forward-string re end t expand))
                        (expand (pocketlang-forward-string w end t t))
                        (t (re-search-forward
                            (if (string= w "\\")
                                "\\\\[^\\]*\\\\"
                              (concat "[^\\]\\(\\\\\\\\\\)*" w))
                            end t)))
            (setq in-string (point))
            (goto-char end)))
         (t
          (goto-char pnt))))
       ((looking-at "\\?")              ;skip ?char
        (cond
         ((and (pocketlang-expr-beg)
               (looking-at "\\?\\(\\\\C-\\|\\\\M-\\)*\\\\?."))
          (goto-char (match-end 0)))
         (t
          (goto-char pnt))))
       ((looking-at "\\$")              ;skip $char
        (goto-char pnt)
        (forward-char 1))
       ((looking-at "#")                ;skip comment
        (forward-line 1)
        (goto-char (point))
        )
       ((looking-at "[\\[{(]")
        (setq nest (cons (cons (char-after (point)) pnt) nest))
        (setq depth (1+ depth))
        (goto-char pnt)
        )
       ((looking-at "[])}]")
        (setq depth (1- depth))
        (setq nest (cdr nest))
        (goto-char pnt))
       ((looking-at pocketlang-block-end-re)
        (if (or (and (not (bolp))
                     (progn
                       (forward-char -1)
                       (setq w (char-after (point)))
                       (or (eq ?_ w)
                           (eq ?. w))))
                (progn
                  (goto-char pnt)
                  (setq w (char-after (point)))
                  (or (eq ?_ w)
                      (eq ?! w)
                      (eq ?? w))))
            nil
          (setq nest (cdr nest))
          (setq depth (1- depth)))
        (goto-char pnt))
       ((looking-at "def\\s +[^(\n;]*")
        (if (or (bolp)
                (progn
                  (forward-char -1)
                  (not (eq ?_ (char-after (point))))))
            (progn
              (setq nest (cons (cons nil pnt) nest))
              (setq depth (1+ depth))))
        (goto-char (match-end 0)))
       ((looking-at (concat "\\_<\\(" pocketlang-block-beg-re "\\)\\_>"))
        (and
         (save-match-data
           (or (not (looking-at "do\\_>"))
               (save-excursion
                 (back-to-indentation)
                 (not (looking-at pocketlang-non-block-do-re)))))
         (or (bolp)
             (progn
               (forward-char -1)
               (setq w (char-after (point)))
               (not (or (eq ?_ w)
                        (eq ?. w)))))
         (goto-char pnt)
         (not (eq ?! (char-after (point))))
         (skip-chars-forward " \t")
         (goto-char (match-beginning 0))
         (or (not (looking-at pocketlang-modifier-re))
             (pocketlang-expr-beg 'modifier))
         (goto-char pnt)
         (setq nest (cons (cons nil pnt) nest))
         (setq depth (1+ depth)))
        (goto-char pnt))
       ((looking-at ":\\(['\"]\\)")
        (goto-char (match-beginning 1))
        (pocketlang-forward-string (match-string 1) end t))
       ((looking-at ":\\([-,.+*/%&|^~<>]=?\\|===?\\|<=>\\|![~=]?\\)")
        (goto-char (match-end 0)))
       ((looking-at ":\\([a-zA-Z_][a-zA-Z_0-9]*[!?=]?\\)?")
        (goto-char (match-end 0)))
       ((or (looking-at "\\.\\.\\.?")
            (looking-at "\\.[0-9]+")
            (looking-at "\\.[a-zA-Z_0-9]+")
            (looking-at "\\."))
        (goto-char (match-end 0)))
       (t
        (error "Bad string %s" (buffer-substring (point) pnt))))))
  (list in-string nest depth pcol))

(defun pocketlang-parse-region (start end)
  "TODO: document."
  (let (state)
    (save-excursion
      (if start
          (goto-char start)
        (pocketlang-beginning-of-indent))
      (save-restriction
        (narrow-to-region (point) end)
        (while (and (> end (point))
                    (setq state (apply #'pocketlang-parse-partial end state))))))
    (list (nth 0 state)                 ; in-string
          (car (nth 1 state))           ; nest
          (nth 2 state)                 ; depth
          (car (car (nth 3 state)))     ; pcol
          ;(car (nth 5 state))          ; indent
          )))

(defun pocketlang-indent-size (pos nest)
  "Return the indentation level in spaces NEST levels deeper than POS."
  (+ pos (* (or nest 1) pocketlang-indent-level)))

(defun pocketlang-calculate-indent (&optional parse-start)
  "Return the proper indentation level of the current line."
  ;; TODO: Document body
  (save-excursion
    (beginning-of-line)
    (let ((pocketlang-indent-point (point))
          (case-fold-search nil)
          state eol begin op-end
          (paren (progn (skip-syntax-forward " ")
                        (and (char-after) (matching-paren (char-after)))))
          (indent 0))
      (if parse-start
          (goto-char parse-start)
        (pocketlang-beginning-of-indent)
        (setq parse-start (point)))
      (back-to-indentation)
      (setq indent (current-column))
      (setq state (pocketlang-parse-region parse-start pocketlang-indent-point))
      (cond
       ((nth 0 state)                   ; within string
        (setq indent nil))              ;  do nothing
       ((car (nth 1 state))             ; in paren
        (goto-char (setq begin (cdr (nth 1 state))))
        (let ((deep (pocketlang-deep-indent-paren-p (car (nth 1 state)))))
          (if deep
              (cond ((and (eq deep t) (eq (car (nth 1 state)) paren))
                     (skip-syntax-backward " ")
                     (setq indent (1- (current-column))))
                    ((let ((s (pocketlang-parse-region (point) pocketlang-indent-point)))
                       (and (nth 2 s) (> (nth 2 s) 0)
                            (or (goto-char (cdr (nth 1 s))) t)))
                     (forward-word-strictly -1)
                     (setq indent (pocketlang-indent-size (current-column)
						    (nth 2 state))))
                    (t
                     (setq indent (current-column))
                     (cond ((eq deep 'space))
                           (paren (setq indent (1- indent)))
                           (t (setq indent (pocketlang-indent-size (1- indent) 1))))))
            (if (nth 3 state) (goto-char (nth 3 state))
              (goto-char parse-start) (back-to-indentation))
            (setq indent (pocketlang-indent-size (current-column) (nth 2 state))))
          (and (eq (car (nth 1 state)) paren)
               (pocketlang-deep-indent-paren-p (matching-paren paren))
               (search-backward (char-to-string paren))
               (setq indent (current-column)))))
       ((and (nth 2 state) (> (nth 2 state) 0)) ; in nest
        (if (null (cdr (nth 1 state)))
            (error "Invalid nesting"))
        (goto-char (cdr (nth 1 state)))
        (forward-word-strictly -1)               ; skip back a keyword
        (setq begin (point))
        (cond
         ((looking-at "do\\>[^_]")      ; iter block is a special case
          (if (nth 3 state) (goto-char (nth 3 state))
            (goto-char parse-start) (back-to-indentation))
          (setq indent (pocketlang-indent-size (current-column) (nth 2 state))))
         (t
          (setq indent (+ (current-column) pocketlang-indent-level)))))

       ((and (nth 2 state) (< (nth 2 state) 0)) ; in negative nest
        (setq indent (pocketlang-indent-size (current-column) (nth 2 state)))))
      (when indent
        (goto-char pocketlang-indent-point)
        (end-of-line)
        (setq eol (point))
        (beginning-of-line)
        (cond
         ((and (not (pocketlang-deep-indent-paren-p paren))
               (re-search-forward pocketlang-negative eol t))
          (and (not (eq ?_ (char-after (match-end 0))))
               (setq indent (- indent pocketlang-indent-level))))
         ((and
           (save-excursion
             (beginning-of-line)
             (not (bobp)))
           (or (pocketlang-deep-indent-paren-p t)
               (null (car (nth 1 state)))))
          ;; goto beginning of non-empty no-comment line
          (let (end done)
            (while (not done)
              (skip-chars-backward " \t\n")
              (setq end (point))
              (beginning-of-line)
              (if (re-search-forward "^\\s *#" end t)
                  (beginning-of-line)
                (setq done t))))
          (end-of-line)
          ;; skip the comment at the end
          (skip-chars-backward " \t")
          (let (end (pos (point)))
            (beginning-of-line)
            (while (and (re-search-forward "#" pos t)
                        (setq end (1- (point)))
                        (or (pocketlang-special-char-p end)
                            (and (setq state (pocketlang-parse-region
                                              parse-start end))
                                 (nth 0 state))))
              (setq end nil))
            (goto-char (or end pos))
            (skip-chars-backward " \t")
            (setq begin (if (and end (nth 0 state)) pos (cdr (nth 1 state))))
            (setq state (pocketlang-parse-region parse-start (point))))
          (or (bobp) (forward-char -1))
          (and
           (or (and (looking-at pocketlang-symbol-re)
                    (skip-chars-backward pocketlang-symbol-chars)
                    (looking-at (concat "\\<\\(" pocketlang-block-hanging-re
                                        "\\)\\>"))
                    (not (eq (point) (nth 3 state)))
                    (save-excursion
                      (goto-char (match-end 0))
                      (not (looking-at "[a-z_]"))))
               (and (looking-at pocketlang-operator-re)
                    (not (pocketlang-special-char-p))
                    (save-excursion
                      (forward-char -1)
                      (or (not (looking-at pocketlang-operator-re))
                          (not (eq (char-before) ?:))))
                    ;; Operator at the end of line.
                    (let ((c (char-after (point))))
                      (and
;;                     (or (null begin)
;;                         (save-excursion
;;                           (goto-char begin)
;;                           (skip-chars-forward " \t")
;;                           (not (or (eolp) (looking-at "#")
;;                                    (and (eq (car (nth 1 state)) ?{)
;;                                         (looking-at "|"))))))
                       ;; Not a regexp or percent literal.
                       (null (nth 0 (pocketlang-parse-region (or begin parse-start)
                                                       (point))))
                       (or (not (eq ?| (char-after (point))))
                           (save-excursion
                             (or (eolp) (forward-char -1))
                             (cond
                              ((search-backward "|" nil t)
                               (skip-chars-backward " \t\n")
                               (and (not (eolp))
                                    (progn
                                      (forward-char -1)
                                      (not (looking-at "{")))
                                    (progn
                                      (forward-word-strictly -1)
                                      (not (looking-at "do\\>[^_]")))))
                              (t t))))
                       (not (eq ?, c))
                       (setq op-end t)))))
           (setq indent
                 (cond
                  ((and
                    (null op-end)
                    (not (looking-at (concat "\\<\\(" pocketlang-block-hanging-re
                                             "\\)\\>")))
                    (eq (pocketlang-deep-indent-paren-p t) 'space)
                    (not (bobp)))
                   (goto-char (or begin parse-start))
                   (skip-syntax-forward " ")
                   (current-column))
                  ((car (nth 1 state)) indent)
                  (t
                   (+ indent pocketlang-indent-level))))))))
      (goto-char pocketlang-indent-point)
      (beginning-of-line)
      (skip-syntax-forward " ")
      (if (looking-at "\\.[^.]\\|&\\.")
          (+ indent pocketlang-indent-level)
        indent))))

(defun pocketlang-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.
With ARG, move backward multiple defuns.  Negative ARG means
move forward."
  (interactive "p")
  (let (case-fold-search)
    (and (re-search-backward (concat "^\\s *" pocketlang-defun-beg-re "\\_>")
                             nil t (or arg 1))
         (beginning-of-line))))

(defun pocketlang-end-of-defun ()
  "Move point to the end of the current defun.
The defun begins at or after the point.  This function is called
by `end-of-defun'."
  (interactive "p")
  (forward-sexp)
  (let (case-fold-search)
    (when (looking-back (concat "^\\s *" pocketlang-block-end-re)
                        (line-beginning-position))
      (forward-line 1))))

(defun pocketlang-beginning-of-indent ()
  "Backtrack to a line which can be used as a reference for
calculating indentation on the lines after it."
  (while (and (re-search-backward pocketlang-indent-beg-re nil 'move)
              (if (pocketlang-in-ppss-context-p 'anything)
                  t
                ;; We can stop, then.
                (beginning-of-line)))))

(defun pocketlang-move-to-block (n)
  "Move to the beginning (N < 0) or the end (N > 0) of the
current block, a sibling block, or an outer block.  Do that (abs N) times."
  (back-to-indentation)
  (let ((signum (if (> n 0) 1 -1))
        (backward (< n 0))
        (depth (or (nth 2 (pocketlang-parse-region (point) (line-end-position))) 0))
        case-fold-search
        down done)
    (when (looking-at pocketlang-block-mid-re)
      (setq depth (+ depth signum)))
    (when (< (* depth signum) 0)
      ;; Moving end -> end or beginning -> beginning.
      (setq depth 0))
    (dotimes (_ (abs n))
      (setq done nil)
      (setq down (save-excursion
                   (back-to-indentation)
                   ;; There is a block start or block end keyword on this
                   ;; line, don't need to look for another block.
                   (and (re-search-forward
                         (if backward pocketlang-block-end-re
                           (concat "\\_<\\(" pocketlang-block-beg-re "\\)\\_>"))
                         (line-end-position) t)
                        (not (nth 8 (syntax-ppss))))))
      (while (and (not done) (not (if backward (bobp) (eobp))))
        (forward-line signum)
        (cond
         ;; Skip empty and commented out lines.
         ((looking-at "^\\s *$"))
         ((looking-at "^\\s *#"))
         ;; Jump over a multiline literal.
         ((pocketlang-in-ppss-context-p 'string)
          (goto-char (nth 8 (syntax-ppss)))
          (unless backward
            (forward-sexp)
            (when (bolp) (forward-char -1)))) ; After a heredoc.
         (t
          (let ((state (pocketlang-parse-region (point) (line-end-position))))
            (unless (car state) ; Line ends with unfinished string.
              (setq depth (+ (nth 2 state) depth))))
          (cond
           ;; Increased depth, we found a block.
           ((> (* signum depth) 0)
            (setq down t))
           ;; We're at the same depth as when we started, and we've
           ;; encountered a block before.  Stop.
           ((and down (zerop depth))
            (setq done t))
           ;; Lower depth, means outer block, can stop now.
           ((< (* signum depth) 0)
            (setq done t)))))))
    (back-to-indentation)))

(defun pocketlang-beginning-of-block (&optional arg)
  "Move backward to the beginning of the current block.
With ARG, move up multiple blocks."
  (interactive "p")
  (pocketlang-move-to-block (- (or arg 1))))

(defun pocketlang-end-of-block (&optional arg)
  "Move forward to the end of the current block.
With ARG, move out of multiple blocks."
  (interactive "p")
  (pocketlang-move-to-block (or arg 1)))

(defun pocketlang-in-ppss-context-p (context &optional ppss)
  (let ((ppss (or ppss (syntax-ppss (point)))))
    (if (cond
         ((eq context 'anything)
          (or (nth 3 ppss)
              (nth 4 ppss)))
         ((eq context 'string)
          (nth 3 ppss))
         ((eq context 'non-heredoc)
          (and (pocketlang-in-ppss-context-p 'anything)
               (not (pocketlang-in-ppss-context-p 'heredoc))))
         ((eq context 'comment)
          (nth 4 ppss))
         (t
          (error (concat
                  "Internal error on `pocketlang-in-ppss-context-p': "
                  "context name `%s' is unknown")
                 context)))
        t)))

(defconst pocketlang-font-lock-keyword-beg-re "\\(?:^\\|[^.@$:]\\|\\.\\.\\)")

(defconst pocketlang-font-lock-keywords
  `(;; Functions.
    ("^\\s *def\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)"
     1 font-lock-function-name-face)
    ;; Keywords.
    (,(concat
       pocketlang-font-lock-keyword-beg-re
       (regexp-opt
        '("and"
          "break"
          "class"
          "fn"
          "def"
          "do"
          "else"
          "for"
          "end"
          "if"
          "in"
          "not"
          "or"
          "is"
          "from"
          "import"
          "as"
          "return"
          "self"
          "super"
          "then"
          "while"
          "yield")
        'symbols))
     (1 font-lock-keyword-face))
    ;; Singleton objects.
    (,(concat pocketlang-font-lock-keyword-beg-re
              "\\_<\\(null\\|true\\|false\\)\\_>")
     1 font-lock-constant-face)
    ;; Symbols.
    ("\\(^\\|[^:]\\)\\(:@\\{0,2\\}\\(?:\\sw\\|\\s_\\)+\\)"
     (2 font-lock-constant-face))
    ("\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+"
     0 font-lock-variable-name-face)
    ;; Constants.
    ("\\_<\\([A-Z]+\\(\\w\\|_\\)*\\)"
     1 (unless (eq ?\( (char-after)) font-lock-type-face))
    ;; Expression expansion.
    (pocketlang-match-expression-expansion
     0 font-lock-variable-name-face t)
    ;; Negation char.
    ("\\(?:^\\|[^[:alnum:]_]\\)\\(!+\\)[^=~]"
     1 font-lock-negation-char-face)
    ;; Character literals.
    ;; FIXME: Support longer escape sequences.
    ("\\?\\\\?\\_<.\\_>" 0 font-lock-string-face)
    ;; Regexp options.
    ("\\(?:\\s|\\|/\\)\\([imxo]+\\)"
     1 (when (save-excursion
               (let ((state (syntax-ppss (match-beginning 0))))
                 (and (nth 3 state)
                      (or (eq (char-after) ?/)
                          (progn
                            (goto-char (nth 8 state))
                            (looking-at "%r"))))))
         font-lock-preprocessor-face))
    )
  "Additional expressions to highlight in Pocketlang mode.")

(defun pocketlang-match-expression-expansion (limit)
  (let* ((prop 'pocketlang-expansion-match-data)
         (pos (next-single-char-property-change (point) prop nil limit))
         value)
    (when (and pos (> pos (point)))
      (goto-char pos)
      (or (and (setq value (get-text-property pos prop))
               (progn (set-match-data value) t))
          (pocketlang-match-expression-expansion limit)))))

;;;###autoload
(define-derived-mode pocketlang-mode prog-mode "Pocketlang"
  "Major mode for editing Pocketlang code."
  (pocketlang-mode-variables)

  (setq-local imenu-create-index-function #'pocketlang-imenu-create-index)
  (setq-local beginning-of-defun-function #'pocketlang-beginning-of-defun)
  (setq-local end-of-defun-function #'pocketlang-end-of-defun)

  (add-hook 'electric-indent-functions #'pocketlang--electric-indent-p nil 'local)

  (setq-local font-lock-defaults '((pocketlang-font-lock-keywords) nil nil
                                   ((?_ . "w"))))
  )

;;; Invoke pocketlang-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pk?\\'" . pocketlang-mode))

;; ;;;###autoload
;; (dolist (name (list "pocketlang" "rbx" "jpocketlang" "pocketlang1.9" "pocketlang1.8"))
;;   (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'pocketlang-mode)))

(provide 'pocketlang-mode)

;;; pocketlang-mode.el ends here
