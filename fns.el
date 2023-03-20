;;; -*- lexical-binding: t; -*-
;;; fns.el --- Random drawer of functions.

(require 'dash)

(defun mean (xs)
  (/ (apply #'+ xs) (+ 0.0 (length xs))))

(defun std (xs)
  (let ((xs-squared (--map (* it it) xs))
        (mean-xs (mean xs)))
    (sqrt (- (mean xs-squared) (* mean-xs mean-xs)))))

(defun logodds (p)
  "Computes the logodds of P."
  (assert (and (>= p 0.0) (<= p 1.0)) nil "probabilities must be between 0 and 1")
  (- (log p) (log (- 1 p))))

(defun odds (p)
  "Computes the odds from P."
  (/ p (- 1 p)))

(defun harmonic-mean (&rest xs)
  "Computes the harmonic mean of the XS."
  (/ (length xs) (apply '+ (mapcar (lambda (x) (/ 1.0 x)) xs))))


(defun coin-flips (p n &optional result)
  "Flips N coins with probability P.
Returns a list of flips as T and NIL."
  (cond ((<= n 0) result)
        (t
         (let ((x (/ (random) (+ 0.0 most-positive-fixnum))))
           (coin-flips p (- n 1) (cons (if (> x p) t nil) result))))))


(defun binomial-likelihood (p xs &optional result)
  "Computes the likelihood of a sequence XS given probabilty P."
  (let ((q (or result 1.0)))
    (cond ((null xs) result)
          ((car xs)
           (binomial-likelihood p (cdr xs) (* p q)))
          (t
           (binomial-likelihood p (cdr xs) (* (- 1.0 p) q))))))

(defun ffnn-flops (&rest layers)
  "Computes the number of floating point required for LAYERS."
  (apply #'+ (--map (* (car it) (cdr it)) (-zip layers (-drop 1 layers)))))

(defun prime? (n)
  "Returns 't if N is prime."
  (let ((possible-factors (--unfold (unless (> it (sqrt n)) (cons it (1+ it))) 2)))
    (--none? (= (mod n it) 0) possible-factors)))

(defun divisors (n)
  "Returns the divisors of N as a list."
  (let ((possible-factors (--unfold (unless (> it (sqrt n)) (cons it (1+ it))) 2)))
    (--filter (= (mod n it) 0) possible-factors)))

(defun birthday-prob (bins attempts)
  "Returns the likelihood of a collision for ATTEMPTS among BINS."
  (if (> bins attempts)
      (- 1.0 (exp (-reduce #'+ (--map (- (log (- bins attempts)) (log bins))
                               (number-sequence 0 (- attempts 1))))))
    1.0))

(defun plist--without-r (plist keys res)
  (cond ((null plist) res)
        ((memq (car plist) keys)
         (plist--without-r (cddr plist) keys res))
        (t
         (plist--without-r (cddr plist) keys (append res (list (car plist) (cadr plist)))))))

(defun plist-without (plist keys-to-remove)
  "Returns PLIST with the specified KEYS-TO-REMOVE removed.

KEYS-TO-REMOVE can either be a single key (i.e., a symbol) or a list."
  (let ((keys (if (symbolp keys-to-remove) (list keys-to-remove) keys-to-remove)))
    (plist--without-r plist keys nil)))


(defun plist--merge-r (old new res)
  (if (null old)
      (append res new)
    (let ((key (car old))
          (val (cadr old))
          (rest (cddr old)))
      (if (plist-member new key)
          (plist--merge-r rest (plist-without new key) (append res (list key (plist-get new key))))
        (plist--merge-r rest new (append res (list key val)))))))

(defun plist-merge (old-plist &rest new-plists)
  "Iteratively replaces keys from OLD-PLIST with values from NEW-PLISTS."
  (if (null new-plists)
      old-plist
    (apply #'plist-merge (plist--merge-r old-plist (car new-plists) nil) (cdr new-plists))))


(defun plist-flatten (plist)
  "Tries to sensibly flatten PLIST.

For example:

  (:foo \"bar\" nil :baz 3) => (:foo \"bar\" :baz 3)
  (:foo \"bar\" (list :baz 3)) => (:foo \"bar\" :baz 3)
"
  (if (null plist) '()
    (let ((head (car plist))
          (tail (cdr plist)))
      (cond
       ((null head)
        (plist-flatten tail))
       ((symbolp head)
        (append (list head (cadr plist)) (plist-flatten (cddr plist))))
       ((listp head)
        (append (plist-flatten head) (plist-flatten tail)))
       (t
        (error "unexpected plist structure"))))))

(defun plist-get-nonempty (plist key)
  "Extract a non-empty value from a property list.

Returns 'nil unless PLIST's value for KEY is a non-empty string,
in which case, the actual value is returned."
  (let ((res (plist-get plist key)))
    (when (nonempty res) res)))

(defun plist-get-as-string (plist key)
  "Extracts a value from a plist as a string."
  (format "%s" (plist-get plist key)))

(defun args--flag-value-to-string (value)
  "Converts VALUE for use as a flag value."
  (cond ((stringp value) value)
        ((numberp value) (number-to-string value))
        ((eq value t) "true")
        ((eq value nil) "false")))

(defun args--keyword-to-flag (kw)
  "Converts symbol KW to a string for use as a flag."
  (concat "--" (replace-regexp-in-string "-" "_" (substring (symbol-name kw) 1))))

(defun args--maybe-quote-comma-list (value)
  "Optionally quotes VALUE based on whether or not it contains a comma."
  (if (string-match-p "," value)   ; Wrap in double-quotes, escape existing
      (concat "\""
              (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\"") value)
              "\"")
    value))

(defun args--vars-value-to-string (value)
  "Converts VALUE to a string for use as a `--vars' value."
  (cond ((stringp value) (args--maybe-quote-comma-list value))
        ((numberp value) (number-to-string value))

        ;; Unfortunately, the empty list and nil are ambiguous synonyms. In this
        ;; context, we'll assume that nil means "false" since that has seemed to
        ;; be a much more common way to interpret it.
        ((booleanp value) (if value "true" "false"))
        ((listp value)
         (let ((quoted-values (-map (lambda (x) (concat "\"" x "\"")) value)))
           (concat "[" (string-join quoted-values ",") "]")))))

(defun args--keyword-to-var (kw)
  "Converts keyword symbol KW to a string for use as a `--vars' keyword."
  (concat (replace-regexp-in-string "-" "_" (substring (symbol-name kw) 1))))

(defun args--plist-to-r (keyword-fn value-fn args)
  "Converts plist ARGS into a list of flags.

Applies KEYWORD-FN and VALUE-FN to the keyword and value,
respectively, to generate appropriate strings."
  (cond ((null args) '())
        ((let ((flag-name (funcall keyword-fn (car args)))
               (flag-value (funcall value-fn (cadr args))))
           (cons (concat flag-name "=" flag-value)
                 (args--plist-to-r keyword-fn value-fn (cddr args)))))))

(defun args-plist-to-flags (arg-or-list &rest more-args)
  "Returns a list of flags from plist ARGS.

For example '(:foo 12 :baz nil) yields '(\"--foo=12\" \"--baz=false\")."
  (let ((args (if (symbolp arg-or-list) (cons arg-or-list more-args)
                arg-or-list)))
    (args--plist-to-r #'args--keyword-to-flag #'args--flag-value-to-string args)))

(defun args-plist-to-urlparams (args)
  "Returns a URL parameter string from plist ARGS."
  (string-join
   (--map (format "%s=%s"
                  (args--keyword-to-var (car it))
                  (url-hexify-string (args--vars-value-to-string (cadr it))))
          (-partition 2 args))
   "&"))

(defun snake-case-symbol (sym)
  "Return SYM as a snake_case version of itself; i.e., replace dashes with underscores."
  (intern (replace-regexp-in-string "-" "_" (symbol-name sym))))

(defun plist-snake-keys (plist)
  "Returns PLIST with all the keys converted to snake-case"
  (-flatten-n
   1
   (--map (list (snake-case-symbol (car it))
                (let ((value (cadr it)))
                  (cond ((and (listp value)
                              (not (null value))
                              (symbolp (car value)))
                         (plist-snake-keys value))
                        ((and (vectorp value)
                              (length> value 0)
                              (listp (aref value 0)))
                         (apply #'vector (-map #'plist-snake-keys value)))
                        (t value))))
          (-partition 2 plist))))

(defun kebab-case-symbol (sym)
  "Return SYM as a kebab-case version of itself; i.e., replace underscores with dashes."
  (intern (replace-regexp-in-string "_" "-" (symbol-name sym))))

(defun plist-kebab-keys (plist)
  "Returns PLIST with all the keys converted to kebab-case"
  (-flatten-n
   1
   (--map (list (kebab-case-symbol (car it))
                (let ((value (cadr it)))
                  (cond ((and (listp value)
                              (not (null value))
                              (symbolp (car value)))
                         (plist-kebab-keys value))
                        ((and (vectorp value)
                              (length> value 0)
                              (listp (aref value 0)))
                         (apply #'vector (-map #'plist-kebab-keys value)))
                        (t value))))
          (-partition 2 plist))))

(defun symbols-to-flags (xs)
  "Produce an argument list from a mixed list XS of keywords and values."
  (cond ((null xs) nil)
        ((symbolp (car xs))
         (let ((flag (args--keyword-to-flag (car xs)))
               (value (args--flag-value-to-string (cadr xs))))
           (cons (concat flag "=" value) (symbols-to-flags (cddr xs)))))
        (t
         (cons (car xs) (symbols-to-flags (cdr xs))))))

(defun append-string-to-buffer (buffer string)
  "Append STRING to BUFFER.

Modified from `append-to-buffer', defined in the emacs-lisp intro."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer
                                            (current-buffer) t))
         (read-string "String to append: ")))

  (let* ((append-to (get-buffer-create buffer))
         (windows (get-buffer-window-list append-to t t))
         move-point-in-windows)
    (with-current-buffer append-to
      (barf-if-buffer-read-only)
      ;; record in which windows we should keep point at eob.
      (dolist (window windows)
        (when (= (window-point window) (point-max))
          (push window move-point-in-windows)))
      (let (return-to-position)
        ;; decide whether we should reset point to return-to-position
        ;; or leave it at eob.
        (unless (= (point) (point-max))
          (setq return-to-position (point))
          (goto-char (point-max)))
        (insert string)
        (when return-to-position
          (goto-char return-to-position)))
      ;; advance to point-max in windows where it is needed
      (dolist (window move-point-in-windows)
        (set-window-point window (point-max))))))

(defun pairs (xs)
  "Compute distinct unordered pairs of elements in XS."
  (cond ((null xs) '())
        ((null (rest xs)) '())
        (t
         (append
          (mapcar (lambda (x) (cons (first xs) x)) (rest xs))
          (pairs (rest xs))))))

(defun to-dhm (ddhhmm)
  "Converts the DDHHMM numeric value (days, hours, minutes) into
  a numeric list."
  (let* ((ts (cond ((stringp ddhhmm) ddhhmm)
                   ((numberp ddhhmm) (number-to-string ddhhmm))
                   (t (error "argument should be a string or a number"))))
         (ts (if (eq (length ts) 5) (concat "0" ts) ts))
         (d (string-to-number (substring ts 0 2)))
         (h (string-to-number (substring ts 2 4)))
         (m (string-to-number (substring ts 4 6))))
    (list d h m)))

(defun dhm-diff (t1 t2)
  "Computes T1 - T2 in minutes, where T1 and T2 are DDHHMM times
  e.g. from TO-DHM."
  (let* ((d (- (nth 0 t1) (nth 0 t2)))
         (h (- (nth 1 t1) (nth 1 t2)))
         (m (- (nth 2 t1) (nth 2 t2))))
    (+ (* 24 60 d) (* 60 h) m)))

(defun from-rkm-string (s)
  "Converts S from a string like `16k' to a number e.g. 16384."
  (assert (stringp s))
  (if (null (string-match "^\\([0-9]+\\)\\([gmk]\\)?$" s))
      (error "expected string matching ###(gmk)")
    (let ((num (substring s (match-beginning 1) (match-end 1)))
          (suffix (when (match-beginning 2) (substring s (match-beginning 2) (match-end 2)))))
      (* (string-to-number num)
         (cond ((null suffix) 1)
               ((member suffix '("K" "k")) 1024)
               ((member suffix '("M" "m")) (* 1024 1024))
               ((member suffix '("G" "g")) (* 1024 1024 1024)))))))

(defun empty-stringp (x)
  "Returns t if X is a string and is empty, otherwise nil."
  (and (stringp x) (s-blank? x)))

(defun nonempty (x)
  "If X is the empty string, returns nil; otherwise returns X."
  (if (empty-stringp x) nil x))

(defun random-words (pos n)
  "Uses Wordnet to retrieve N random words with part-of-speech POS.

POS can be `noun' or `adj' or whatever."
  (split-string
   (shell-command-to-string
    (format "grep -E '^[a-z]+ ' /usr/share/wordnet/index.%s | cut -f1 -d' ' | sort -R | head -%d" pos n))
   "\n" t))

(defun random-codenames (n)
  "Generates N random codenames."
  (let ((adjs (random-words "adj" n))
        (nouns (random-words "noun" n)))
    (--map (concat (car it) " " (cdr it)) (-zip-pair adjs nouns))))

(defun maybe-tmux-wrap (seq)
  "Conditionally wraps the terminal escape SEQ for tmux."
  (if (not (null (getenv "TMUX")))
      (concat "\ePtmux;\e" seq "\e\\")
    seq))

(defun hterm-notify (title body)
  "Send a terminal notification with TITLE and BODY via hterm."
  (let ((seq (concat "\e]777;notify;" title ";" body "\a")))
    (send-string-to-terminal (maybe-tmux-wrap seq))))

(defun flatten-list (&rest args)
  "Applies `-flatten` to ARGS."
  (apply #'-flatten (list args)))

(defmacro --unless-empty-stringp-let (val body)
  "If VAL evaluates to anything but the empty string, then bind
it to symbol 'it' and do BODY."
  `(let ((it ,val)) (unless (empty-stringp it) ,body)))

(defun set-terminal-title (title)
  "Renames the window title, at least in iTerm2"
  (send-string-to-terminal (concat "\e]0;" title "\007")))

(provide 'fns)
