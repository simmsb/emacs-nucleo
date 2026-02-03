;;; -*- lexical-binding: t; -*-

(require 'nucleo-module)

(defvar nucleo--searchers (make-hash-table :test 'equal))
(defvar nucleo--all (make-hash-table :test 'equal))
(defvar nucleo--ttl 600)
(defvar nucleo--filtering-p nil)
(defvar nucleo--prune-timer nil)

(defun nucleo--prune ()
  (when nucleo--searchers
    (maphash (pcase-lambda (k `(,_ . ,last-access))
               (if (< last-access (- (float-time) nucleo--ttl))
                   (remhash k nucleo--searchers)))
             nucleo--searchers))
  (when nucleo--all
    (maphash (pcase-lambda (k `(,_ . ,last-access))
               (if (< last-access (- (float-time) nucleo--ttl))
                   (remhash k nucleo--all)))
             nucleo--all)))

(defun nucleo--ensure-timer ()
  (unless nucleo--prune-timer
    (setq nucleo--prune-timer (run-at-time t nucleo--ttl #'nucleo--prune))))

(defun nucleo--get-searcher (haystack ignore-case)
  (pcase (gethash haystack nucleo--searchers)
    (`(,searcher . ,_)
     (puthash haystack (cons searcher (float-time)) nucleo--searchers)
     searcher)
    (_
     (let ((nuc (nucleo-module-new t ignore-case t t)))
       (nucleo-module-feed nuc haystack)
       (puthash haystack (cons nuc (float-time)) nucleo--searchers)
       nuc))))

(defun nucleo--do-filter (needle all ignore-case)
  (let ((nuc (nucleo--get-searcher all ignore-case)))
    (nucleo-module-set-search nuc needle)
    (while (progn
             (pcase-let ((`(,running ,changed) (nucleo-module-tick nuc 10)))
               ;; (message "running: %s changed: %s" running changed)
               (or running changed))))
    (nucleo-module-results nuc all)))

(defun nucleo--get-all (table pred needle prefix)
  (pcase (gethash table nucleo--all)
    (`(,all . ,_)
     (puthash table (cons all (float-time)) nucleo--all)
     all)
    (_
     (let ((all (if (and (string= prefix "") (stringp (car-safe table))
                         (not (or pred completion-regexp-list (string= needle ""))))
                    table (all-completions prefix table pred))))
       (puthash table (cons all (float-time)) nucleo--all)
       all))))

(defun nucleo-highlight (result)
  "Highlight destructively the characters NEEDLE matched in HAYSTACK.
HAYSTACK has to be a match according to `hotfuzz-all-completions'."
  (when-let* ((spans (get-text-property 0 'spans result)))
    (cl-loop for (start . end) in spans
             do (add-face-text-property start (+ end 1) 'completions-common-part nil result)))
  result)

;;;###autoload
(defun nucleo-all-completions (string table &optional pred point)
  "Get nucleo-completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT.
This function prematurely sorts the completions; mutating the result
before passing it to `display-sort-function' or `cycle-sort-function'
will lead to inaccuracies."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (if point (substring string point) ""))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (needle (substring beforepoint (car bounds)))
         (all (nucleo--get-all table pred needle prefix))
         (results (nucleo--do-filter needle all completion-ignore-case)))
    (nucleo--ensure-timer)
    (setq nucleo--filtering-p (not (string= needle "")))
    (defvar completion-lazy-hilit-fn) ; Introduced in Emacs 30 (bug#47711)
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn #'nucleo-highlight))
    (cl-loop for (value spans) in results
             collect (propertize value 'spans spans))))

;;;###autoload
(defun nucleo--adjust-metadata (metadata)
  "Adjust completion METADATA for nucleo sorting."
  (if nucleo--filtering-p
      `(metadata (display-sort-function . identity) (cycle-sort-function . identity)
        . ,(cdr metadata))
    metadata))

;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '(nucleo completion-flex-try-completion nucleo-all-completions
                 "Nucleo fuzzy completion."))
  (put 'nucleo 'completion--adjust-metadata #'nucleo--adjust-metadata))


(provide 'nucleo)
