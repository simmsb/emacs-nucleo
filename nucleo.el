;;; -*- lexical-binding: t; -*-

(require 'nucleo-module)

(defvar nucleo--current-searcher nil)
(defvar nucleo--last-used-haystack nil)

(defun nucleo--do-filter (needle all ignore-case)
  (when (or (not (eq all nucleo--last-used-haystack)) (null nucleo--current-searcher))
    (setq nucleo--last-used-haystack all)
    (setq nucleo--current-searcher (nucleo-module-new t ignore-case t t))
    (nucleo-module-feed nucleo--current-searcher all))
  (nucleo-module-set-search nucleo--current-searcher needle)
  (while (progn
           (pcase-let ((`(,running ,changed) (nucleo-module-tick nucleo--current-searcher 10)))
             ;; (message "running: %s changed: %s" running changed)
             (or running changed))))
  (nucleo-module-results nucleo--current-searcher))


(defun nucleo-highlight (result)
  "Highlight destructively the characters NEEDLE matched in HAYSTACK.
HAYSTACK has to be a match according to `hotfuzz-all-completions'."
  (when-let ((spans (get-text-property 0 'spans result)))
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
         (all (if (and (string= prefix "") (stringp (car-safe table))
                       (not (or pred completion-regexp-list (string= needle ""))))
                  table (all-completions prefix table pred)))
         (results (nucleo--do-filter needle all completion-ignore-case)))
    (setq nucleo--filtering-p (not (string= needle "")))
    (defvar completion-lazy-hilit-fn) ; Introduced in Emacs 30 (bug#47711)
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn #'nucleo-highlight))
    (cl-loop for (score value spans) in results
             collect (propertize value 'spans spans 'score score))))

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
