;;; bench.el --- Benchmarks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ben Simms
;;
;; Author: Ben Simms <ben@bensimms.moe>
;; Maintainer: Ben Simms <ben@bensimms.moe>
;; Created: September 26, 2025
;; Modified: September 26, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/simmsb/bench
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Benchmarks
;;
;;; Code:


(setq random-col (all-completions "" 'help--symbol-completion-table nil))

(benchmark-run-compiled 10 (nucleo-all-completions "a" random-col))
(benchmark-run-compiled 10 (hotfuzz-all-completions "a" random-col))

;;; bench.el ends here
