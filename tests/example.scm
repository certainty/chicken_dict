;;; example.scm --- 
;; 
;; Filename: example.scm
;; Description: 
;; Author: David Krentzlin <david@lisp-unleashed.de>
;; Maintainer: 
;; Created: Di Apr 21 21:50:41 2009 (CEST)
;; Version: $Id$
;; Version: 
;; Last-Updated: Mi Apr 22 22:48:40 2009 (CEST)
;;           By: David Krentzlin <david@lisp-unleashed.de>
;;     Update #: 7
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;; RCS $Log$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Copyright (c) 2009 David Krentzlin <david@lisp-unleashed.de>
;; 
;;   Permission is hereby granted, free of charge, to any person
;;   obtaining a copy of this software and associated documentation
;;   files (the "Software"), to deal in the Software without
;;   restriction, including without limitation the rights to use,
;;   copy, modify, merge, publish, distribute, sublicense, and/or sell
;;   copies of the Software, and to permit persons to whom the
;;   Software is furnished to do so, subject to the following
;;   conditions:
;; 
;;   The above copyright notice and this permission notice shall be
;;   included in all copies or substantial portions of the Software.
;; 
;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;   OTHER DEALINGS IN THE SOFTWARE.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(use dict)

(*current-log-port* (current-output-port))

(define con (connect "dict.org"))

(receive (success matches) (!match con "scheme" db: 'all)
  (if success
      (printf "Found matches: ~A~%" matches)))

(receive (success def) (!define con "Scheme-to-C" db: "foldoc")
  (if (and success (not (null? def)))
      (printf "Defintion for Scheme-to-C: ~A~%" (definition-text (car def)))))

(receive (success strats) (!strategies con)
  (if success
      (printf "Strategies: ~A~%" strats)))

(disconnect con)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example.scm ends here
