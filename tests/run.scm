;; Author: David Krentzlin <david@lisp-unleashed.de>
;; Created: Di Apr 14 21:01:34 2009 (CEST)
;; Last-Updated: Sa Apr 24 09:00:52 2010 (CEST)
;;           By: David Krentzlin
;;     Update #: 292
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

(use test utf8 dict md5)

;;some helpers
(define (with-stubbed-io input proc)
  (let ((result #f)
        (output-text #f))
    (set! output-text (call-with-output-string (lambda (outport)
                                                 (call-with-input-string input
                                                                         (lambda (inport)
                                                                           (let ((con (make-connection input-port: inport output-port: outport connected: #t msg-id: "<test@example.org>")))
                                                                             (set! result (proc con))))))))
    `((output . ,output-text) (result . ,result))))

(define closed-connection (make-connection connected: #f))
(define (connection-with-stubbed-output p)
  (make-connection connected: #t output-port: p))

(define (connection-with-stubbed-input p)
  (make-connection connected: #t input-port: p))


(define test-make-status-response cons)
(define test-make-banner list)
                                                                            
(test-begin "dict-unit")


(test-begin "status")

(test "can decide if an object is a status-response"
      #t
      (status-response? (test-make-status-response 400 "message")))

(test "a status-response is a pair"
      #f
      (status-response? "non-pair"))

(test "the car of a status-response must be a number"
      #f
      (status-response? (cons "test" "test")))

(test "the cdr of a status-response must be a string"
      #f
      (status-response? (cons 500 0)))

(test "status-code extracts the status-code"
      500
      (response-status-code (test-make-status-response 500 "test")))

(test "status-message extracts the message"
      "test"
      (response-status-message (test-make-status-response 500 "test")))


(test "it can be asked if a status response represents an error"
      #t
      (response-status-error? (test-make-status-response 400 "test")))


(test "status->string retrieves the textual representation of a status-code"
      "ok"
      (string-downcase (response-status-code->string 250)))

;;TODO: find out how to test macro-expansion
(test "define-status expands correctly" #t #t)
      
(test-end "status")
            

(test-begin "commands")

   (test "match success"
         '((output .  "MATCH ! . test\r\n")
           (result #t  (("db2" "match2" "match1") ("db1" "match2" "match1"))))
         (with-stubbed-io "152 n matches retrieved\r\ndb1 \"match1\"\r\ndb1 \"match2\"\r\ndb2 \"match1\"\r\ndb2  \"match2\"\r\n.\r\n250 ok\r\n"
                                 (lambda (con)
                                   (receive (success result) (!match con "test")
                                     (list success result)))))

   (test "match failure"
         `((output . "MATCH ! . test\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (success result) (!match con "test")
                              (list success result)))))

   (test "define success"
         '((output . "DEFINE ! test\r\n")
           (result #t (("foo" "jargon" "jargon-descr" "foo\nsome-text\n")
                       ("foo" "db1" "db1-descr" "some more text\nand more\n"))))
         (with-stubbed-io "150 n definitions retrieved\r\n151 \"foo\" jargon \"jargon-descr\"\r\nfoo\r\nsome-text\r\n.\r\n151 \"foo\" db1 \"db1-descr\"\r\nsome more text\r\nand more\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!define con "test")
                              (list s r)))))                   
   (test "define failure"
         `((output . "DEFINE ! test\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!define con "test")
                              (list s r)))))

   (test "databases success"
         `((output . "SHOW DB\r\n")
           (result #t (("db1" "db-descr") ("db2" "db2-descr"))))
         (with-stubbed-io "110 2 databases present\r\ndb1 db-descr\r\ndb2 db2-descr\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!databases con)
                              (list s r)))))

   (test "databases failure"
         `((output . "SHOW DB\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!databases con)
                              (list s r)))))

   (test "strategies success"
         '((output . "SHOW STRAT\r\n")
           (result #t (("s1" "s1-descr") ("s2" "s2-descr"))))
         (with-stubbed-io "111 2 databases present\r\ns1 s1-descr\r\ns2 s2-descr\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!strategies con)
                              (list s r)))))

    (test "strategies failure"
          `((output . "SHOW STRAT\r\n")
            (result #f ,(test-make-status-response 500 "Syntax error")))
          (with-stubbed-io "500 Syntax error\r\n"
                           (lambda (con)
                             (receive (s r) (!strategies con)
                               (list s r)))))

   (test "server-info success"
         '((output . "SHOW SERVER\r\n")
           (result #t "server information\n"))
         (with-stubbed-io "114 server information\r\nserver information\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!server-information con)
                              (list s r)))))
         
   (test "server-info failure"
         `((output . "SHOW SERVER\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!server-information con)
                              (list s r)))))
   (test "db-info success"
         '((output . "SHOW INFO db1\r\n")
           (result #t "db info\n"))
         (with-stubbed-io "112 information for db1\r\ndb info\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!database-information con "db1")
                              (list s r)))))
                          
   (test "db-info failure"
         `((output . "SHOW INFO db1\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!database-information con "db1")
                              (list s r)))))
   (test "help success"
         '((output . "HELP\r\n")
           (result #t "helptext comes here\n"))
         (with-stubbed-io "113 help text follows\r\nhelptext comes here\r\n.\r\n250 ok\r\n"
                          (lambda (con)
                            (receive (s r) (!help con)
                              (list s r)))))
   (test "help failure"
         `((output . "HELP\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!help con)
                              (list s r)))))
   (test "status success"
         `((output . "STATUS\r\n")
           (result #t ,(test-make-status-response 210 "status [d/m/c = 0/0/6; 325.000r 0.000u 0.000s]")))
         (with-stubbed-io "210 status [d/m/c = 0/0/6; 325.000r 0.000u 0.000s]\r\n"
                          (lambda (con)
                            (receive (s r) (!status con)
                              (list s r)))))
   (test "status failure"
         `((output . "STATUS\r\n")
           (result #f ,(test-make-status-response 500 "Syntax error")))
         (with-stubbed-io "500 Syntax error\r\n"
                          (lambda (con)
                            (receive (s r) (!status con)
                              (list s r)))))
   ;;enable this once md5 has been ported to chicken 4
    (define test-password (md5-digest (string-append "<test@example.org>" "test")))

    (test "authenticate success"
          `((output . ,(conc "AUTH test " test-password "\r\n"))
            (result . #t))
          (with-stubbed-io "230 authenticated\r\n"
                           (lambda (con)
                             (!authenticate con "test" "test"))))
    (test "authenticate failure" #t #t)
                   
(test-end "commands")
(test-end "dict-unit")


;;(test-begin "dict-system")

;;(test-end "dict-system")


(unless (zero? (test-failure-count)) (exit 1))
