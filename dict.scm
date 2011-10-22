;; Author: David Krentzlin <david@lisp-unleashed.de>
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


(module dict
  (status-response? response-status-code response-status-message response-status-code->string
   response-status-error? status:n-databases-present? status:n-strategies-present?
    status:database-information-follows? status:help-text-follows? status:server-information-follows?
    status:challenge-follows? status:n-definitions-retrieved? status:word-database-name?
    status:n-matches-retrieved? status:statistic? status:banner? status:closing-connection?
    status:authentication-successful? status:ok? status:send-response? status:server-temporarily-unavailable?
    status:shutdown-at-op-request? status:syntax-error-command? status:syntax-error-parameter?
    status:command-not-implemented? status:parameter-not-implemented? status:access-denied?
    status:access-denied-show-info? status:access-denied-unknown-mech? status:invalid-database?
    definition-word definition-db definition-db-descr definition-text

    status:invalid-strategy? status:no-match? status:no-database-present? status:no-strategies-present?
    make-connection connect disconnect connection-msg-id connection-server-capabilities 
    *current-log-port* !match !define !databases !strategies !server-information !database-information
    !help !status !quit !announce-client !authenticate)
  
  (import scheme chicken)
  (require-library tcp defstruct srfi-13 srfi-14 md5)

  (cond-expand
   (irregex-is-core-unit
    (use irregex))
   (else
    (require-library regex)
    (import (rename irregex
                    (irregex-match-start irregex-match-start-index)
                    (irregex-match-end irregex-match-end-index)))
    (define irregex-num-submatches irregex-submatches)
    (define irregex-match-num-submatches irregex-submatches)
    (define (irregex-match-valid-index? m i)
      (and (irregex-match-start-index m i) #t))

    (define (irregex-split irx str . o)
      (if (not (string? str)) (error "irregex-split: not a string" str))
      (let ((start (if (pair? o) (car o) 0))
            (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
        (irregex-fold
         irx
         (lambda (i m a)
           (if (= i (irregex-match-start-index m 0))
               a
               (cons (substring str i (irregex-match-start-index m 0)) a)))
         '()
         str
         (lambda (i a)
           (reverse (if (= i end) a (cons (substring str i end) a))))
         start
         end)))

    (define (maybe-string->sre obj)
      (if (string? obj) (string->sre obj) obj))))
  
  (import tcp defstruct
          (only md5 md5-digest) 
          (only srfi-14 char-set:digit)
          (only srfi-13 string-join string-index string-trim-right string-trim-both string-trim string-skip string-take string-drop)
;          (only regex string-split-fields)
          (only extras fprintf read-line)
          (only data-structures alist-ref alist-update!))
          

  ;;add support for parsing mime-headers
  ;;when option mime is set

  (define (make-exn-condition loc msg . args)
    (apply make-property-condition
           'exn
           (append (if loc (list 'location loc)  '())
                   (if msg (list 'message msg) '())
                   (if (not (null? args)) (list 'arguments args) '()))))

  (define (complain msg . args)
    (signal (apply make-exn-condition #f msg args)))

  ;;there are two kinds of answers we can expect from the
  ;;dict-server.
  ;;1) a status response which consists of a numeric status-code
  ;;   optionally followed by textual information
  ;;   The status-code may of may not have leading zeros
  ;;2) data which has different formats depending on the request

  ;;so a status response may be naturally represented using a pair.
  (define (make-status-response code msg)
    (unless (and (integer? code) (positive? code))
      (complain "First argument to make-status-response must be a positive integer"))
    (unless (string? msg)
      (complain "Second argument to make-status-response must be a non-empty string"))
    (cons code msg))
  
  (define (status-response? arg)
    (and (pair? arg) (number? (car arg)) (string? (cdr arg))))

  ;;accessors for convenience
  (define response-status-code car)
  (define response-status-message cdr)


  ;;map status-codes to textual representation
  (define +status-texts+ (list))
  (define (add-status-text status)
    (set! +status-texts+ (cons status +status-texts+)))

  (define (response-status-code->string  code)
    (or (alist-ref code +status-texts+) ""))

  (define (response-status-error? resp)
    (and (status-response? resp) (>= (response-status-code resp) 400)))


  ;;add the status to the mapping
  ;;create a predicate to test if a status-object is of a given type
  ;;define a constant to that status
  (define-syntax define-status
    (lambda (exp rename compare)
      (let* ((status-name (cadr exp))
             (code (caddr exp))
             (string (cadddr exp))
             (predicate (string->symbol (string-append (symbol->string status-name) "?")))
             (%begin (rename 'begin))
             (%define (rename 'define))
             (%and (rename 'and))
             (%= (rename '=))
             (%cons (rename 'cons))
             (%status-resp? (rename 'status-response?))
             (%status-code (rename 'response-status-code))
             (%add-status-text (rename 'add-status-text)))
        `(,%begin
          (,%define ,status-name (,%cons ,code ,string))
          (,%define (,predicate arg)
                    (,%and (,%status-resp? arg) (,%= (,%status-code arg) ,code)))
          (,%add-status-text ,status-name)))))
  
  (define-syntax define-cmd
    (lambda (exp rename compare)
      (let* ((cmd (cadr exp))
             (%define (rename 'define))
             (cmd-symbol (string->symbol (conc "+cmd:" (string-translate* (string-downcase cmd) '((" " . "-"))) "+"))))
        `(,%define ,cmd-symbol ,(string-upcase cmd)))))

  ;;1yz repsonse-codes
  (define-status status:n-databases-present 110 "n databases present - text follows")
  (define-status status:n-strategies-present 111 "n strategies available - text follows")
  (define-status status:database-information-follows 112 "database information follows")
  (define-status status:help-text-follows 113 "help text follows")
  (define-status status:server-information-follows 114 "server information follows")
  (define-status status:challenge-follows 130 "challenge follows")
  (define-status status:n-definitions-retrieved 150 "n definitions retrieved - definitions follow")
  (define-status status:word-database-name 151 "word database name - text follows")
  (define-status status:n-matches-retrieved 152 "n matches found - text follows")

  ;;2yz
  (define-status status:statistic 210 "statistic")
  (define-status status:banner 220 "banner")
  (define-status status:closing-connection 221 "Closing Connection")
  (define-status status:authentication-successful 230 "Authentication successful")
  (define-status status:ok 250 "Ok")

  ;;3yz
  (define-status status:send-response 330 "send response")

  ;;4yz
  (define-status status:server-temporarily-unavailable 420 "Server temporarily unavailable")
  (define-status status:shutdown-at-op-request 421 "Server shutting down at operator request")

  ;;5yz
  (define-status status:syntax-error-command 500 "Syntax error, command not recognized")
  (define-status status:syntax-error-parameter 501 "Syntax error, illegal parameters")
  (define-status status:command-not-implemented 502 "Command not implemented")
  (define-status status:parameter-not-implemented 503 "Command parameter not implemented")
  (define-status status:access-denied 530 "Access denied")
  (define-status status:access-denied-show-info 531 "Access denied, use \"SHOW INFO\" for server information")
  (define-status status:access-denied-unknown-mech 532 "Access denied, unknown mechanism")
  (define-status status:invalid-database 550 "Invalid database, use \"SHOW DB\" for list of databases")
  (define-status status:invalid-strategy 551 "Invalid strategy, use \"SHOW STRAT\" for a list of strategies")
  (define-status status:no-match 552 "No match")
  (define-status status:no-database-present 554 "No databases present")
  (define-status status:no-strategies-present 555 "No strategies available")


  (define *default-port* (make-parameter 2628))
  (define +crlf+ "\r\n")

 
  (define-cmd "quit")
  (define-cmd "client")
  (define-cmd "define")
  (define-cmd "match")
  (define-cmd "show strat")
  (define-cmd "show db")
  (define-cmd "show info")
  (define-cmd "show server")
  (define-cmd "status")
  (define-cmd "help")
  (define-cmd "auth")
  (define-cmd "option mime")

  ;;banner-object
  (define (make-banner text msg-id cap)
    (unless (list? cap)
      (complain "capabilities must be a list"))
    (unless (and (string? text) (string? msg-id))
      (complain "text and msg-id must be strings"))
    (list text msg-id cap))

  (define banner-msg-id cadr)
  (define banner-text car)
  (define banner-capabilities caddr)


  ;;logging support
  ;;every command and response will be logged
  ;;to *current-log-port* if logging is enabled
  (define *current-log-port* (make-parameter #f))


  (define (dict-log  fmt . args)
    (if (*current-log-port*)
        (apply fprintf (*current-log-port*) fmt args)
        #f))


  ;;input routines and parsing

  ;;readline with logging
  ;;and optionally erroring when eof is received
  (define (dict-read-line port #!optional (eof-is-error? #f))
    (let ((line (read-line port)))
      (when (and (eof-object? line) eof-is-error?)
        (complain "unexpected eof received"))
      (dict-log "<<: ~A~%" line)
      line))


  ;;parse a status response
  ;;the rfc states that a legal status response
  ;;is an integer representing the status-code (optionally with leading zeros)
  ;;which is optionally followed by textual information
  (define (parse-status-response input)
    (let ((eoc (string-skip input char-set:digit)))
      (cond
       ((not eoc)   (make-status-response (string->number (string-trim-both (string-trim input #\0) #\space)) ""))
       ((zero? eoc) (complain "Malformed status response " input))
       (else        (make-status-response (string->number (string-trim (string-take input eoc) #\0)) (string-drop input (+ 1 eoc)))))))


  (define (read-status-response port)
    (parse-status-response (string-trim (dict-read-line port) #\space)))

  ;;parse the banner status-response
  ;;the textual information of the banner-status
  ;;provides addtional information about the server
  ;;we're interested in the message-id and the list of capabilities
  (define (parse-banner input)
    (let ((parts (reverse (irregex-split "\\s+" input))))
      (when (< (length parts) 2)
        (complain "Malformed banner received"))
      (let ((msg-id (car parts))
            (cap (cadr parts)))
        (if (and (positive? (string-length cap)) (char=? (string-ref cap 0) #\<))
            (make-banner  (string-join (reverse (cddr parts)) " ")
                          msg-id
                          (irregex-split "\\." (string-trim-right (string-trim cap #\<) #\>)))
            (make-banner (string-join (reverse (cdr parts)) " ")
                         msg-id
                         '())))))

  ;;input: the textual information of status 151
  (define (parse-status-151 input)
    (let ((ws (string-index input #\space)))
      (unless ws (complain "Malformed status 151 response. Expected `word`" input))
 
      (let ((word (string-trim-both (string-take input ws) #\"))
            (ws2 (string-index input #\space (+ ws 1))))
        
        (unless ws2 (complain "Malformed status 151 response. Expected `databasename`"  input))
   
        (let ((db (string-trim-both (string-take (string-drop input (+ ws 1)) (- ws2 (+ 1 ws))) #\"))
              (descr (string-trim-both (string-trim-both (string-drop input (+ 1 ws2)) #\space) #\")))
          (list word db descr)))))

  ;;fold input until we reach .\r\n 250 ok\r\n
  (define (fold-input port knil kons)
    (let loop ((expect-status? #f) (knil knil))
      (if expect-status?
          (let ((status (read-status-response port)))
            (if (status:ok? status) knil (complain "Unexpected status response. Expected 250 ok" status)))
          (let ((line (dict-read-line port #t)))
            (if (string=? (string-trim-both line #\space) ".")
                (loop #t knil)
                (loop #f (kons knil line)))))))

  ;;simply consume all text
  (define (read-text port)
    (fold-input port "" (cut string-append <> <> "\n")))


  ;;consume all input building pairs out of each line
  ;;used for example for strategy-lists
  (define (line->pair line)
    (let ((ws (string-index line #\space)))
      (list (string-trim-both (string-take line ws) #\space) (string-trim-both (string-trim-both (string-drop line (+ ws 1)) #\space) #\"))))

  (define (read-pairs port)
    (reverse (fold-input port '() (lambda (lines line) (cons (line->pair line) lines)))))


  (define (read-matches port)
    (fold-input port '() (lambda (matches line)
                           (let* ((match (line->pair line)))
                             (alist-update! (car match)
                                            (cons (cadr match) (alist-ref (car match) matches string-ci=? '()))
                                            matches string-ci=?)))))

  (define definition-word car)
  (define definition-db cadr)
  (define definition-db-descr caddr)
  (define definition-text cadddr)

  (define (read-definitions port)
    (let loop ((expect-status? #t) (wdn '()) (text "") (defs '()))
      (if expect-status?
          (let ((status (read-status-response port)))
            (cond
             ((status:ok? status) (reverse defs))
             ((status:word-database-name? status)
              (loop #f (parse-status-151 (response-status-message status)) text defs))
             (else (complain "unexpected status response" status))))
          (let ((line (dict-read-line port #t)))
            (if (string=? (string-trim-both line #\space) ".")
                (loop #t '() "" (cons (append wdn (list text)) defs))
                (loop #f wdn (string-append text line "\n") defs))))))
  
  ;;the connection-object
  (defstruct connection input-port output-port connected msg-id text server-capabilities)


  (define (translate-database db)
    (cond
     ((symbol? db)
      (case db
        ((first) "!")
        ((all) "*")
        (else (complain "Invalid db-placeholder. Must be either of 'all, 'first"))))
     ((string? db) db)
     (else (complain "unsopported type for database. Must be either symbol or string" db))))

  (define (translate-strategy strat)
    (cond
     ((and (symbol? strat) (eq? strat 'default)) ".")
     ((string? strat) strat)
     (else
      (complain "invalid type for strategy given. Must be either 'default or a string naming the strategy" strat))))


  (define (send-command con cmd . args)
    (unless (connection? con)
      (complain "supplied arguments is not a valid connection"))
    (unless (connection-connected con)
      (complain "not connected"))
    
    (let ((port (connection-output-port con)))
      (dict-log ">>: ")
      (dict-log cmd)
      (display cmd port)
      (for-each (lambda (param)
                  (dict-log " ")
                  (dict-log param port)
                  (display " " port)
                  (display param port))
                args)
      (dict-log "\n")
      (display +crlf+ port)
      (flush-output port)))

  (define (issue-command success? proc con cmd . args)
    (apply send-command con cmd args)
    (let ((resp (read-status-response (connection-input-port con))))
      (if (success? resp)
          (values #t (proc (connection-input-port con)))
          (values #f resp))))
  
  ;;commands follow
  (define (!match con word #!key (strategy 'default) (db 'first))
    (let ((strat (translate-strategy strategy))
          (db (translate-database db)))
      (issue-command status:n-matches-retrieved? read-matches con +cmd:match+ db strat word)))

  (define (!define con word #!key (db 'first))
    (let ((db (translate-database db)))
      (issue-command status:n-definitions-retrieved? read-definitions con +cmd:define+ db word)))

  (define (!databases con)
    (issue-command status:n-databases-present? read-pairs con +cmd:show-db+))
  

  (define (!strategies con)
    (issue-command status:n-strategies-present? read-pairs con +cmd:show-strat+))


  (define (!server-information con)
    (issue-command status:server-information-follows? read-text con +cmd:show-server+))


  (define (!database-information con db)
    (issue-command status:database-information-follows? read-text con +cmd:show-info+ db))

  (define (!help con)
    (issue-command status:help-text-follows? read-text con +cmd:help+))

  (define (!status con)
    (send-command con +cmd:status+)
    (let ((status (read-status-response (connection-input-port con))))
      (values (status:statistic? status) status)))

  (define (!quit con)
    (send-command con +cmd:quit+)
    (let ((status (read-status-response (connection-input-port con))))
      (values (status:closing-connection? status) status)))

  (define (!announce-client con client)
    (send-command con +cmd:client+ client)
    (let ((status (read-status-response (connection-input-port con))))
      (values (status:ok? status) status)))

  
   (define (!authenticate con username password)
     (send-command con +cmd:auth+ username (compute-password con password))
     (let ((status (read-status-response (connection-input-port con))))
       (status:authentication-successful? status)))

   (define (compute-password con password)
     (md5-digest (string-append (connection-msg-id con) password)))


  (define (connect server #!key (port (*default-port*)) (client "dict.egg for chicken scheme") (timeout #f))
    (parameterize ((tcp-connect-timeout timeout))
      (receive (i o) (tcp-connect server port)
        (let ((status (read-status-response i)))
          (cond
           ((status:banner? status)
            (let* ((banner (parse-banner (response-status-message status)))
                   (con (make-connection input-port: i
                                         output-port: o
                                         connected: #t
                                         msg-id: (banner-msg-id banner)
                                         text: (banner-text banner)
                                         server-capabilities: (banner-capabilities banner))))
              (!announce-client con client)
              con))
           (else
            (complain "Could not connect to server" status)))))))

  (define (disconnect con)
    (when (connection-connected con)
      (receive (success? result) (!quit con)
        (when success?
          (close-input-port (connection-input-port con))
          (close-output-port (connection-output-port con))
          (connection-connected-set! con #f))))
      (not (connection-connected con)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dict.scm ends here
