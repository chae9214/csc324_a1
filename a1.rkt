#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

; (helper) search1 is used in split-body
(define (search1 item lst)
  (let search-ind ([lst lst]
                   [ind 0])
    (cond [(empty? lst) #f]
          [(equal? item (first lst)) ind]
          [else (search-ind (rest lst) (+ 1 ind))])))

; (helper) search2 is used in check-expr (one level deeper than search1)
(define (search2 item lst)
  (let search-ind ([lst lst]
                   [ind 0])
    (cond [(empty? lst) #f]
          [(equal? item (first (first lst))) ind]
          [else (search-ind (rest lst) (+ 1 ind))])))

; (helper) split-body is used in evaluate
(define (split-body body)
  (define i (search1 finis body))
  (if (equal? i #f)
        (cons body '())
        (cons (take body i) (split-body (drop body (+ i 1))))))

; (helper) sublist is used in make-splitter
(define (sublist sub lst)
  (let sublist-ind ((lst lst)
                    (ind 0))
    (cond [(empty? lst) #f]
          [(> (length sub) (length lst)) #f]
          [(equal? (take lst (length sub)) sub) ind]
          [else (sublist-ind (rest lst) (+ 1 ind))])))

; (helper) make-splitter is used in interpret-expr
(define (make-splitter splitter)
  (lambda (lst)
    (letrec ([sub (string-split splitter)]
             [i (sublist sub lst)])
      (if (equal? i #f) #f (list (take lst i) (drop lst (+ i (length sub))))))))

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#
(define (evaluate body)
  (letrec ([body-lst (split-body body)]
           [personae-lst (rest (first body-lst))]
           [settings-lst '()]
           [dialogue-lst (last body-lst)])
    (if (equal? (length body-lst) 3)
        (set! settings-lst (rest (second body-lst))) (void))
    (define plst (interpret-personae-lst personae-lst))
    (define flst (interpret-settings-lst settings-lst))
    (define dlst (interpret-dialogue-lst dialogue-lst plst flst))
    (map (lambda (item) (last item)) dlst)))

(define (interpret-personae-lst lst)
  (if (empty? lst)
      '()
      (let ([s (string-split (first lst))])
        (cons (list (string-trim (first s) ",") (interpret-desc (rest s)))
              (interpret-personae-lst (rest lst))))))

(define (interpret-settings-lst lst)
  (if (empty? lst)
      '()
      (let ([s (string-split (first lst))])
        (cons (list (string-trim (first s) ",") (interpret-func (rest s)))
              (interpret-settings-lst (rest lst))))))

(define (interpret-dialogue-lst lst plst flst)
  (if (empty? lst)
      '()
      (let ([name (string-trim (first lst) ":")])
        (cons (list name (interpret-expr (string-split (second lst)) name plst flst))
              (interpret-dialogue-lst (drop lst 2) plst flst)))))

; Changed to take just the description for repeated use in multiple functions
(define (interpret-desc desc)
  (letrec ([num-word (length desc)]
           [num-bad
            (length (filter (lambda (x) (not (equal? (member x bad-words) #f))) desc))]
           [value (* (* (expt 2 num-bad) num-word) -1)])
    (if (equal? num-bad 0) num-word value)))

(define (interpret-func expr)
  (if (and (>= (length expr) 3) (equal? (string-join (take expr 3)) call))
      (list (list-ref expr 3) (interpret-func-expr (drop expr 5)))
      (interpret-func-expr expr)))

(define (interpret-func-expr expr)
  (let ([split-add (make-splitter add)]
        [split-mult (make-splitter mult)])
    (if (equal? (length expr) 1)
        (if (equal? (first expr) param)
            (lambda (x) x)
            (lambda (x) (interpret-desc expr)))
        (cond [(not (equal? (split-add expr) #f)) (help-split expr split-add +)]
              [(not (equal? (split-mult expr) #f)) (help-split expr split-mult *)]
              [else (lambda (x) (interpret-desc expr))]))))

(define (help-split expr split-f f)
  (let ([e1 (first (split-f expr))]
        [e2 (last (split-f expr))])
    (cond [(and (equal? (length e1) 1) (equal? (first e1) param))
           (if (and (equal? (length e2) 1) (equal? (first e2) param))
               (lambda (x) (f x x))
               (lambda (x) (f x (interpret-desc e2))))]
          [(and (equal? (length e2) 1) (equal? (first e2) param))
           (lambda (x) (f x (interpret-desc e1)))]
          [else (lambda (x) (f (interpret-desc e1) (interpret-desc e2)))])))

(define (interpret-expr expr name plst flst)
  (if (and (>= (length expr) 3) (equal? (string-join (take expr 3)) call))
      (call-func (list-ref expr 3) (interpret-expr-expr (drop expr 5) name plst) flst)
      (interpret-expr-expr expr name plst)))

(define (call-func fname arg flst)
  (let ([func (last (list-ref flst (search2 fname flst)))])
    (if (pair? func)
        (call-func (first func) ((last func) arg) flst)
        (func arg))))

; Uses check-expr which uses interpret-desc
(define (interpret-expr-expr expr name plst)
  (let ([split-add (make-splitter add)]
        [split-mult (make-splitter mult)])
    (cond [(not (equal? (split-add expr) #f))
           (+ (check-expr (first (split-add expr)) name plst)
              (check-expr (last (split-add expr)) name plst))]
          [(not (equal? (split-mult expr) #f))
           (* (check-expr (first (split-mult expr)) name plst)
              (check-expr (last (split-mult expr)) name plst))]
          [else (check-expr expr name plst)])))

; Checks for self-reference, name-lookup or neither in an expression
(define (check-expr expr name plst)
  (if (equal? (length expr) 1)
      (cond [(not (equal? (member (first expr) self-refs) #f))
             (last (list-ref plst (search2 name plst)))]
            [(not (equal? (member (first expr) (flatten plst)) #f))
             (last (list-ref plst (search2 (first expr) plst)))]
            [else (interpret-desc expr)])
      (interpret-desc expr)))
