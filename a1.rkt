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

#|
(search1 item lst)
  item: a string to search for.
  lst: a list of strings.

  Returns the index of the 'item' in the list 'lst'.
  If no match is found, returns #f.

> (search1 "two" '("one" "two" "three"))
1
> (search1 "four" '("one" "two" "three"))
#f
|#
(define (search1 item lst)
  (let search-ind ([lst lst]
                   [ind 0])
    (cond [(empty? lst) #f]
          [(equal? item (first lst)) ind]
          [else (search-ind (rest lst) (+ 1 ind))])))

#|
(search2 item lst)
  item: a string to search for.
  lst: a list of pairs in form (str content).

  Returns the index of the pair that contains 'item' at index 0
  in the list 'lst'. If no match is found, returns #f.

> (search2 "two" '(("one" 1) ("two" 2) ("three" 3)))
1
> (search2 "four" '(("one" 1) ("two" 2) ("three" 3)))
#f
|#
(define (search2 item lst)
  (let search-ind ([lst lst]
                   [ind 0])
    (cond [(empty? lst) #f]
          [(equal? item (first (first lst))) ind]
          [else (search-ind (rest lst) (+ 1 ind))])))

#|
(split-body body)
  body: a list of lines corresponding to the semantically meaningful text
        of a FunShake file.

  Returns a list containing three seperate parts of the 'body',
  respectively the personae, settings (optional), and dialogue parts.

> (define body (list "Dramatis personae"
                     "David, a character"
                     "Finis"
                     "Settings"
                     "Verona, a place"
                     "Finis"
                     "David:"
                     "Hello world"))
> (split-body body)
'(("Dramatis personae" "David, a character")
  ("Settings" "Verona, a place")
  ("David:" "Hello world"))
|#
(define (split-body body)
  (define i (search1 finis body))
  (if (equal? i #f)
        (cons body '())
        (cons (take body i) (split-body (drop body (+ i 1))))))

#|
(sublist sub lst)
  sub: a list
  lst: a list

  Checks whether 'sub' is a sublist of 'lst' (i.e., all the items in
  'sub' appear consecutively in 'lst'). If 'sub' is a sublist of 'lst',
  this function returns the *index* of the first element of the first
  occurrence of 'sub' within 'lst'. Otherwise, this function returns #f.

> (sublist '(30 40) '(10 20 30 40 50))
2
> (sublist '(20 30) '(10 20 30 20 30 40 50))
1
> (sublist '(1 2 3) '(5 4 3 2 1))
#f
|#
(define (sublist sub lst)
  (let sublist-ind ((lst lst)
                    (ind 0))
    (cond [(empty? lst) #f]
          [(> (length sub) (length lst)) #f]
          [(equal? (take lst (length sub)) sub) ind]
          [else (sublist-ind (rest lst) (+ 1 ind))])))

#|
(make-splitter splitter)
  splitter: a string

  Returns a function f that takes a list of strings lst and implements
  the following behaviour:
    1. If 'splitter' does not occur in the list lst, return #f.
    2. Else, return a list of two elements (list before after), where
       'before' is the list of words in 's' *before* the first occurrence
       of 'splitter', and 'after' is the list of words in 's' *after* the
       first occurrence of 'splitter'.

> (define f (make-splitter "hello world"))
> (f '("this" "is" "a" "hello" "world" "kind" "of" "party"))
'(("this" "is" "a") ("kind" "of" "party"))
> (f '("this" "is" "a" "hello" "not" "world" "kind" "of" "party"))
#f
|#
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

#|
(interpret-personae-lst lst)
  lst: a list of lines corresponding to personae part of a FunShake file.

  Returns a list of pairs of identifier names and their value in form
  ("name" value).
  ** calls interpret-desc

> (interpret-personae-lst (list "David, a man of four"
                                "MacGries, a vile merchant"))
'(("David" 4) ("MacGries" -6))
|#
(define (interpret-personae-lst lst)
  (if (empty? lst)
      '()
      (let ([s (string-split (first lst))])
        (cons (list (string-trim (first s) ",") (interpret-desc (rest s)))
              (interpret-personae-lst (rest lst))))))

#|
(interpret-settings-lst lst)
  lst: a list of lines corresponding to settings part of a FunShake file.

  Returns a list of pairs of function names and their contents in form
  ("fname" (lambda (x) ...)). If the body of function is another function
  call, it results in nested pair ("fname" ("fname2" (lambda (x) ...))).
  ** calls interpret-func, which calls interpret-func-expr

> (interpret-settings-lst (list "Verona, one join'd with Hamlet."
                                "Scotland, The song of Verona and Hamlet"))
'(("Verona" #<procedure:...)
  ("Scotland" ("Verona" #<procedure:...)))
|#
(define (interpret-settings-lst lst)
  (if (empty? lst)
      '()
      (let ([s (string-split (first lst))])
        (cons (list (string-trim (first s) ",") (interpret-func (rest s)))
              (interpret-settings-lst (rest lst))))))

#|
(interpret-dialogue-lst lst)
  lst: a list of lines corresponding to dialogue part of a FunShake file.

  Returns a list of pairs of speaker names and the evaluation of their
  dialogue lines in form ("name" value).
  ** calls interpret-expr, which calls interpret-expr-expr and call-func

> (interpret-dialogue-lst (list "David:"
                                "The song of Verona and Myself")
                          (list '("David" 3))
                          (list (cons "Verona" (cons (lambda (x) (+ x 1)) '()))))
'(("David" 4))
|#
(define (interpret-dialogue-lst lst plst flst)
  (if (empty? lst)
      '()
      (let ([name (string-trim (first lst) ":")])
        (cons (list name (interpret-expr (string-split (second lst)) name plst flst))
              (interpret-dialogue-lst (drop lst 2) plst flst)))))

#|
(interpret-desc desc)
  desc: a list of words in a description.

  Returns an integer value the description evaluates to,
  by counting the number of words n and number of bad words b
  and returning (-1) * (2 ^ b) * n. If b = 0, returns just n.

> (interpret-desc '("a" "scoundrelous" "and" "vile" "merchant"))
-20
> (interpret-desc '("a" "man" "of" "four"))
4
|#
(define (interpret-desc desc)
  (letrec ([num-word (length desc)]
           [num-bad
            (length (filter (lambda (x) (not (equal? (member x bad-words) #f))) desc))]
           [value (* (* (expt 2 num-bad) num-word) -1)])
    (if (equal? num-bad 0) num-word value)))

#|
(interpret-func expr)
  expr: a list of words in a function definition expression.

  Returns a lambda function the function definition evaluates to.
  Detects possible function call in the body, and recursively evaluates
  the rest of the definition for use with the function call.

> (interpret-func '("chocolate" "cookie" "join'd" "with" "Hamlet"))
#<procedure:...>
> (interpret-func '("The" "song" "of" "Verona" "and" "Hamlet"))
'("Verona" #<procedure:...>)
> ((interpret-func '("chocolate" "cookie" "join'd" "with" "Hamlet")) 3)
5
|#
(define (interpret-func expr)
  (if (and (>= (length expr) 3) (equal? (string-join (take expr 3)) call))
      (list (list-ref expr 3) (interpret-func-expr (drop expr 5)))
      (interpret-func-expr expr)))

#|
(interpret-func-expr expr)
  expr: a list of words in a function definition expression,
        with possible function call trimmed.

  Returns a lambda function the expression evaluates to.

> ((interpret-func-expr '("Hamlet" "entranc'd" "by" "one" "two")) 3)
6
> ((interpret-func-expr '("a" "vile" "man" "join'd" "with" "Hamlet")) 3)
-3
> ((interpret-func-expr '("Hamlet")) 3)
3
|#
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

#|
(help-split expr split-f f)
  expr: a list of words in a function definition expression,
        that contains either addition or multiplication denoter.
  split-f: a function that splits a list of strings into two,
           before and after a certain string occurrence.
  f: a function that should be operated depending on the denoter.

  Returns a lambda function the expression evaluates to,
  by splitting the expression by either addition of multiplication
  denoter and evaluating each end.
  ** helps interpret-func-expr

> (define split-add (make-splitter add))
> (define split-mult (make-splitter mult))
> ((help-split '("Hamlet" "entranc'd" "by" "one" "two") split-mult *) 3)
6
> ((help-split '("a" "vile" "man" "join'd" "with" "Hamlet") split-add +) 3)
-3
|#
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

#|
(interpret-expr expr name plst flst)
  expr: a list of words in a dialogue expression.
  name: the name of the speaker, as a string.
  plst: a list of identifier names and their value pairs.
  flst: a list of function names and their contents pairs.

  Returns an interger value the dialogue evaluates to.
  Detects possible function call, and calls function with the rest of
  the expression as an argument. Otherwise just evaluates expression.

> (interpret-expr '("The" "song" "of" "Verona" "and" "Myself")
                  "David"
                  (list '("David" 3) '("MacGries" 10))
                  (list (cons "Verona" (cons (lambda (x) (+ x 1)) '()))))
4
> (interpret-expr '("MacGries" "entranc'd" "by" "Myself")
                  "David"
                  (list '("David" 3) '("MacGries" 10))
                  (list (cons "Verona" (cons (lambda (x) (+ x 1)) '()))))
30
|#
(define (interpret-expr expr name plst flst)
  (if (and (>= (length expr) 3) (equal? (string-join (take expr 3)) call))
      (call-func (list-ref expr 3) (interpret-expr-expr (drop expr 5) name plst) flst)
      (interpret-expr-expr expr name plst)))

#|
(call-func fname arg flst)
  fname: the name of the function to be called, as a string.
  arg: an integer value to be called on the function.
  flst: a list of function names and their contents pairs.

  Recursively calls the lambda function equivalent of the pair
  from 'flst' with function name 'fname' on 'arg'.

> (define flst (interpret-settings-lst
                (list "Verona, one join'd with Hamlet."
                      "Scotland, The song of Verona and Hamlet")))
> (call-func "Verona" 3 flst)
4
> (call-func "Scotland" 3 flst)
4
|#
(define (call-func fname arg flst)
  (let ([func (last (list-ref flst (search2 fname flst)))])
    (if (pair? func)
        (call-func (first func) ((last func) arg) flst)
        (func arg))))

#|
(interpret-expr-expr expr name plst)
  expr: a list of words in a dialogue expression,
        with possible function call trimmed.
  name: the name of the speaker, as a string.
  plst: a list of identifier names and their value pairs.

  Returns an interger value the expression evaluates to.
  Detects any addtion or multiplication denoter and evaluates
  each end (or the whole expression in case of none).

> (interpret-expr-expr '("MacGries" "entranc'd" "by" "Myself")
                       "David"
                       (list '("David" 3) '("MacGries" 10)))
30
|#
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

#|
(check-expr expr name plst)
  expr: a list of words in a dialogue expression,
        with possibile addition or multiplication denoter trimmed.
  name: the name of the speaker, as a string.
  plst: a list of identifier names and their value pairs.

  Returns an integer value the expression evaluates to,
  by checking for either self-reference or name-lookup and returning
  the value equivalent of the pair from 'plst' with identifier name 'name'.
  In case of none of both, just evaluates the expression normally.
  ** helps interpret-expr-expr

> (check-expr '("Myself") "David" (list '("David" 3) '("MacGries" 10)))
3
> (check-expr '("MacGries") "David" (list '("David" 3) '("MacGries" 10)))
10
> (check-expr '("a" "vile" "man") "David" (list '("David" 3) '("MacGries" 10)))
-6
|#
(define (check-expr expr name plst)
  (if (equal? (length expr) 1)
      (cond [(not (equal? (member (first expr) self-refs) #f))
             (last (list-ref plst (search2 name plst)))]
            [(not (equal? (member (first expr) (flatten plst)) #f))
             (last (list-ref plst (search2 (first expr) plst)))]
            [else (interpret-desc expr)])
      (interpret-desc expr)))
