;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Sudoku-Solver) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This is a Sudoku-solver written in Racket.
;; All the functions are introduced in CS135 offer by UW, with the
;; exception of the following list:
;; 1.
;; (take lst pos) returns a fresh list whose elements are the first pos
;;   elements of lst.
;; take: (listof Any) Int -> (listof Any)
;; Requires: pos is non-negative
;; 2.
;; (drop lst pos) returns the list after the first pos elements of lst.
;; take: (listof Any) Int -> (listof Any)
;; Requires: pos is non-negative
;; 3.
;; (list-ref lst pos) Returns the element of lst at position pos, where
;;   the list’s first element is position 0.
;; list-ref: (listof Any) Int -> Any
;; Requires: pos is non-negative
;; 4.
;; (andmap pred lst) produces true if every element in lst applied to
;;   pred produces true, and false otherwise
;; andmap: (X -> Bool) (listof X) -> Bool


;; All above functions can be written as functions after taking CS135,
;; but for simplicity built-in funtions are used.
;; These functions are included by writing:
(require racket/list)


;; This project was designed by Prof. Gregor Kiczales, who explained in
;; detail on the thought process of making the sudoku-solver in Racket,
;; in his YouTube channel Systematic Program Design: 
;; https://www.youtube.com/channel/UC7dEjIUwSxSNcW4PqNRQW8w


;; The following code follows the design recipe outlined by CS135.
;; Additional documentations were made to clearly explain the concepts.
;; Thank you for reading. I hope you enjoy this project.
;; Yangming (David) Hu
;; Jan 2021




;; Data Definitions

;; Val is a Nat in the range of [1, 9].
;; Board is a (listof (anyof Val false)) that is 81 elements long.
;; Pos is a Nat in the range of [0, 80].
;; Unit is (listof Pos) of length 9


;; Constants:
(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ; where B stands for blank
;; This is simply for writing out boards easier. 

(define BD1
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))


(define BD2
  (list 1 2 3 4 5 6 7 8 9
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4 ;; an easy one
  (list 2 6 3 B 7 1 B B 5
        B B B B B B 9 B B
        9 1 4 3 B B B B 7
        B 7 B B B B B B B
        5 B B B B 4 B B B
        3 B 1 B B B B 9 B
        4 B B B 6 B 8 5 B
        B B B 8 4 B B B 1
        B 5 B B B 7 B 6 B)) 
 
(define BD4s
  (list 2 6 3 9 7 1 4 8 5
        7 8 5 4 2 6 9 1 3
        9 1 4 3 5 8 6 2 7
        8 7 6 5 3 9 1 4 2
        5 9 2 6 1 4 7 3 8
        3 4 1 7 8 2 5 9 6
        4 2 7 1 6 3 8 5 9
        6 3 9 8 4 5 2 7 1
        1 5 8 2 9 7 3 6 4))

(define BD5 ;; a hard one
  (list B B 3 B B 8 9 B B
        B B 2 B B B B B B
        B 5 B 2 B B B B 8
        B B B B B 9 B B 4
        B B 5 B 4 B B B 1
        B 3 B B B B 2 B B
        7 B 4 B 9 B B 5 B
        B 1 9 B B B B B B
        B B B 8 B B 1 3 B))


(define BD5s
  (list 6 7 3 4 1 8 9 2 5
        8 4 2 9 6 5 7 1 3
        9 5 1 2 3 7 4 6 8
        1 6 8 3 2 9 5 7 4
        2 9 5 7 4 6 3 8 1
        4 3 7 5 8 1 2 9 6
        7 8 4 1 9 3 6 5 2
        3 1 9 6 5 2 8 4 7
        5 2 6 8 7 4 1 3 9))

(define BD6 ;; This board is impossible to solve. 
  (list 1 2 3 4 5 6 7 8 B
        B B B B B B B B 2 
        B B B B B B B B 3
        B B B B B B B B 4
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))


  
;; Positions of all the ros, columns and boxes:
(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))


;; Functions

;; (r-c->Pos r c) converts 0-based row and column to Pos.


;; Examples:
(check-expect (r-c->Pos 3 6) 33)


;; r-c->Pos: Int Int -> Int
(define (r-c->Pos r c)
  (+ (* r 9) c))


;; Aside: The position in the board can be calculated by knowing its
;; row and column number.
;; Pos = Row * 9 + Col



;; (read-square bd p) produces the value at position p on board bd 


;; Examples: 
(check-expect (read-square BD5 (r-c->Pos 2 3)) 2)


;; read-square: Board Pos -> (anyof Val false)
(define (read-square bd p)
  (list-ref bd p))



;; (fill-square bd p nv) produces a new board with value nv at position
;;    p in board bd


;; Examples:
(check-expect (fill-square BD1 (r-c->Pos 0 0) 1)
              (cons 1 (rest BD1)))


;; fill-square: Board Pos -> (anyof Val false)
(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))



;; (solve bd) produces a solution board, or false if bd is unsolvable


;; Aside:
;; The key of the problem has three parts:
;; we are "generating"
;; an "arbitrary-arity tree"
;; and doing "backtracking search" over it


;; Examples:
(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD6) false)


;; solve: Board -> (anyof Board false)
;; Requires: bd is valid
(define (solve bd)
  (local[(define (solve--bd bd)
           (cond[(solved? bd) bd]
                [else (solve--lobd (next-boards bd))]))

         (define (solve--lobd lobd)
           (cond[(empty? lobd) false]
                [else
                 (local[(define try (solve--bd (first lobd)))]
                        (cond[(not (false? try)) try]
                             [else (solve--lobd (rest lobd))]))]))]
    (solve--bd bd)))

;; Aside: Mutual recursion where one function is for handling board and
;;        one function is for handling a list of boards
;;        This is the main function.



;; (solved? bd) produces true if bd is solved, and false otherwise


;; Examples:
(check-expect (solved? BD1) false)
(check-expect (solved? BD2) false)
(check-expect (solved? BD4s) true)


;; solved?: Board -> Bool
;; Requires: bd is valid
(define (solved? bd)
  (andmap number? bd))



;; (next-boards bd) produces list of valid next boards from bd


;; Examples:
(check-expect (next-boards (cons 1 (rest BD1))) 
              (list (cons 1 (cons 2 (rest (rest BD1))))
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))))


;; next-boards: Board -> (listof Board)
(define (next-boards bd)
  (keep-only-valid (fill-with-1-9 (find-blank bd) bd)))



;; (find-blank bd) produces the position of the first blank square


;; Examples:
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons 2 (rest BD1))) 1)
(check-expect (find-blank (cons 1 (cons 2 (rest (rest BD1))))) 2)


;; find-blank: Board -> Pos
;; Requires: the board has at least one blank square
(define (find-blank bd)
  (cond[(empty? bd) (error "The board does not have any blank spaces")]
       [(false? (first bd)) 0]
       [else (+ 1 (find-blank (rest bd)))]))



;; (fill-with-1-9 p bd) produces 9 boards with blank filled
;;    with Nat 1-9


;; Examples:
(check-expect (fill-with-1-9 0 BD1) (list
                                     (cons 1 (rest BD1))
                                     (cons 2 (rest BD1))
                                     (cons 3 (rest BD1))
                                     (cons 4 (rest BD1))
                                     (cons 5 (rest BD1))
                                     (cons 6 (rest BD1))
                                     (cons 7 (rest BD1))
                                     (cons 8 (rest BD1))
                                     (cons 9 (rest BD1))))


;; fill-with-1-9: Pos Board -> (listof Board)
(define (fill-with-1-9 p bd)
  (local[(define (build-one n)
           (fill-square bd p (add1 n)))]
    (build-list 9 build-one)))



;; (keep-only-valid lobd) produces a list that contains valid boards
;;    from lobd


;; Examples:
(check-expect
 (keep-only-valid (list (cons 1 (cons 1 (rest (rest BD1)))))) empty)


;; keep-only-valid: (listof Board) -> (listof Board)
(define (keep-only-valid lobd)
  (filter valid-board? lobd))



;; (valid-board? bd) producess true if bd is valid, i.e. no unit on the
;;    board has the same value twice, and false otherwise 


;; Examples:
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? (cons 2 (rest BD2))) false)
(check-expect (valid-board? (cons 2 (rest BD3))) false)


;; valid-board?: Board -> Bool
(define (valid-board? bd)
  (local[; (listof Unit) -> Bool 
         (define (valid-units? lou)
           (andmap valid-unit? lou))

         ;; Unit -> Bool
         (define (valid-unit? u)
           (no-duplicates?
            (keep-only-values
             (read-unit u))))

         ;; Unit -> (listof (anyof Val false))
         (define (read-unit u)
           (map read-pos u))

         ;; Pos -> (anyof Val false) 
         (define (read-pos p)
           (read-square bd p))

         ;; (listof (anyof Val false)) -> (listof Val)
         (define (keep-only-values lovf)
           (filter number? lovf))

         ;; (listof Val) -> Bool
         (define (no-duplicates? lov)
           (cond[(empty? lov) true]
                [(member? (first lov) (rest lov)) false]
                [else (no-duplicates? (rest lov))]))]
    (valid-units? UNITS)))
