;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      CS 3 Scheme programming assignment               ;;;;
;;;;                 April 2013                            ;;;;
;;;;        joe blogg's solutions                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; This depends on "cs3-black-jack.scm"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Your code goes here                                    ;;;;
;;;;  Submit a file of code of everything you created below  ;;;;
;;;;  please do not submit the predefined code              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "c://UCT/CSC3002F/Functional/cs-black-jack.scm")

;;for Testing:
(load "cs-black-jack.scm")
;;"black-jack stupid"
;;(black-jack stupid)
;;"black-jack stop-at-17"
;;(black-jack stop-at-17)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Question 1.  Code for "best-hand"

;; Dummy Version of best-hand
;; (define (best-hand hand)
;;   (min-val hand))

;; Best Value of Hand
(define (equal x y) 
  (if (member x (list y)) #t 
      #f))

(define (card-max-val card)
  (if (equal (car card) 'A) 11 
      (card-min-val card)))

;--test card-max-val--
;(card-max-val '(A h))
;returns 11 - success

(define (ace-count hand count)
  (if (null? hand) count
      (if (equal (car (car hand)) 'A) (ace-count (cdr hand) (+ count 1))
          (ace-count (cdr hand) count))))

;--Test ace-count
;(ace-count '((A h) (A s) (3 d)) 0)
;returns 2 - success
;(ace-count '((3 d) (3 s)) 0)
; returns 0 - success
;(ace-count '((A s) (A h) (3 s) ) 0)
;returns 2 (noted that aces are stored first - success

(define (max-val hand)
  (if (null? hand) 0
      (+ (card-max-val (car hand)) (max-val (cdr hand)))))

;--Test max-val
; (max-val '((A s) (A h) (2 s)))
;returns 24 - success

(define (maximize max-hand aces)
  (if (and (> max-hand 21) (> aces 0)) (maximize  (- max-hand 10) (- aces 1))
      max-hand))

;--Test Maximize
;(value of 2 aces and a 3)
;(maximize 24 2)
;Returns 14 - success 
; (value of 3 aces and a 3)
;(maximize 35 2)
;Returns 15 - success

(define (best-hand hand) (maximize (max-val hand) (ace-count hand 0)))


;--Test Best-Hand
;(best-hand '((A d) (8 s)))
;19 - success
;(best-hand '((A d) (8 s) (5 h)))
;14 - success
;(best-hand '((A h) (A c) (8 h) (8 c) ))
;18 - success


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Question 2.  stop-at takes an argument n that determines a
;;  strategy where a card is taken only if the best total so far is
;;  less than n. 

 (define (stop-at n)
   (lambda (my-hand-so-far dealer-card) 
     (< (best-hand my-hand-so-far) n)))
 
;((stop-at 17) '((7 s) (Q h)) '((A s)))
; (stop-at-17 '((7 s) (Q h)) '((A s)))
 
;--Test stop-at
;(black-jack stop-at-17)
;(black-jack (stop-at 17))
;results are the same.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Question 3.   repeat-game takes a strategy and a number as
;;  arguments returns the number of games won less the number of games
;;  lost 
 
 (define (repeat-help strategy n score)
   (if (= n 0) score
         (repeat-help strategy (- n 1) (+ score (black-jack strategy)))))
   
 
 (define (repeat-game strategy n)
   (repeat-help strategy n 0))
 
 ;--Test Repeat-game
 ;(repeat-game (stop-at 15) 100)
 ;(repeat-game stop-at-17 10)
 ;(repeat-game stupid 100)
 ;--Test success


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Question 4.    clever
;
(define (clever my-hand up-card)
  (cond ((and (<= (best-hand my-hand) 16) 
              (>= (best-hand (list up-card)) 7)) #t)
        ((and (<= (best-hand my-hand) 11) 
              (and (<=(best-hand(list up-card)) 6) 
                   (>= (best-hand (list up-card)) 2)) ) #t)
        (else #f)))
 

;--Test clever
;(black-jack clever)
;(repeat-game clever 500)
;--Test Success

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Question 5.                Majority
;;;
(define (majority-help strategy my-hand dealer-card)
  (if (strategy my-hand dealer-card) 1 
      0))

(define (majority strat-1 strat-2 strat-3)
     (lambda (my-hand dealer-card) 
     (> (+ (majority-help strat-1 my-hand dealer-card)
           (majority-help strat-2 my-hand dealer-card)
           (majority-help strat-3 my-hand dealer-card)) 
        1)))

;--Test Majority with repeat game
;(repeat-game (majority stop-at-17 (stop-at 12) clever) 1000)
;test success

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Question 6.              Get Stats
;;;
(define (stat-list strategy repeat-count data-points)
  (if (> data-points 1)
     (list (repeat-game strategy repeat-count)  (append (get-stats strategy repeat-count (- data-points 1))))))

(define (get-stats strategy repeat-count data-points)
   (stat-list strategy repeat-count data-points))
      

(get-stats clever 100 3);

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Question 8.   interactive 


; function to get the input returns #t if the user types y otherwise #f
(define (hit-me?)
  (eq? (read) 'y))

