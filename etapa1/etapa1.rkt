#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.
(define (get-men mpref)
  (if (null? mpref)
      '[]
      (cons (caar mpref) (get-men (cdr mpref)))
      )
 )


; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.

(define (get-women-helper wpref wacc)
  (if (null? wpref)
      (reverse wacc)
      (get-women-helper (cdr wpref) (cons (caar wpref) wacc))
      )
  )

(define (get-women wpref)
 
      (get-women-helper wpref '[])
  )


; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.
(define (get-pref-list pref person)
  (cond ((null? pref) '[])
       ( (eq? (caar pref) person ) (car(cons (cdar pref) (get-pref-list (cdr pref) person))))
       (else (get-pref-list (cdr pref) person))
    )
  )


; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a  obține același efect.


(define (preferable? pref-list x y)
  (or (null? pref-list)
      (equal? (car pref-list) x)
      (and (not (equal? (car pref-list) y))
           (preferable? (cdr pref-list) x y))))
    


; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne ,,, deeeci lista de perechi cu pct 
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.
(define (get-partner engagements person)
  (cond ((null? engagements)
        #f)
        ((eq? (caar engagements) person)
         (cdar engagements))
        (else (get-partner (cdr engagements) person))
        
  ))

;(get-partner engagements (car p1-list))
; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2=
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit
(define (eqhelp1 list p)
  (if (eq? (car list) p)
         '[]
        (cons (car list) (eqhelp1 (cdr list) p)))
  )




(define (better-match-exists? p1 p2 p1-list pref2 engagements) 
   (if (null? (eqhelp1 p1-list p2))
       #f
      (if(= 1 (length (eqhelp1 p1-list p2))) (preferable? (get-pref-list pref2 (car (eqhelp1 p1-list p2))) p1 (get-partner engagements (car(eqhelp1 p1-list p2))))
         (preferable? (get-pref-list pref2 (cadr (eqhelp1 p1-list p2))) p1 (get-partner engagements (cadr(eqhelp1 p1-list p2))))
         ))
       )
 