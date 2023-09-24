#lang racket

(require "etapa2.rkt")
(require rnrs/mutable-pairs-6)


(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de




; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.



(define (get-unstable-couples engagements mpref wpref)
  (let* ((rev (lambda (pair) (cons (cdr pair) (car pair))))
         (reverse-all (map rev engagements))
         ( better?(lambda(x) (let* ((w (car x))
                                    (m (cdr x))
                                    (mpreff (get-pref-list mpref m))
                                    (wpreff (get-pref-list wpref w))
                                    )
                               (or (better-match-exists? w m wpreff mpref reverse-all ) (better-match-exists?  m w mpreff wpref engagements ))
                               )))
         )(filter better? engagements)))



           


    



  
; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
;; changes the partner of a woman in a list of couples





(define (engage free-men engagements mpref wpref)
  (let loop1 ((bachelors free-men)
              (engagements engagements)
              )
    (if (null? bachelors)engagements
        (let* ((m (car bachelors))
               (man-pref (get-pref-list mpref m))
               )
          (let loop2 ((pref man-pref)
                      (engagements engagements)
                      )
            (if (null? pref)engagements
                (let* ((w (car pref))
                       (woman-pref (get-pref-list wpref w))
                       (n (get-partner engagements w))
                       
                       )
                  (if (eq? #f n )(loop1 (cdr bachelors) (append (cons (cons w m) '[]) engagements))
                      (if (preferable? woman-pref m n) (loop1 (cons n (cdr bachelors)) (update-engagements engagements w m)  )
                          (loop2 (cdr pref) engagements)
                          )  

                      )
                  )
                )
            )

          )
        )
    ))

;(require racket/trace)
;(trace engage)
;(engage '(adi bobo) '((cora . cos)) men-preferences-0 women-preferences-0)

;(define (engage free-men engagements mpref wpref) 'your-code-here)


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '[] mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (lambda (x ac)
           (cons (cdr x)(cons (car x) ac))) '() pair-list
                                            ))

