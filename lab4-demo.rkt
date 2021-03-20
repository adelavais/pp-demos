#lang racket

; Discutie domeniu vizibilitate si context computational
; https://github1s.com/adelavais/bison/blob/HEAD/examples/java/calc/Calc.y


; Legare statica - let
(define (func a b)
  (let ((a 1) (b 2)) (list a b)) ; context 2
  ;(list a b)                    ; context 3
  )

(func 3 4)                       ; context 1

; context 1: se apeleaza functia f; identificatorul a are valoarea 3, b are valoarea 4
; context 2: se intra in corpul let-ului; identificatorul a are valoarea 1, b are valoarea 2
; context 3: la iesirea din let; identificatorul a are valoarea 3, b are valoarea 4


; Legare dinamica - define
; Editarea contextului global e interzisa in Racket -> ar introduce efecte laterale

; Construcţii pentru legare
(define a 1)

(let ((a 10) (b (+ a 1))) (+ a b))   ; 12 -> a 10 devine vizibil abia in corpul functiei

(let* ((a 10) (b (+ a 1))) (+ a b))  ; 21 -> a 10 devine vizibil in legarea urmatoare

;(letrec ((b (+ a 1)) (a 10)) (+ a b)); eroare -> la momentul unui calcul trebuie ca legarea sa se fi efectuat deja
(letrec 
    ((even-length? 
      (lambda (L)                    ; even-length? este o închidere funcțională
        (if (null? L)                ; deci corpul funcției nu este evaluat la
            #t                       ; momentul definirii ei
            (odd-length? (cdr L))))) ; deci nu e o problemă că încă nu știm cine e odd-length?
     (odd-length? 
      (lambda (L) 
        (if (null? L)
            #f
            (even-length? (cdr L))))))
  (even-length? '(1 2 3 4 5 6)))     ; în acest moment deja ambele funcții au fost definite

; named let -> caz de utilizare: wrapper pentru o functie recursiva pe coada
(define (my-rec-length L)
  (let my-tail-wrapper ((L L) (acc 0))
    (if (null? L)
        acc
        (my-tail-wrapper (cdr L) (+ acc 1)))))

(my-rec-length '(1 2 3 4)) ; 4

; inchidere functionala -> o instanta de functie

(define (f x)
  (λ (y)
    (+ x y)))

(define b (f 1))

b     ; procedura (f 1) -> inchidere functionala
(b 5) ; evaluarea procedurii ((f 1) 5)



