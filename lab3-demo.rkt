#lang racket

; reutilizare de cod - functii curry si uncurry
; aplicatie convertor bancar

(define conv-curry
    (λ(x)
      (λ(y)
        (* x y))))

(define conv-uncurry
    (λ (x y) ; observatie: nu e necesar sa folosesc lambda aici
      (* x y))) 

((conv-curry 3) 4) ; 12 ; observatie: un set suplimentar de paranteze -> se aplica 2 functii
(conv-curry 3)     ; #<procedure> ; inlocuieste x cu parametru 3 si intoarce o procedura -> λ(y)(* 3 y)
(conv-uncurry 3 4) ; 12
;(conv-uncurry 3)  ; eroare


; convertor valutar pt 1 valoare
(define (lire->lei-curry x)
  ((conv-curry 5.5) x))
;sau
(define (lire->lei-uncurry x)
  (conv-uncurry 5.5 x))

; call-ul exterior are aceeasi sintaxa
(lire->lei-curry 2)   ; 11.0
(lire->lei-uncurry 2) ; 11.0


; convertor valutar pt o lista de valori
(define (lei->lire L)
  (if (null? L)
      '()
      (cons (/ (car L) 5.5) (lei->lire (cdr L)))))

(define (lire->lei L)
  (if (null? L)
      '()
      (cons (* (car L) 5.5) (lei->lire (cdr L)))))

; probleme: mult cod duplicat
; observam ca am putea scrie o functie generala, in care aplicam o functie pe toate elementele unei liste
; pentru functiile de mai sus, singura diferenta ar fi chiar functia aplicata (inmultire/impartire cu 5.5)
(define (aplic-f f L)
  (if (null? L)
      '()
      (cons (f (car L)) (aplic-f f (cdr L)))))

(define (lei->lire-aplic L) (aplic-f (conv-curry 5.5) L)) ; (conv-curry 5.5)
(define (lire->lei-aplic L) (aplic-f (λ(x) (/ x 5.5)) L))

(lei->lire-aplic '(1 2 3))         ; '(5.5 11.0 16.5)
(lire->lei-aplic '(5.5 11.0 16.5)) ; '(1.0 2.0 3.0)

; functionale
(map (λ(x) (* x 5.5)) '(1 2 3)) ; '(5.5 11.0 16.5)
(map (λ(x y) (* x y)) '(1 2 3) '(5.5 5.5 5.5)) ; observatie: liste de aceeasi lungime
(map + '(1 1 1) '(2 2 2))        ; '(3 3 3)
(filter odd? '(1 2 3))           ; '(1 3)
(apply + '(1 2 3))               ; 6 ; aplica functia + pe elementele listei
(apply map + '((1 1 1) (2 2 2))) ; '(3 3 3) ; observatie: sintaxa de lista de liste are ' doar la inceput
(foldr cons '() '(1 2 3))        ; '(1 2 3)
(foldl cons '() '(1 2 3))        ; '(3 2 1)
