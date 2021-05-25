% Aflarea tuturor soluțiilor pentru satisfacerea unui scop

% findall(+Template, +Goal, -Bag)
even(Numbers, Even):-
    findall(X,(member(X, Numbers), X mod 2 =:= 0), Even).

/*
se aseamănă cu list comprehension din Haskell
Even -> lista rezultat
X -> forma elementelor din lista rezultat
predicatul -> condiția pentru care sunt puse elementele în lista rezultat
           -> trebuie teoretic să fie doar unul, dacă este nevoie de mai multe
              se pun într-un set de paranteze

?- even([1,2,3,4,5,6,7,8,9], Even).
Even = [2, 4, 6, 8].

*/

% bagof(+Template, +Goal, -Bag)

/*
-> predicatul bagof seamănă cu findall
-> diferența: bagof construiește câte o listă Bag pentru fiecare instanțiere
   diferită a variabilelor libere din Goal
-> pentru a evita gruparea soluțiilor pentru fiecare valoare separată a
   variabilelor ce apar în scopul lui bagof/3 se poate folosi construcția Var^Goal
*/

digits([1,2,3,4,5,6,7,8,9]).
numbers([4,7,9,14,15,18,24,28,33,35]).

multiples(D,L):-
    digits(Digits),
    numbers(Numbers),
    bagof(N,(member(D, Digits), member(N, Numbers), N mod D =:= 0), L).
    %bagof(N,D^(member(D, Digits), member(N, Numbers), N mod D =:= 0), L).

/*

?- multiples(D,L).
D = 1,
L = [4, 7, 9, 14, 15, 18, 24, 28, 33|...] ;
D = 2,
L = [4, 14, 18, 24, 28] ;
D = 3,
L = [9, 15, 18, 24, 33] ;
D = 4,
L = [4, 24, 28] ;
D = 5,
L = [15, 35] ;
D = 6,
L = [18, 24] ;
D = 7,
L = [7, 14, 28, 35] ;
D = 8,
L = [24] ;
D = 9,
L = [9, 18].

?- multiples(1, L).
L = [4, 7, 9, 14, 15, 18, 24, 28, 33|...].

*/

% setof(+Template, +Goal, -Bag)

/*
-> are aceeași comportare cu bagof/3, dar cu diferența că soluțiile găsite
   sunt sortate și se elimină duplicatele
*/

are(andrei, laptop, 1).
are(andrei, pix, 5).
are(andrei, ghiozdan, 2).

are(radu, papagal, 1).
are(radu, ghiozdan, 1).
are(radu, laptop, 2).

are(ana, telefon, 3).
are(ana, masina, 1).

/*
findall(X, are(_, X, _), Bag).
Bag = [laptop, pix, ghiozdan, papagal, ghiozdan, laptop, telefon, masina].

bagof(X, X^P^C^are(P, X, C), Bag). % echivalent cu findall
bagof(X, P^C^are(P, X, C), Bag). % echivalent cu findall

bagof(X, are(andrei, X, _), Bag). % legari in functie de _
Bag = [laptop] ;
Bag = [ghiozdan] ;
Bag = [pix].

bagof(X, C^are(P, X, C), Bag).
P = ana,
Bag = [telefon, masina] ;
P = andrei,
Bag = [laptop, pix, ghiozdan] ;
P = radu,
Bag = [papagal, ghiozdan, laptop].

setof(X, C^are(P, X, C), Bag).
P = ana,
Bag = [masina, telefon] ;
P = andrei,
Bag = [ghiozdan, laptop, pix] ;
P = radu,
Bag = [ghiozdan, laptop, papagal].

setof(X, P^C^are(P, X, C), Bag).
setof(X, X^P^C^are(P, X, C), Bag).
Bag = [ghiozdan, laptop, masina, papagal, pix, telefon].

*/

% forall(?Cond, ?Acțiune)

/*
-> se verifică dacă există o instanță a lui cond pt care acțiunea e falsă

L = [2,3,5,7], member(Min,L), forall(member(N,L), N>=Min).
L = [2, 3, 5, 7],
Min = 2 .

L = [2,3,5,7], member(Min,L), forall(member(N,L), N mod 2 =:= 0).
false
*/