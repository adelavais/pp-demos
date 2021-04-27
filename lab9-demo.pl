/*

Despre Prolog

* limbaj logic, descriptiv

* specifica problema de rezolvat dpdv al
- unor fapte cunoscute despre obiectele universului problemei
- ai relațiilor existente între aceste obiecte

* tot ceea ce nu este cunoscut sau nu poate fi demonstrat este
considerat a fi fals (ipoteza lumii închise)

* execuția unui program Prolog constă în deducerea implicațiilor
acestor fapte și relații

* entitati continute de programele Prolog:
- fapte
- reguli
- scopuri

*/

% Fapte - predicate de ordinul întâi
papagal(coco). % constantele din Prolog incep cu litera mica, variabilele cu litera mare
iubeste(mihai, maria).
iubeste(mihai, ana).
frumoasa(ana).
bun(gelu).
deplaseaza(cub, camera1, camera2).

% structuri - sintaxa de fapte, dar sunt argumente pentru predicate
are(ion,carte(aventuri,2002)).


/*

Scopuri
- obtinerea de rezultate
- predicate pentru care se doreste aflarea valorii de adevar
- pot fi văzute ca întrebări, rezultatul unui program Prolog este răspunsul la o întrebare
(sau la o conjuncție de întrebări)

*/

% Debug
mypred():- write('String de debug').

/*

Variabile
- exemplele de mai sus sunt obiecte particulare, numite și constante sau atomi simbolici
- predicatele Prolog admit ca argumente și obiecte generice numite variabile
- tipuri: instantiata sau neinstantiata (libera)
- semnul _ isi pastreaza semnificatia din Haskell (nu ne intereseaza valoarea)
- programul leaga toate valorile daca in consola apasati ;
- la realizarea primei unificări se marchează faptul care a unificat și care reprezintă prima soluție.
- la obținerea următoarei soluții, căutarea este reluată de la marcaj în jos în baza de cunoștințe.
- sistemul Prolog, fiind un sistem interactiv, permite utilizatorului obținerea fie a primului răspuns,
fie a tuturor răspunsurilor. În cazul în care, după afișarea tuturor răspunsurilor, un scop nu mai poate
fi resatisfăcut, sistemul răspunde false.

papagal(CineEste).
deplaseaza(_, DeUnde, Unde).

*/

% Reguli -  un fapt care depinde de alte fapte
% S :- S1, S2, ..., Sn.
% S -> antet de regula, S1, S2... -> corpul regulii
% S e adevarat daca S1, S2... sunt adevarate

frumoasa(ana).                                         %1
bun(vlad).                                             %2
cunoaste(vlad, maria).                                 %3
cunoaste(vlad, ana).                                   %4
iubeste(mihai, maria).                                 %5 - fapt
iubeste(X, Y):- bun(X), cunoaste(X, Y), frumoasa(Y).   %6 - regula

% am explicitat predicatul "iubeste" si printr-un predicat si printr-o regula.

/*
Operatori
- Aritmetici: + - * /
- Relaționali: =\= < > =< >= =:= is (=:= și is forțează evaluarea unei expresii, = verifica egalitatea structurală)
- Logici: , (si) ; (sau) \+ (not)
- Negare: \+ (nu se poate demonstra ca operandul e adevarat pe nicio legare)


?- 1 + 2 = 3.
false. % verifica structural/"ca string"

?- 1 + 2 = 2 + 1.
false. % verifica structural/"ca string"

?- 1 + 2 = 1 + 2.
true. % verifica structural/"ca string"

?- 1 + 2 = 1 +2.
true. % nu tine cont de whitespace

?- 1 + 2 = 1+2.
true.  % nu tine cont de whitespace

?- 1 + 2 =:= 3.
true. % forteaza evaluarea lui 1 + 2 la 3

?- 1 + 2 is 3.
false. % 1 + 2 este rezultatul evaluarii expresiei 3? NU

?- 3 is 2 + 1.
true. % 3 este evaluarea expresiei 1 + 2? DA

*/

% liste
returnFirst([PrimulElem | RestulElementelor], Rezultat):- Rezultat is PrimulElem.
% ?- returnFirst([1,2,3,4], R).
% R = 1.

returnFifth([Unu, Doi, Trei, Patru, Cinci|Restul], R):- R is Cinci, write(Restul).
/*
era scris mai corect daca in locul variabilelor nefolosite puneam _

?- returnFifth([1,2,3,4,5,6,7,8,9], R).
[6,7,8,9]
R = 5.

?- returnFifth([1,2,3,4,5], R).
[]
R = 5.

?- returnFifth([1,2,3,4], R).
false.
*/


/*

Documentarea predicatelor și a argumentelor

predicat/nrArgumente
predicat(+Arg1, -Arg2, ?Arg3, …, +ArgN)

Pentru a diferenția intrările (+) de ieșiri(-), se prefixează argumentele cu indicatori. Acele argumente care pot fi fie intrări, fie ieșiri se prefixează cu '?'. Instanțierea parametrilor ține de specificarea acestora:

Arg1 va fi instanțiat atunci când se va încerca satisfacerea p/3
Arg2 se va instanția odată cu satisfacerea p/3
Arg3 va putea fi instanțiat sau nu atunci când se va satisface p/3

*/