% lungime(+Lista,-Lungime)
lungime([], 0).
lungime([_|R], N) :- lungime(R, N1), N is N1 + 1.

/*
?- lungime([1,2,3],N).
N = 3.

Prolog incearca satisfacerea scopului lungime([1,2,3],N).
prin instanțierea convenabilă a variabilei N
*/

% membru(?Elem,+Lista)
membru(Elem, [Elem|_]).
membru(Elem, [_|Rest]) :- membru(Elem, Rest).

/*
?- membru(3, [1, 2, 3, 4, 5]).
true ;
false.

?- membru(N, [1, 2, 3]).
N = 1 ;
N = 2 ;
N = 3 ;
false.
*/

/*
solutii bazate pe combinatii
?-  [L1,L2,L3]=[[1,2,3], [4,5,6], [5,6]], member(X, L1), member(Y, L2), S is X + Y, \+ member(S, L3).
L1 = [1, 2, 3],
L2 = [4, 5, 6],
L3 = [5, 6],
X = 1,
Y = 6,
S = 7 ;
L1 = [1, 2, 3],
L2 = [4, 5, 6],
L3 = [5, 6],
X = 2,
Y = 5,
S = 7 ;
... (in total 6 solutii)
*/

/*
Obținerea de soluții prin generare și testare

Fie problema colorării a 7 țări de pe o hartă folosind 3 culori.
Scopul este acela de a atribui câte o culoare fiecărei țări, astfel
încât nicio țară să nu aibă niciun vecin de aceeași culoare cu aceasta.
Soluția problemei va fi o listă de atribuiri din domeniul ["r", "g", "b"],
care desemnează culorile atribuite fiecărei țări (1, 2, 3, 4, 5, 6, 7).

% predicat care verifică că toate elementele din prima listă sunt prezente în a doua
all_members([], _).
all_members([X | Rest], In) :- member(X, In), all_members(Rest, In).

% predicat care verifică faptul că țările nu au culori identice cu niciun vecin
solve(S) :- L = [_ | _], length(L, 7), all_members(L, ["r", "g", "b"]), safe(S).

imbunatatire:

% Lungimea soluției este cunoscută și fixă.
template([1/_, 2/_, 3/_, 4/_, 5/_, 6/_, 7/_]).

correct([]) :- !.
correct([X/Y | Others]):-
       correct(Others),
       member(Y, ["r", "g", "b"]),
       safe(X/Y, Others).

solve_maps(S):-template(S), correct(S).

Regulă: Atunci când calea către soluție respectă un anumit template (avem de
instanțiat un număr finit, predeterminat, de variabile), este eficient să definim
un astfel de template în program.
Observație: În exemplul de mai sus am reținut explicit ordinea celor 7 țări.
Redundanța în reprezentarea datelor ne asigură un câștig în viteza de calcul
(câștigul se observă la scrierea predicatului safe).
*/

% Controlul execuției: operatorul cut (!), negația (\+) și false

/*
Negația ca eșec (\+)

Prolog utilizează presupunerea lumii închise: ceea ce nu poate fi demonstrat,
este fals. De aceea, în Prolog \+ p trebuie citit ca “scopul p nu poate fi
satisfăcut” sau ”p nu poate fi demonstrat”. Faptul că Prolog utilizează negația
ca eșec (eng. negation as failure) are implicații asupra execuției programelor.

În logica de ordinul întâi, următoarele două expresii sunt echivalente:
¬a(X) & b(X) și b(X) & ¬a(X). În Prolog, următoarele 2 clauze (p1 și p2) vor
produce rezultate diferite:
*/

student(andrei). student(marius). lazy(marius).
p1(X) :- student(X), \+ lazy(X).
p2(X) :- \+ lazy(X), student(X).

/*
̀?- p1(X).
X = andrei ;
false.

?- p2(X).
false.

In Prolog putem folosi negația doar pentru a verifica variabile deja legate,
sau pentru a exprima faptul că nu se poate demonstra că predicatul este adevărat.
*/

/*
Predicatul false

- folosit cand nu vrem ca o regula sa influenteze restul programului (ex: debugging)
*/

my_reverse(List, Acc, _) :- format('List:~w, Acc:~w~n', [List, Acc]), false.
my_reverse([], Sol, Sol).
my_reverse([Head | Tail], Acc, Sol):-my_reverse(Tail, [Head | Acc], Sol).

/*

?- my_reverse([1,2,3], [], X).
List:[1,2,3], Acc:[]
List:[2,3], Acc:[1]
List:[3], Acc:[2,1]
List:[], Acc:[3,2,1]
X = [3, 2, 1].

*/

/*
Predicatul cut (!)

- are rolul de a elimina toate punctele de bifurcație create în predicatul curent
- la evaluarea predicatului cut într-un predicat p, se vor realiza două tipuri de
efecte:
    * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curent eșuează)
    pentru alte reguli ale predicatului p
    * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curent eșuează),
    pentru alte soluții ale condițiilor care apar în aceeași regulă cu cut, și înainte
    de cut.
*/

p(a).
p(b).
p(A/B) :- q(A), !, t(A/B).
p(d).

q(a).
q(b).
q(c).

t(a/a).
t(a/b).
t(b/c).
t(b/d).

/*
?- p(X).
X = a ;
X = b ;
X = a/a ;
X = a/b.
*/

min(X, Y, Min) :- X < Y, X = Min. % regula 1
min(X, Y, Min) :- X >= Y, Y = Min. % regula 2

/*

?- min(3,1,X).
X = 1.

?- min(2,3,X).
X = 2 ;
false.

- stim ca min ar trebui sa aiba raspuns unic, dar Prolog incearca sa gaseasca
si alte solutii => cand a gasit o solutie, vrem sa opreasca backtracking-ul
*/

% green cut -> cut poate sa lipseasca si executia inca este corecta
min2(X, Y, Min) :- X < Y, !, X = Min. % regula 1
min2(X, Y, Min) :- X >= Y, Y = Min. % regula 2

% red cut -> cut asigura corectitudinea solutiei, nu poate lipsi din algoritm
min3(X, Y, Min) :- X < Y, !, X = Min.
min3(_, Y, Min) :- Y = Min.


