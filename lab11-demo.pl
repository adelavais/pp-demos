/* număr nedeterminat de stări intermediare

solve(Solution):-
    initial_state(State),
    search([State], Solution).

search(+StăriVizitate,-Soluţie)
- next_state/2
- final_state/2

search([CurrentState|Other], Solution):-
    final_state(CurrentState), !,
    reverse([CurrentState|Other], Solution).

search([CurrentState|Other], Solution):-
    next_state(CurrentState, NextState),
    \+ member(NextState, Other),
    search([NextState,CurrentState|Other], Solution).

*/

/* căutare în lăţime (BFS)

- se doreşte drumul minim între o stare iniţială şi o stare finală
- expandarea stărilor “vechi” are prioritate în faţa expandării stărilor “noi”

do_bfs(Solution):-
    initial_node(StartNode),
    bfs([(StartNode,nil)], [], Discovered),
    extract_path(Discovered, Solution).

bfs(+CoadaStărilorNevizitate,+StăriVizitate,-Soluţie)

*/

/* căutare A*

- algoritm de căutare informată de tipul best-first search
- caută calea de cost minim (distanță, cost, etc.) către scop
- dintre toate stările căutate, o alege pe cea care pare să conducă cel mai repede la soluție
- A* selectează o cale care minimizează f(n) = g(n)+ h(n), unde
        n este nodul curent din cale,
        g(n) este costul de la nodul de start până la nodul n
        h(n) este o euristică ce estimează cel mai mic cost de la nodul n la nodul final.

astar_search(Start, End, Grid, Path) :-
    manhattan(Start, End, H),
    astar(End, [H:Start], [Start:("None", 0)], Grid, Discovered),
    get_path(Start, End, Discovered, [End], Path).

astar(+End, +Frontier, +Discovered, +Grid, -Result)
        End - starea finală
        Frontier - coadă de priorități
        Discovered - stări vizitate
        Grid - Hartă

*/