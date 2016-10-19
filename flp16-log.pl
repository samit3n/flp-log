/**
*   FLP-log 2016
*   Turinguv stroj
*
*   Implementace simulatoru turingova stroje
*
*
*   Autor: Vojtech Dvoracek
*   Email: xdvora0y@stud.fit.vutbr.cz
*   Datum: 29.4.2016
*
*/

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
* You can set maximal number of operations here
* 
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

cycle_limit(1000).


:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- dynamic tape/2, rule/4, abn_stop/1.

abn_stop(true).

/**
*  Example data TODO REMOVE
*/

sample_rules:- assertz(rule('S','a','B','a')),
               assertz(rule('B','a','B','b')),
               assertz(rule('B','c','B','R')),
               assertz(rule('B','c','B','a')),
               assertz(rule('B','x','F','c')),
               assertz(rule('B','x','F','d')).

sample_tape:- 
    string_chars("|Boaacaa", C), assertz(tape(1,C)),
    string_chars("|Bxaacae", D), assertz(tape(1,D)),
    string_chars("|Bxaacbe", E), assertz(tape(1,E)).


sample :- sample_rules, sample_tape.

remTS :- retractall(rule(_,_,_,_)), retractall(tape(_,_)).

/**
* First and rest items
*/

head([],_) :- false.
head([L|_], L).

/**
* Chops last elem from given list
* Returns last elem and chopped list
*/

chop([X],X,[]).
chop([X|XS], R, [X|LS]) :- chop(XS,R,LS).


/**
* Splits given tape to Actual state, Left and Right part.
*/

splitTape([T|TS], T , [],  TS) :-  char_type(T, upper), !.
splitTape([T|TS], St, [T|LS], RS) :-  splitTape(TS, St, LS, RS). 

/**
* Returns actual TS state and symbol under head base on tape/1.
*/

getState(Tape, St,Head) :-  splitTape(Tape, St, _,R), head(R,Head).

shift(Tape, Dir, Sn, Tnew) :- 
          splitTape(Tape, _, L, R), 
          S =.. [Dir,L,R,Ls,Rs],
          call(S),
          splitTape(Tnew, Sn, Ls, Rs). /*join parts together */


right([],[],['-'],[]) :- !.
right([],[R|RS],[R], RS ) :-! .
right(L, [], LSp, []) :- append(L, ['-'], LSp).
right(L, [R|RS], LSp, RS) :- append(L, [R],LSp).

left(['|'],_,_,_) :- write("NTS crossed left tape end\n"), !, fail.

left([],[],[],['-']) :- !.
left([],R,[],Rn) :- append(['-'], R,Rn), !.
left(L, R, Ln, Rn):- chop(L, Lx , Ln),  append([Lx],R,Rn), !.

/**
* Predicate execs given operation on TS
*
* apply( actual tape, next state, next head, next tape) 
*
*/

apply(T, [Sn,'R'], Tnew) :-  shift(T, right, Sn, Tnew).
apply(T, [Sn,'L'], Tnew) :-  shift(T, left, Sn, Tnew).

apply(T, [Sn,Hn], Tnew) :- 
        splitTape(T, _, L,[_|R]), /* splits tape to logical parts */
        splitTape(Tnew, Sn, L,[Hn|R]). /* glue them together again */

apply(_,_) :- write("Wrong operation to perform\n"), fail.

/*
* Applies list of possible transitions to actual type
* and returns list of new tapes.
*
*/
applyList(_, [], []).
applyList(T, [Op|Ops], [Tn|Tns]) :- apply(T, Op, Tn), applyList(T, Ops, Tns).


/*
* Returns list item which satisfied predicate
* false if not found
*/

find(_, [], _ ) :- false, !.
find(P, [L|LS], Y) :- 
    Pc =.. [P, L],
    ( call(Pc) -> 
        Y = L
        ;
        find(P, LS,Y)
    ).


/*
* Predicates for selecting rules applicable on TS state, defined on Tape
* Rules leading to F state has priority and if multiple occurs, first is selected.
* Otherwise all posible pairs [Next State, Nex symbol under head] are returned
*
* If predicate fails, TS stopped abnormally
*/

selectRule(Tape,[NStates]) :- 
        getState(Tape,S,H),
        bagof(['F',Hn], rule(S,H,'F',Hn),X),
        head(X, NStates), !. /* In case of mulitple final rules, select first one */ 
        

selectRule(Tape, NStates) :- 
    getState(Tape,S,H),
    bagof([Sn,Hn], rule(S,H,Sn,Hn),NStates).
    


/************************
*   
*   NTS RUNNING PREDICATES
*
**************************/

isGoal(T) :- last(T,LT), splitTape(LT,'F',_,_), !.
isGoalAll(L,R) :- find(isGoal, L, R).


/*
*  Generates all non-determ. next states
*
*/

genTapes(_, []).
genTapes(It, [H|HS]) :- 
    (genTape(H, NT) -> 
        (
            storeHist(It, H, NT), /* Store new state if applicable rule found */
            retract(abn_stop(_)),
            assertz(abn_stop(false)), /* NTS did not stop abnormally */
            genTapes(It, HS)
        )
            ;
        (
            assertz(tape(It,H)), 
            genTapes(It,HS)
        )
    ).


storeHist(_, _,  []).
storeHist(It, H, [NT|NTS]) :- 
    append(H, [NT],Tmp),
    assertz(tape(It, Tmp)),
    storeHist(It,H, NTS).
    

genTape(Hist,NTapes) :- 
    last(Hist, TAct),          /* actual tape is last elem in list */
    selectRule(TAct,NStates),   /* select all applicable rules */
/*    write("selrule:"), write(NStates),write('\n'),*/
    applyList(TAct, NStates,  NTapes).
/*    write("applied: "), write(NTapes), write('\n') */
/*
* Input accepted
*/
    

run(It) :- 
    findall(T, tape(It, T),X),
/*    write("fin. foundall"), write(X),write('\n'), */
    isGoalAll(X,Res),
    printTapeAll(Res),!.

/*
* Run or stop abnormally
*/

run(It) :- 
    cycle_limit(L),
    \+ It == L,
    retract(abn_stop(_)),
    assertz(abn_stop(true)),
    findall(T, tape(It, T),Hists),
    retractall(tape(It, _)),
    Inc is It + 1,
    genTapes(Inc, Hists),

    ( abn_stop(true) ->
        (
            atom_string(String,"NTS stopped abnormally, no applicable rule found\n"),
            write(String)
        )
            ;
        ( 
            run(Inc)
        )
    ).

run(It) :- 
    write("NTS reached operation limit: "),
    write(It),
    write(", it probably won't accept.\n").




/************************************
*
*       INPUT PARSING PREDICATES
*
**************************************/

isLow(' ') :- !.
isLow(C):- char_type(C,lower), !.

isLowList([]):- false.
isLowList(L) :- maplist( isLow, L).

/**
* save input tape to DB beginning with
* start symbol
*/

parseTape(Cs) :- append(['|','S'], Cs, Ts ), assertz(tape(0,[Ts])).

/**
* Checks rule validity and adds no rule
* to database
*/

parseRule(Cs) :- 
               head(Cs,S1),
               nth0(2,Cs,I),
               nth0(4,Cs,S2),
               nth0(6,Cs,O),
               char_type(S1,upper),
               (
                   char_type(I, lower);
                   I == ' '
               ),

               char_type(S2,upper),

               ( 
                 char_type(O,lower); 
                 O == 'R' ; 
                 O == 'L' ;
                 O == ' '
               ),

              assertz( rule(S1,I,S2,O) ) .

/**
* converts ordinal chars list to characters list
*/

atomCharsLst([], []):-  !.
atomCharsLst([X|XS], [R|RS]) :- atom_char(R,X), atomCharsLst(XS, RS).

/**
* Opens input - regular file or stdin
*/

openRead(user_input, Lns) :-
   readFile(user_input, Lns).

openRead(File, Lns) :-
    open(File,read, Str),
    readFile(Str,Lns), !,
    close(Str).

/**
* Parses data from given input stream.
*/
readFile(Str, []) :- at_end_of_stream(Str).

readFile(Str, [X|XS]) :-
    \+ at_end_of_stream(Str),
    read_line_to_codes(Str,Cds),
    atomCharsLst(Cds, X),
    readFile(Str,XS).


parse([]) :- !.
parse([X]) :- isLowList(X), parseTape(X) , !.
parse([X|XS]) :- 
    parseRule(X),
    parse(XS).


/**
* Tape output
* printTape : print single tape
*/
printTape([_|TP]) :- atom_string(TP, Str), write(Str), write('\n').

printTapeAll(L) :- maplist(printTape, L).
/**
*
* DEBUG OUTPUT: Print actual TS state to STDOUT
*/


printRules([]) :- !.
printRules([R|RS]) :- text_to_string(R,Str), write(Str), write('\n'), printRules(RS).


testPrint :- write("Loaded TS:\n"), 
             findall([A,B,C,D], rule(A,B,C,D), Rs),
             printRules(Rs),
             printTape.


/**
* returns snd arg if exists
* otherwise returns user_input eg. stdin
*/         

file([_|XS], Y) :- head(XS, Y), !.
file(_, Y) :- Y = user_input.


/******************
*   MAIN 
*******************/

main(Args) :-

    file(Args,F), 
    openRead(F, Lines),
    parse(Lines),
    run(0).
