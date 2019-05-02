%Ethan Johnson 

%Q1: oddMultOf3/1
oddMultOf3(N) :- (
                 %if not int, print error
                 not(integer(N))->
                 write('ERROR:The given parameter is not an integer');
                 %oddmultof3?
                 0 is N mod 3 , !, 1 is N mod 2
                 ).


%Q2: list_prod/2
list_prod(List, Int) :- list_prod_Def(List, Int).

%empty
list_prod_Def([],0).
%1 item
list_prod_Def([H],H).
%mult the thingies
list_prod_Def([H|T], Product) :- list_prod(T, Rest), Product is Rest * H.

%Q3: palindrome/1
palindrome(L):-
  reverse(L, L).


%Q4: secondMin/2
secondMin(List, Min2) :-
    (   ground(List) ->
      (   is_list(List),
         maplist(number, List)->
          (   sort(List, [_,Second|_])->
            %if the list has fewer than two unique
             Min2 = Second;
                write('ERROR:List has fewer than two unique elements.')
           );
         %Something is not a number in it
         write('ERROR: something in there is not a number.')
        );
       %Something went horribly, horribly wrong.
       throw(error(instantiation_error, _))
    ).


% Q5: segregate/3
segregate(List, Even, Odd) :- segregate_help(List, Even, Odd).

%base cases
segregate_help([],[],[]).
%even case, and odd case
segregate_help([H|T],[H|X],Odd):- 0 is H mod 2,!,segregate_help(T,X,Odd).
segregate_help([H|T],Even,[H|Y]):- segregate_help(T,Even,Y).


% Q6: bookends/3
bookends(Starter,Ender,AList):- starter(Starter,AList), ender(Ender,AList).

%left side part
starter([],_):- !.
starter([HP|TP],[HL|TL]):- HP == HL,starter(TP,TL).

%right side
ender(L,L):- !.
ender(S,[_|T1]):- ender(S,T1),!.


% Q7: subslice/2
subslice(Sub, List) :- subslice_help(Sub, List).

% Base case
subslice_help([],_):- !.
%subslice main
subslice_help(Sub,[H|T]):- prefix(Sub,[H|T]),!;subslice_help(Sub,T).

% Q8: shift/3
shift(L1, N, L2) :- shift_help(L1, N, L2).

shift_help(L1, N, L2) :-
    N < 0, !,             %negative check
    length(L1, Len),
    N1 is Len + N,
    shift_help(L1, N1, L2).

%combines it back
shift_help(L1, N, L2) :-
    append(Lx, Ly, L1),
    append(Ly, Lx, L2),
    length(Lx, N).

%Q9 : luhn
%calc sum2
sum2(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    Result is N1+N2,!.
%calcs sum
sum(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),

    %calc sum of second over
    N3 is N2*2,
    N3>9 -> sum2(N3,X),Result is X+N1,!;

    %Otherwise, add the digits.
    N1 is mod(Num,10),
    N2 is div(Num,10),
    N3 is N2*2,
    Result is N1+N3,!.

%base case of 2 nums
digitsum(0,Sum,Result):-
    Result is Sum,!.
digitsum(N,Sum,Result):-
    %get last 2 num
    Rem is mod(N,100),
    Queso is div(N,100),
    %calc sum
    sum(Rem,S2),
    NewSum is Sum+S2,
    digitsum(Queso,NewSum,Result),!.
%luhn func
luhn(Num):-
    digitsum(Num,0,Result),
    Rem is mod(Result,10),
    Rem = 0.

%Q10 : geneology
parents(X,Y):-
parent(X,Y).

%mother
mother(X,Y):-
parents(X,Y),
female(X).

%father
father(X,Y):-
parents(X,Y),
male(X).

%child of parents
child(X,Y):-
parents(X,Y).

%grandparents
grandparent(X,Y):-
parents(Z,Y),
parents(X,Z).

%grandfather side
grandfather(X,Y):-
male(X),
parents(Z,Y),
parents(X,Z).

%grandmother side
grandmother(X,Y):-
parents(Z,Y),
parents(X,Z),
female(X).

%grandchild side
grandchild(X,Y):-
parents(Y,Z),
parents(Z,X).

%grandson side
grandson(X,Y):-
parents(Y,Z),
parents(Z,X),
male(X).

%granddaughter side
granddaughter(X,Y):-
parents(Y,Z),
parents(Z,X),
female(X).

%siblings
sibling(X,Y):-
parents(Z,X),
parents(Z,Y),
not(X=Y).
















