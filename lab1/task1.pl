% Первая часть задания - предикаты работы со списками

m_length([],0).
m_length([_|T],S):- m_length(T,Sprev),S is Sprev+1.

m_member([X|_],X):-!.
m_member([_|T],X):- m_member(T,X).

m_append([],R,R):-!.
m_append([H|T],L,[H|Lnew]):-m_append(T,L,Lnew).

m_remove([X|T2],X,T):-!.
m_remove([H|T],X,[H|New]):-m_remove(T,X,New).

m_permute([],[]).
m_permute(T,[X|Tail]):-m_remove(T,X,NList),m_permute(NList,Tail).

m_sublist(_,[]).
m_sublist(T,[X|Tail]):-m_member(T,X),m_sublist(T,Tail).

% task 1
task([],[]).
task(X,R):-X=[_,_,_|R].
%task 2
task2([H|T],R):-delete(T,H,T1),length(T1,R1),length([H|T],R2),R is R2-R1.
