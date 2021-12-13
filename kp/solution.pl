:-["family_tree.pl"].
relation(father,X,Y):-parent(X,Y),sex(X,m).
relation(mother,X,Y):-parent(X,Y),sex(X,f).
relation(husband,X,Y):-sex(X,m),relation(father,X,A),relation(mother,Y,A).
relation(wife,X,Y):-sex(X,f),relation(mother,X,A),relation(father,Y,A).
relation(sister,X,Y):-sex(X,f),relation(father,A,X),relation(father,A,Y),X \= Y.
relation(brother,X,Y):-sex(X,m),relation(father,A,X),relation(father,A,Y),X \= Y.
relation(zolowka,X,Y):-relation(wife,Y,A),relation(sister,X,A).