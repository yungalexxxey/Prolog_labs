:-load_files(['solution.pl']).

% :- debug.

:- nb_setval(prev_female, noperson).
:- nb_setval(prev_male, noperson).

ask_question(R) :-
    ask_question(R, Ans),
    print_res(Ans).

ask_question(R, Ans) :-
    read_string_to_list(R, A),
    parse_question(Model, A, []),
    parse_model(Model, Args),
    generate_ans(Args, Ans).

read_string_to_list(S, L) :-
    split_string(S, " ", " ?", L).

parse_question(X) --> n1r_q(X). % Who is N R
parse_question(X) --> n2r_q(X). % Whose R is N  
parse_question(X) --> rn1n2_q(X). % What kind/type relations between N1 and N2
parse_question(X) --> num_q(X). % How many R does/did/do N have

n1r_q(n1r_q(W1, W2, W3)) -->
    q_word(W1, n1r_q),
    add_verb(be_forms),
    person(W2, posessive, []),
    rec_relative(W3, _).


n1r_q(n1r_q(W1, W3, W2)) -->
    q_word(W1, n1r_q),
    add_verb(be_forms),
    article,
    rec_relative(W2, _),
    ["of"],
    person(W3, common, []).

n2r_q(n2r_q(W1, W2, W3)) -->
    q_word(W1, n2r_q),
    rec_relative(W3, _),
    add_verb(be_forms),
    person(W2, common, []).


rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    q_word(W1, rn1n2_q1),
    [X], {member(X, ["kind", "type"])},
    ["relations", "between"],
    person(W2, common, []),
    ["and"],
    person(W3, common, []).

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    q_word(W1, rn1n2_q2),
    add_verb(be_forms),
    person(W2, common, []),
    ["for"],
    person(W3, common, []).


num_q(num_q(W1, W2, W3)) -->
    q_word(W1, num_q),
    rec_relative(W2, _),
    add_verb(do_forms),
    person(W3, common, []),
    [X1], {string_lower(X1, X), member(X, ["have", "had"])}.

q_word(q_word(["who"]), n1r_q) --> [X], {string_lower(X, "who")}.
q_word(q_word(["whose"]), n2r_q) --> [X], {string_lower(X, "whose")}.
q_word(q_word(["what"]), rn1n2_q1) --> [X], {string_lower(X, "what")}.
q_word(q_word(["who"]), rn1n2_q2) --> [X], {string_lower(X, "who")}.
q_word(q_word(["how", "many"]), num_q) --> [X, Y], {string_lower(X, X1), string_lower(Y, Y1), X1 = "how", Y1 = "many"}.

rec_relative(relative(L), Pl) -->
    relative_noun(X, Pl),
    [Of], {string_lower(Of, "of")},
    rec_relative(relative(L1), Pl),
    {append([X], L1, L)}.
rec_relative(relative([X]), Pl) --> relative_noun(X, Pl).

relative_noun(Y,s) --> [X], {string_lower(X, Y), 
                             member(Y, ["brother", "sister", "mother", "child", "father", "wife", "husband", "son", "daughter", "motherinlaw"])}.
relative_noun(R,pl) --> [X], {string_concat(R,"s",X), relative_noun(R,s,[R],[])}.

person(person(X), common, X) :-
    sex(X, _).

person(R, posessive, P) :-
    string_concat(X,"'s", P),
    person(R, common, X).

person(Pers, CP, L) --> [],
                        {reverse(L, R),
                         atomics_to_string(R, ' ', P),
                         person(Pers, CP, P)}, !.
person(Pers, CP, L) --> [P], person(Pers, CP, [P|L]), !.
person(Pers, CP, L) --> [P], {reverse([P|L], R),
                             atomics_to_string(R, ' ', P2),
                             person(Pers, CP, P2)}, !.

person(person(P), posessive, _) --> [P1], {string_lower(P1, P), member(P,["his","her","him"])}.

person(person(P), common, _) --> [P1], {string_lower(P1, P), member(P, ["he", "she"])}.

all_pers(R1, R2, _, []) --> person(R1, _, []), person(R2, _, []).



add_verb(be_forms) --> [P1], {string_lower(P1, P), member(P, ["is", "was", "were"])}.
add_verb(do_forms) --> [P1], {string_lower(P1, P), member(P, ["do", "does", "did"])}.

article --> [X1], {string_lower(X1, X), member(X, ["the", "a", "an"])}.
article --> [].

parse_model(Model, model(n1r_q, P1, R)) :-
    Model = n1r_q(q_word(_), person(N1), relative(R)),
    handle_prev_pronoun(N1, P1).


parse_model(Model, model(n2r_q, P2, R)) :-
    Model = n2r_q(q_word(_), person(N2), relative(R)),
    handle_prev_pronoun(N2, P2).


parse_model(Model, model(rn1n2_q, P1, P2)) :-
    Model = rn1n2_q(q_word(_), person(N2), person(N1)),
    handle_prev_pronoun(N1, P1),
    handle_prev_pronoun(N2, P2).    

parse_model(Model, model(num_q, P1, R)) :-
    Model = num_q(q_word(_), relative(R), person(N1)),
    handle_prev_pronoun(N1, P1).


handle_prev_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    sex(P, m), nb_setval(prev_male, P).

handle_prev_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    sex(P, f), nb_setval(prev_female, P).

handle_prev_pronoun(P, N) :-
    member(P, ["he", "his", "him"]),
    nb_getval(prev_male, N).

handle_prev_pronoun(P, N) :-
    member(P, ["she", "her"]),
    nb_getval(prev_female, N).

generate_ans(model(n1r_q, N1, R), Ans) :- 
    relative(R, N1, N2),
    add_of(R, R1),
    append(["The"], R1, First_part),
    append(First_part, ["of", N1, "is", N2], Ans).


generate_ans(model(n2r_q, N2, R), Ans) :-
    relative(R, N1, N2),
    add_of(R, R1),
    append([N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).

generate_ans(model(rn1n2_q, N2, N1), Ans) :-
    relative(R, N1, N2),
    add_of(R, R1),
    append([N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).


generate_ans(model(rn1n2_q, N2, N1), Ans) :-
    not(relative(_, N1, N2)),
    Ans = ["there is relation connections between given persons"].


generate_ans(model(num_q, N1, R), Ans) :-
    setof(N2, relative(R, N1, N2), L),
    length(L, C),
    to_plural(R, R1, C),
    add_of(R1, R2),
    append([N1, "has", C], R2, Ans).


add_of([X], [X]).
add_of([X|T], [X, "of"|T1]) :-
    add_of(T, T1).


to_plural(R, R, 1).
to_plural(R, R1, _) :-
    to_plural(R, R1).

to_plural([], []).
to_plural([Y|T], [Y1|T1]) :-
    not(member(Y, ["father", "mother", "wife", "husband", "motherinlaw"])),
    string_chars(Y, Z),
    append(Z, [s], Z1),
    string_chars(Y1, Z1),
    to_plural(T, T1).
to_plural([X|T], [X|T1]) :-
    member(X, ["father", "mother", "wife", "husband", "motherinlaw"]),
    to_plural(T, T1).


print_res(L) :-
    atomics_to_string(L, ' ', S),
    write(S), nl, !.