name(alekseev).
name(borisov).
name(konstantinov).
name(dmitriev).

profession(astronom).
profession(poet).
profession(prozaik).
profession(dramaturg).


no_repetitions([]):-!.
no_repetitions([Head|Tail]):-
   not(member(Head, Tail)),
   no_repetitions(Tail).


no_reading_own_book([]):-!.
no_reading_own_book([passenger(_, XRead, XBuy, XWrite)|T]):-
  no_reading_own_book(T),!,XRead\=XWrite,XBuy\=XWrite.


solve():-
  Listofpassangers = [passenger(X, XRead, XBuy, XWrite), passenger(Y, YRead, YBuy, YWrite),
          passenger(Z, ZRead, ZBuy, ZWrite), passenger(L, LRead, LBuy, LWrite)],
  name(X), name(Y), name(Z), name(L), no_repetitions([X, Y, Z, L]),
  profession(XWrite), profession(YWrite),
  profession(ZWrite), profession(LWrite),
  no_repetitions([XWrite, YWrite, ZWrite, LWrite]),
  profession(XBuy), profession(YBuy),
  profession(ZBuy), profession(LBuy),
  no_repetitions([XBuy, YBuy, ZBuy, LBuy]),
  profession(XRead), profession(YRead),
  profession(ZRead), profession(LRead),
  no_repetitions([XRead, YRead, ZRead, LRead]),
  no_reading_own_book(Listofpassangers),
  member(passenger(_, dramaturg, _, poet), Listofpassangers),
  not(member(passenger(_, astronom, _, prozaik), Listofpassangers)),
  not(member(passenger(_, _, astronom, prozaik), Listofpassangers)),
  not(member(passenger(dmitriev, _, _, prozaik), Listofpassangers)),
  member(passenger(alekseev, AlekseevRead, AlekseevBuy, _), Listofpassangers),
  member(passenger(borisov, AlekseevBuy, AlekseevRead, _), Listofpassangers),
  member(passenger(dmitriev, _, _, DmitrievWrite), Listofpassangers),!,
  member(passenger(borisov, DmitrievWrite, _, _), Listofpassangers),
  write(X),write(" is "),write(XWrite),write(" and was reading book of "),write(XRead),nl,
  write(Y),write(" is "),write(YWrite),write(" and was reading book of "),write(YRead),nl,
  write(Z),write(" is "),write(ZWrite),write(" and was reading book of "),write(ZRead),nl,
  write(L),write(" is "),write(LWrite),write(" and was reading book of "),write(LRead),nl.
