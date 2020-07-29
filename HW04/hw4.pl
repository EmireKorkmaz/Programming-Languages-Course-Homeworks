%--- Emire Korkmaz ---%

flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).
flight(istanbul,gaziantep).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

flight(izmir,istanbul).
flight(izmir,isparta).

flight(isparta,izmir).
flight(isparta,burdur).

flight(burdur,isparta).

flight(konya,ankara).
flight(konya,antalya).

flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).

flight(gaziantep,istanbul).
flight(gaziantep,antalya).

flight(ankara,istanbul).
flight(ankara,konya).
flight(ankara,van).

flight(van,rize).
flight(van,ankara).

flight(rize,istanbul).
flight(rize,van).


route(X, Y) :- flight(X, Y), flight(Y, X), format("~a ~a ~n", [X, Y]).

route(X, Y) :- flight(X, Z),
			flight(Z, Y), format("~a ~a ~a~n", [X, Z, Y]).

route(X, Y) :- flight(X, Z),
			flight(Z, T), flight(T, Y), format("~a ~a ~a ~a ~n", [X, Z, T, Y]).

%------2-----%

distance(istanbul,izmir, 328).
distance(istanbul,antalya, 482).
distance(istanbul,ankara, 351).
distance(istanbul,van, 1262).
distance(istanbul,rize, 967).
distance(istanbul,gaziantep, 847).

distance(edirne,edremit, 914).

distance(edremit,edirne, 914).
distance(edremit,erzincan, 736).

distance(erzincan,edremit, 736).

distance(izmir,istanbul, 328).
distance(izmir,isparta, 308).

distance(isparta,izmir, 308).
distance(isparta,burdur, 24).

distance(burdur,isparta, 24).

distance(konya,ankara, 227).
distance(konya,antalya, 192).

distance(antalya,istanbul, 482).
distance(antalya,konya, 192).
distance(antalya,gaziantep, 592).

distance(gaziantep,istanbul, 847).
distance(gaziantep,antalya, 592).

distance(ankara,istanbul, 351).
distance(ankara,konya, 227).
distance(ankara,van, 920).

distance(van,rize, 373).
distance(van,ankara, 920).

distance(rize,istanbul, 967).
distance(rize,van, 373).


sroute(X, Y, C) :- distance(X, Y, C), distance(Y, X, C), format("~a ~a ~n", [X, Y]), C is C.

sroute(X, Y, C) :- distance(X, Z, A),
				  distance(Z, Y, B),format("~a ~a ~a ~n", [X, Z, Y]),
				  (C is A+B).

sroute(X, Y, C) :- distance(X, Z, A), distance(Z, T, B),
				  distance(T, Y, D), format("~a ~a ~a ~a ~n", [X, Z, T, Y]),
				  (C is (A+B+D)).

%----------3-------%

when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).

enrollment(a,102).
enrollment(a,108).
enrollment(b,102).
enrollment(c,108).
enrollment(d,341).
enrollment(e,455).

schedule(S,P,T):- enrollment(S,X), when(X,T), where(X,P).

usage(P,T):- where(X,P), when(X,T).

conflictTime(X,Y):- when(X,TF), when(Y,TS),(TF==TS), write("Due to time").

conflictRoom(X,Y):- where(X,PF), where(Y,PS),(PF==PS), write("Due to classroom").

conflict(X,Y):- conflictTime(X,Y); conflictRoom(X,Y).

meet(X,Y):- enrollment(X,C1), where(C1,P1), enrollment(Y,C2), where(C2,P2), C1==C2,P1==P2,!.

%------4-----%

%----4.1---%
element(E,S) :- member(E,S).

%----4.4---%

equal([H|T], S2).

equivalent(S1,S2) :- length(S1, A), length(S2, B), (A==B), equal(S1,S2).

equal([H|T],S2) :- member(H, S2), equal(T, S2), !.

%---4.2----%

uni(S1,S2,S3) :- append(S1, S2, X), list_to_set(X, Y), write(Y), equivalent(Y, S3). 


%---4.3---%

intersect(S1, S2, []).
intersect(S1, S2, [H|T]) :- member(H, S1), member(H, S2), intersect(S1, S2, T), !.

