%Predicate A

assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
	free_schedule(AllTAs,TeachingSchedule,FreeSchedule),
	assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).

%Predicate B

free_schedule(ALLTAs,TeachingSchedule,FreeSchedule):- 
	TeachingSchedule=[SAT,SUN,MON,TUE,WED,THUR],
	FreeSchedule=[day(sat,R1),day(sun,R2),day(mon,R3),day(tue,R4),day(wed,R5),day(thu,R6)],
	free(ALLTAs,SAT,R1),
	free(ALLTAs,SUN,R2),
	free(ALLTAs,MON,R3),
	free(ALLTAs,TUE,R4),
	free(ALLTAs,WED,R5),
	free(ALLTAs,THUR,R6).
	
free(AllTAs,Day,R):-
	free_helper(AllTAs,Day,[[],[],[],[],[]],L),
	L=[S1,S2,S3,S4,S5],
	permutation(S1,P1),
	permutation(S2,P2),
	permutation(S3,P3),
	permutation(S4,P4),
	permutation(S5,P5),
	R=[P1,P2,P3,P4,P5].
free_helper([],_,AC,AC).
free_helper([H1|T1],Day,AC,R):-
	taFree(H1,Day,Result),
	Result=[S1,S2,S3,S4,S5],
	AC=[S11,S21,S31,S41,S51],
	append(S11,S1,Slot1),
	append(S21,S2,Slot2),
	append(S31,S3,Slot3),
	append(S41,S4,Slot4),
	append(S51,S5,Slot5),
	NewAC=[Slot1,Slot2,Slot3,Slot4,Slot5],
	free_helper(T1,Day,NewAC,R).
	

taFree(ta(_,DayOff),day(DayName,_),L2):-
	DayOff=DayName,
	L2=[[],[],[],[],[]].

taFree(ta(Name,DayOff),day(DayName,[H1|T1]),[H2|T2]):-
	DayOff\=DayName,
	member(Name,H1),
	H2=[],
	taFree(ta(Name,DayOff),day(DayName,T1),T2).

taFree(ta(Name,DayOff),day(DayName,[H1|T1]),[H2|T2]):-
	DayOff\=DayName,
	\+member(Name,H1),
	H2=[Name],
	taFree(ta(Name,DayOff),day(DayName,T1),T2).
	
taFree(ta(_,_),day(_,[]),[]).

%Predicate C

assign_quizzes([],_,[]).
assign_quizzes([H1|T1], FreeSchedule, ProctoringSchedule) :- helper([H1|T1], FreeSchedule,[], ProctoringSchedule).

helper([],_,Acc,Acc).
helper([H1|T1], FreeSchedule,Acc ,ProctoringSchedule) :- assign_quiz(H1,FreeSchedule,PS1),
														append(Acc,[proctors(H1,PS1)],Acc1),
														helper(T1,FreeSchedule,Acc1,ProctoringSchedule).
	
%Predicate D

assign_quiz(quiz(_,Day,Slot,Number),FreeSchedule,AssignedTAs):- 
	findDay(Day,FreeSchedule,Match),
	nth1(Slot,Match,List),
	assign(List,Number,AssignedTAs).
findDay(Day,[H|_],Match):-
	H=day(Day,Match).
findDay(Day,[H|T],Match):-
	H=day(X,_),
	X\=Day,
	findDay(Day,T,Match).
assign(_, 0, []).
assign(L1, Number, [X|L2]) :-
    Number > 0,
    select(X, L1, Remainder),       
    Number1 is Number - 1,
    assign(Remainder, Number1, L2), 
    \+ member(X, L2).
	