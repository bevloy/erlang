%--------------------------------------------------------------------------------------
%Module Details
%--------------------------------------------------------------------------------------

-module(revision).
-export([int/1, isEven/1, factorial/1, range/2, altRange/2, pieces/1, maxThree/3, 
	equal/3, stars/1, for/1, loopStars/1, doubleAll/1, evens/1, append/2, index/2, 
	merge/2, mergeSort/1, buildList/2, maxList/1, member/2, greater/2, for2/2,
	loopBuild/2, square/1, rectangle/2, rightAngled/2, rightAngled2/2, circle/1,
	hexagon/1, octagon/1, pentagon/1]).

%--------------------------------------------------------------------------------------
%General Functions - mostly using pattern matching and/or recursion.
%--------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------
%Pattern matching example.

int(0) -> 
	10;

int(N) ->
	20.

%--------------------------------------------------------------------------------------
%Function to determine whether given number is even.

isEven(X) when X rem 2 == 0 ->
	true;

isEven(X) ->
	false.

%--------------------------------------------------------------------------------------
%Using recursion to form a factorial function.

factorial(0) ->
	1;

factorial(X) ->
	factorial(X-1) * X.

%--------------------------------------------------------------------------------------
%Finding product of all numbers in a range.

range(X,Y) when X > Y ->
	1;

range(X,Y) ->
	X * range(X-1,Y).

%--------------------------------------------------------------------------------------
%Finding product of all numbers in a range using call to factorial.

altRange(X,Y) when X < Y ->
	factorial(Y) / factorial(X-1);

altRange(X,Y) ->
	factorial(X) / factorial(Y-1).

%--------------------------------------------------------------------------------------
%Finding max pieces from N cuts.

pieces(0) ->
	1;

pieces(N) ->
	pieces(N-1) + N.

%--------------------------------------------------------------------------------------
%Finding the maximum of three numbers.

maxThree(X,Y,Z) ->
	max(max(X,Y),Z).

%--------------------------------------------------------------------------------------
%Finding how many numbers are equal.

equal(X,X,X) -> 3;
equal(X,X,Y) -> 2;
equal(X,Y,X) -> 2;
equal(Y,X,X) -> 2;
equal(_,_,_) -> 0.

%--------------------------------------------------------------------------------------
%Prints stars in a row equal to given number.

stars(0) -> 
	"\n";

stars(N) when N > 0 ->
	"*" ++ stars(N-1);

stars(N) ->
	io:format(stars(N),[]).

%--------------------------------------------------------------------------------------
%As above using a function like a loop.

for(0) ->
	ok;

for(N) when N > 0 ->
	io:fwrite("*~n"),
	for(N-1).

loopStars(X) ->
	for(X).

%--------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------
%Lists
%--------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------
%Double all the values in a list of integers.

doubleAll([]) ->
	[];

doubleAll([X|Xs]) ->
	[2*X|doubleAll(Xs)].

%--------------------------------------------------------------------------------------
%Selects all the even values in a list of integers.

evens([]) ->
	[];

evens([X|Xs]) when X rem 2 == 0 ->
	[X|evens(Xs)];

evens([_|Xs]) ->
	evens(Xs).

%--------------------------------------------------------------------------------------
%Joins two lists together.

append([],Ys) ->
	Ys;

append([X|Xs], Ys) ->
	[X|append(Xs,Ys)].

%--------------------------------------------------------------------------------------
%Indexes a given list.

index(0,[X|Xs]) ->
	X;

index(N,[X|Xs]) when N > 0 ->
	index(N-1, Xs).

%--------------------------------------------------------------------------------------
%Merges ordered lists.

merge([],Ys) ->
	Ys;

merge(Xs,[]) ->
	Xs;

merge([X|Xs],[Y|Ys]) when X =< Y ->
	[X|merge(Xs, [Y|Ys])];

merge([X|Xs],[Y|Ys]) ->
	[Y| merge([X|Xs],Ys)].

%--------------------------------------------------------------------------------------
%Sorts a given list.

mergeSort([]) ->
	[];

mergeSort([X]) ->
	[X];

mergeSort([Xs]) ->	
	{Ys,Zs} = lists:split(length(Xs) div 2, Xs),
	Sorted_Ys = mergeSort(Ys),
	Sorted_Zs = mergeSort(Zs),
	merge(Sorted_Ys,Sorted_Zs).

%--------------------------------------------------------------------------------------
%Builds a list between two numbers.

buildList(X,Y) when X > Y ->
	[];

buildList(X,Y) ->
	[X|buildList(X+1,Y)].

%--------------------------------------------------------------------------------------
%Finds maximum in a list.

maxList([]) ->
	0;

maxList([X]) ->
	X;

maxList([X|Xs]) ->
	max(X,maxList(Xs)).

%--------------------------------------------------------------------------------------
%Checks whethere integer is element of a list.

member(X,[]) ->
	false;

member(X,[X|Xs]) -> 
	true;

member(X,[Y|Ys]) ->
	member(X,Ys).

%--------------------------------------------------------------------------------------
%Returns values in list greater than given number.

greater(X,[]) -> 
	[];

greater(X,[Y|Xs]) when X < Y ->
	[Y|greater(X,Ys)];

greater(X,[Y|Xs]) ->
	greater(X,Ys).

%--------------------------------------------------------------------------------------
%Building using a loop.

for2(X,Y) when X == Y ->
	[X];

for2(X,Y) when X < Y ->
	[X|for2(X+1,Y)].

loopBuild(M,N) ->
	for2(M,N).

%--------------------------------------------------------------------------------------
%As above counting down. 

for3(X,Y) when X == Y ->
	[X];

for3(X,Y) when X > Y ->
	[X|for3(X-1, Y)].

loopBuildDown(M,N) ->
	for3(M,N).

%--------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------
%Area of shape functions - Syntax Practice.
%--------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------
%Find area of square with side N.

square(N) ->
	N*N.

%--------------------------------------------------------------------------------------
%Find area of rectangle with sides N,M.

rectangle(N,M) ->
	N*M.

%--------------------------------------------------------------------------------------
%Find area of right-angled triangle with height N and length M.

rightAngled(N,M) ->
	(N*M)/2.

%--------------------------------------------------------------------------------------
%As above using call to rectangle function.

rightAngled2(N,M) ->
	rectangle(N,M)/2.

%--------------------------------------------------------------------------------------
%Area of circle with radius X using estimation of 3.14 for pi.

circle(X) ->
	3.14*(X*X).

%--------------------------------------------------------------------------------------
%Area of a regular hexagon with side X.

hexagon(X) ->
	(((math:sqrt(3))*3)/2)*(X*X).

%--------------------------------------------------------------------------------------
%Area of regular octagon with side X.

octagon(X) ->
	2*(1+(math:sqrt(2)))*(X*X).

%--------------------------------------------------------------------------------------
%Area of regular pentagon with side X.

pentagon(X) ->
	(1/4)*(math:sqrt(5*(5+(2*(math:sqrt(5))))))*(X*X).

%--------------------------------------------------------------------------------------
