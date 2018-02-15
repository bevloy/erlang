-module(assignment1).
-export([m1/1]).

%--------------------------------------------------------
% Question 1 - Hand Evalution.
%--------------------------------------------------------

m1([]) ->
  1;
  
m1([X|Xs]) when X rem 2 == 1 ->
  X * m1(Xs);
  
m1([_|Xs]) ->
  m1(Xs).
  
% Hand Evaluate m1([2,3,5]).
% m1([2,3,5])
% m1([3,5])
% 3 * (m1([5])) 
% 3 * (5 * (m1([]))) 
% 3 * 5 * 1
% 15

% This function takes a list as an argument and returns
% the product of all the odd numbers in that list.


%--------------------------------------------------------
% Question 2 - Syntax
%--------------------------------------------------------

% It should say 'when' not 'if' for the condtion.
% There should be a semicolon instead of the comma.
% The function only includes one of the numbers.
