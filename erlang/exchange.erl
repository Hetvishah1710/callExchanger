%%%-------------------------------------------------------------------
%%% @author Hetvi Shah
%%% @doc
%%%
%%% This is the main file of the program which would register the
%%% processes, and those processes would talk with each other to send
%%% and get replies from each other.
%%% If at the end there is no reply from any processes for 10 seconds
%%% program will timeout and terminate itself.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(exchange).
-author("Hetvi Shah").

%%%=============================================================================
%%% API
%%%=============================================================================
-export([start/0, displayMessagesOnMain/0]).

%%%=============================================================================
%%% Macros
%%%=============================================================================
-define(MAIN_TIMEOUT, 10000).
-define(RANDOM_SLEEP_MAX, 100).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% This function is used to spawn process from the calling module
%% @end
%%------------------------------------------------------------------------------
startProcess(Module, ProcessName, Name) ->
    spawn(Module, ProcessName, [Name]).

%%------------------------------------------------------------------------------
%% @doc
%% This function will take the list, and it will send the request to each of the
%% sender it should send defined in the call.txt
%% suppose jill has [joe, john, jill] then it will send the request from
%% registered process name of jill to other three.
%% @end
%%------------------------------------------------------------------------------
exchangeStart(C) ->
  [[element(1, X) ! {asking, Y} || Y <- element(2, X)] || X <- C].


%%------------------------------------------------------------------------------
%% @doc
%% This is the function used by main to display the messages
%% if no message is received within 10 seconds it will terminate the main process
%% @end
%%------------------------------------------------------------------------------
displayMessagesOnMain() ->
  timer:sleep(rand:uniform(?RANDOM_SLEEP_MAX)),
  receive
    {intro, Sender, Receiver} ->
      io:format("~p recived intro message from ~p [~p] ~n", [Sender, Receiver, element(3, erlang:now())]),
      displayMessagesOnMain();
    {reply, Sender, Receiver} ->
      io:format("~p recived reply message from ~p [~p] ~n", [Sender, Receiver, element(3, erlang:now())]),
      displayMessagesOnMain();
    {endProcess, Processname} ->
      io:format("~nProcess ~p has recieved no calls for 5 seconds, ending ... ~n", [Processname]),
      displayMessagesOnMain()
  after ?MAIN_TIMEOUT ->
    io:format("~nMaster has received no replies for 10 seconds, ending ... ~n"),
    erlang:halt()
  end.

%%------------------------------------------------------------------------------
%% @doc
%% This is start point of the program.
%% Function will read data from calls.txt and then will print out data first
%% then it will register processes with the names associated with it and
%% call the exchangeStart to initiate process and hand over to the
%% @end
%%------------------------------------------------------------------------------
start() ->
  {ok, C} = file:consult("./calls.txt"),
  io:format(" ** Calls to be made ** ~n"),
  [io:format("~p: ~p ~n", [element(1,X),element(2,X)]) || X <- C],
  [register(element(1, X), startProcess(calling, exchangeCalls, element(1, X))) || X <- C],
  exchangeStart(C),
  displayMessagesOnMain().

%%------
%% EOF
%%------
