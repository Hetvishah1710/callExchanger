%%%-------------------------------------------------------------------
%%% @author Hetvi Shah
%%% @doc
%%% This is the calling module which one will be responsible for
%%% processing requests from each process.
%%% @end
%%%-------------------------------------------------------------------

-module(calling).
-author("Hetvi Shah").

%%%=============================================================================
%%% API
%%%=============================================================================
-export([exchangeCalls/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================
-define(PROCESS_TIMEOUT, 5000).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% This function is used communicate with main func to print the messages
%% @end
%%------------------------------------------------------------------------------
displayMessageProcess() ->
  spawn(exchange, displayMessagesOnMain, []).

%%%=============================================================================
%%% External functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% This function is the receiver of the intro and reply calls from the different
%% processes it would serve the request and according to the request it will
%% send the data to the main process to print the information
%% after 5 seconds of inactivity the process will die and before dying it will
%% send the message to the main process.
%% @end
%%------------------------------------------------------------------------------
exchangeCalls(Name) ->
  receive
    {asking, Sender} ->
      displayMessageProcess() ! {intro, Name, Sender},
      Sender ! {reply, Name},
      exchangeCalls(Name);
    {reply, Sender} ->
      displayMessageProcess() ! {reply, Name, Sender},
      exchangeCalls(Name)
  after ?PROCESS_TIMEOUT ->
    displayMessageProcess() ! {endProcess, Name}
  end.

%%------
%% EOF
%%------
