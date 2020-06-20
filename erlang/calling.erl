%%%-------------------------------------------------------------------
%%% @author Hetvi Shah
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2020 8:22 PM
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
    {asking, Sender, Time} ->
      displayMessageProcess() ! {intro, Name, Sender, Time},
      Sender ! {reply, Name, Time},
      exchangeCalls(Name);
    {reply, Sender, Time} ->
      displayMessageProcess() ! {reply, Name, Sender, Time},
      exchangeCalls(Name)
  after ?PROCESS_TIMEOUT ->
    displayMessageProcess() ! {endProcess, Name}
  end.

%%------
%% EOF
%%------