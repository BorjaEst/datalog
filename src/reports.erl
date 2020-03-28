%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(reports).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([progress_bar/2]).

-type progress_bar() :: #{
    level := number(),
    size  := integer()
}.


-define(DEFAULT_BAR_SIZE, 10).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Prints a list of data into an progress bar.
%% @end
%%--------------------------------------------------------------------
-spec progress_bar(ListOfValues, NumberOfLines) -> Print when
    ListOfValues  :: [number()],
    NumberOfLines :: integer(),
    Print         :: io_lib:chars().
progress_bar(ListOfValues, NumberOfLines) ->
    
    io_lib:format("~0p", [1]).





-spec progress_bar(BarProperties) -> Print when
    BarProperties :: progress_bar(),
    Print         :: io_lib:chars().
progress_bar(BarProperties) -> 
    BarSize  = maps:get(size,  BarProperties, ?DEFAULT_BAR_SIZE),
    BarLevel = maps:get(level, BarProperties),
    Progress = ceil((BarSize*1.01) * BarLevel) -1,
    if
        Progress ==         -1               -> 
            Format = empty(BarSize);
        Progress == BarSize                -> 
            Format = complete(BarSize);
        Progress > -1, Progress < BarSize +1 -> 
            Format = between(Progress, BarSize)
    end,
    io_lib:format(Format, [$=,$.]).

progress_bar_test() -> 
    Bar1 = #{level => 0.00, size => 10},
    ?assertEqual("[..........]", progress_bar(Bar1)),
    Bar2 = #{level => 0.01, size => 10},
    ?assertEqual("[>.........]", progress_bar(Bar2)),
    Bar3 = #{level => 0.24, size => 10},
    ?assertEqual("[==>.......]", progress_bar(Bar3)),
    Bar4 = #{level => 0.72, size => 10},
    ?assertEqual("[=======>..]", progress_bar(Bar4)),
    Bar5 = #{level => 0.99, size => 10},
    ?assertEqual("[=========>]", progress_bar(Bar5)),
    Bar6 = #{level => 1.00, size => 10},
    ?assertEqual("[==========]", progress_bar(Bar6)),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

empty(Size) -> 
    [$[,$~,$0,$c,$~] ++ integer_to_list(Size) ++ [$c,$]].

complete(Size) -> 
    [$[,$~] ++ integer_to_list(Size) ++ [$c,$~,$0,$c,$]].

between(Progress, Size) -> 
    [$[,$~] ++ integer_to_list(Progress) ++ [$c,$>,$~] ++ 
    integer_to_list(Size - Progress - 1) ++ [$c,$]].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

