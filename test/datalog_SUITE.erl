%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(datalog_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info datalog: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error datalog: ~p", [Error])).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
	[{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(start_and_stop, Config) ->
	Config;
init_per_group(_GroupName, Config) ->
    datalog:start(),
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(start_and_stop, _Config) ->
	ok;
end_per_group(_GroupName, _Config) ->
	exit(whereis(datalog), shutdown),
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
	[
		{start_and_stop, [sequence],
		 [correct_start_and_stop]
		 },
		{make_datalogs, [parallel],
		 [random_datalog || _ <- lists:seq(1,10)]
		}
	].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
	[
		{group, start_and_stop},
		{group, make_datalogs}
	].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
	[].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
	ok.

% --------------------------------------------------------------------
% COMMON TESTS -------------------------------------------------------
% ....................................................................
correct_start_and_stop() ->
	[].
correct_start_and_stop(_Config) ->
	datalog:start_link(),
	DatalogPid = whereis(datalog),
	true = is_pid(DatalogPid),
	true = is_process_alive(DatalogPid),
	exit(DatalogPid, normal),
	timer:sleep(10),
	false = is_process_alive(DatalogPid),
	ok.

% ......................................................................................................................
random_datalog() ->
	[].
random_datalog(_Config) -> 
	[write_datalog() || _<- lists:seq(1, rand:uniform(10))],
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

unique_filename() ->
	FileIndex = erlang:unique_integer([positive]),
	"datalog_" ++ integer_to_list(FileIndex) ++ ".json".

sample_map() ->
	#{
		<<"int">> => rand:uniform(100),
		<<"float">> => rand:normal(),
		<<"text">> => <<"Some text">>
	}.

write_datalog() -> 
	{ok, Ref} = datalog:new(unique_filename()),
	[datalog:write(Ref, sample_map()) || _<- lists:seq(1, rand:uniform(10))],
	ok = datalog:close(Ref).
