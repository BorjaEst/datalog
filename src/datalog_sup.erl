%%%-------------------------------------------------------------------
%% @doc datalog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(datalog_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(SPECS_DATALOG, #{
    id       => datalog,
    start    => {datalog, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_server]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        ?SPECS_DATALOG
    ],
    {ok, {SupFlags, ChildSpecs}}.

