-module(rabbitbot).
-export([]).

%% NOTE: https://en.wikipedia.org/wiki/Robot
%% NOTE: https://www.rabbitmq.com/

-behaviour(erlmachine_scope).
-behaviour(gen_statem).

-export([scope/0]).

-export([boot/0]).

-export([start/2]).
-export([stop/1]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% Bot
-export([execute/3]).

%% Commands
-export([connect/0, disconnect/0]).
-export([open/0]).
-export([send/0]).

%% App
-export([get_env/2, priv_dir/0, filename/1]).

-export([modules/0]).
-export([vsn/0]).
-export([description/0]).

-include_lib("wire/include/wire.hrl").

-include_lib("erlmachine/include/erlmachine_system.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-type serial_no() :: binary().

-type command() :: { Name::atom(), Timer::integer(), Code::function() }.
-type ram() :: map().

-type filename() :: file:filename().

-type timestamp() :: integer().

%%% erlmachine_scope

-spec scope() -> atom().
scope() ->
    ?MODULE.

%%% API

-spec timestamp() -> integer().
timestamp() ->
    Res = erlang:monotonic_time(second) + erlang:time_offset(second),
    Res.

-spec range(Int::integer()) -> [integer()].
range(Int) ->
    Res = lists:seq(1, Int),
    Res.

-spec boot() -> [serial_no()].
boot() ->
    [ begin MD5 = erlmachine:md5([Serial]),

            Base64 = erlmachine:base64url(MD5),
            SN = <<"SN-", Base64/binary>>,

            start(SN, []),
            SN

      end || Serial <- range(50)
    ].

%%% Bot API

-spec name(SN::serial_no()) -> tuple().
name(SN) ->
    {via, syn, {?MODULE, SN}}.

-spec start(SN::serial_no(), Opt::[term()]) ->
          success(pid()).
start(SN, _Opt) ->
    Name = name(SN),
    Data = data(SN, _Timestamp = timestamp()),

    %%% TODO
    %%

    Commands = [ command(connect, 10)

                 %% command(open, 2 * 1000),
                 %%command(send, 1000)

                 %command(disconnect, 4 * 1000)

               ],
    %%Dbg = [trace, log, statistics],
    Res = gen_statem:start(Name, ?MODULE, _Data = commands(Data, Commands), []),
    Res.

-spec stop(SN::serial_no()) ->
          success().
stop(SN) ->
    Name = name(SN),

    Res = gen_statem:stop(Name),
    Res.

%% gen_statem

-record(data, { serial_no::serial_no(),

                commands::[command()],
                ram::ram(),

                timestamp::timestamp()
              }).

-type data() :: #data{}.

init(Data) ->
    Actions = [action(Command)|| Command <- commands(Data)],

    Res = erlmachine:success(execute, Data, Actions),
    Res.

terminate(_Reason, _State, Data) ->
    io:format("~n~p~n",[timestamp(Data)]),

    ok.

callback_mode() ->
    [state_functions, state_enter].

%%  State machine

execute(enter, _OldState, Data) ->
    {keep_state, Data, []};

execute({timeout, Tag}, Code, Data) ->
    {keep_state, _Data = Code(Data), [schedule(Tag, Data)]};

execute(info, Message, Data) ->
    io:format("~nInfo: ~p~n", [Message]),

    {keep_state, Data, []}.

%%%  Data access

-spec data(SN::serial_no(), Timestamp::integer()) ->
          data().
data(SN, Timestamp) ->
    #data{ serial_no = SN, timestamp = Timestamp,
           ram = #{}

         }.

-spec commands(Data::data()) -> [command()].
commands(Data) ->
    Data#data.commands.

-spec commands(Data::data(), Commands::[command()]) -> data().
commands(Data, Commands) ->
    Data#data{ commands = Commands }.

-spec var(Name::term(), Data::data()) -> term().
var(Name, Data) ->
    Ram = Data#data.ram,

    Var = maps:get(Name, Ram),
    Var.

-spec var(Name::term(), Value::term(), Data::data()) -> data().
var(Name, Value, Data) ->
    Ram = Data#data.ram,

    Data2 = Data#data{ ram = Ram#{ Name => Value } },
    Data2.

-spec timestamp(Data::data()) -> timestamp().
timestamp(Data) ->
    Data#data.timestamp.

%% Env

%% Scheduler API

-spec command(Name::atom(), Timer::integer()) -> command().
command(Name, Timer) ->
    {Name, Timer, _Code = apply(?MODULE, Name, [])}.

-spec schedule(Name::atom(), Data::data()) -> tuple().
schedule(Name, Data) ->
    Command = lists:keyfind(Name, 1, _Commands = commands(Data)),

    Res = action(Command),
    Res.

-spec action(Command::command()) -> tuple().
action(Command) ->
    {Name, Timer, Code} = Command,

    {{ 'timeout', _Tag = Name }, Timer, Code}.

%% Commands

-spec connect() -> function().
connect() ->
    fun (Data) ->

		 try
		 {ok, Pid} = wire:connect(),  Wire = wire:open(Pid),

                  X = <<"rabbitbot">>,
                  T = <<"topic">>,

                  %K = <<"test">>,

                  Method0 = #'exchange.declare'{ exchange = X, type = T },
		  catch wire:call(Wire, Method0),

                  %Headers = [    ],
                  %Data = <<"test">>,

                  %Signal0 = wire:signal(Headers, Data),
                  %Signal1 = wire:content_type(Signal0, <<"text/plain">>),

                  %Method1 = #'basic.publish'{ 'exchange' = X, 'routing_key' = K },
                  %wire:cast(Wire, Method1, Signal1),

		  catch wire:disconnect(Pid),

                  Data2 = var(connection, Pid, Data),
                  Data2
		  catch _:E:_ ->
			       io:format("Error~p~n", [E]),
				Data
		 end
    end.

-spec open() -> function().
open() ->
    fun (Data) -> Pid = var(connection, Data),
                  wire:open(Pid),

                  Data
    end.

-spec send() -> function().
send() ->
    fun (Data) -> Data
    end.

-spec disconnect() -> function().
disconnect() ->
    fun (Data) -> Data
    end.


%%% Application API

-spec get_env(Par::atom(), Def::term()) -> term().
get_env(Par, Def) ->
    application:get_env(?MODULE, Par, Def).

-spec get_key(Key::atom()) -> 'undefined' | success(term()).
get_key(Key) ->
    application:get_key(?MODULE, Key).

-spec priv_dir() -> filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec filename(Path::filename()) -> filename().
filename(Path) ->
    filename:join(priv_dir(), Path).

-spec modules() -> [module()].
modules() ->
    {ok, Modules} = get_key('modules'), true = is_list(Modules),

    Modules.

-spec vsn() -> binary().
vsn() ->
    {ok, Vsn} = get_key('vsn'), true = is_binary(Vsn),

    Vsn.

-spec description() ->  binary().
description() ->
    {ok, Desc} = get_key('description'), true = is_binary(Desc),

    Desc.
