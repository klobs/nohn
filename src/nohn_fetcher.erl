-module(nohn_fetcher).

-behaviour(gen_server).

%% API
-export([start_link/0, print_status/0, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {last_update,
				hn_list=[], 
				item_store=#{}
			}
		).

-define(SERVER, ?MODULE).
-define(FETCHINTERVAL, 300000). %% Fetch news every 5 minutes
%%-define(FETCHINTERVAL, 60000). %% Fetch news every minute
-define(HNURL, "https://hacker-news.firebaseio.com/v0/topstories.json").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

print_status() ->
  gen_server:call(?SERVER, print_status_report).

get_status() ->
  gen_server:call(?SERVER, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  inets:start(),
  ssl:start(),
  {ok, NState} = refresh(#state{}),
  {ok, NState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(get_status, _From, State) ->
  {reply, {ok, State}, State};

handle_call(print_status_report, _From, State) ->
  print_status_report(State),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({remove_item, Item}, State) when is_integer(Item), is_record(State, state) ->
  case  maps:find(Item, State#state.item_store) of
	  error ->
		  {noreply, State};
	  _ -> 
		  io:format("removing item ~p~n",[Item]),
		  Updated_item_store = maps:remove(Item, State#state.item_store),
		  {noreply, State#state{item_store = Updated_item_store}}
  end;

handle_cast({update_item, Item}, State) when is_integer(Item), is_record(State, state) ->
  case  maps:find(Item, State#state.item_store) of
	  error -> 
		  {ok, ItemValue} = get_item(Item),
		  io:format("updated item ~p with content~n~p~n",[Item, ItemValue]),
		  Updated_item_store = maps:put(Item, ItemValue, State#state.item_store),
		  {noreply, State#state{item_store = Updated_item_store}};
	  _ ->
		  {noreply, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(refresh, State) when is_record(State, state) ->
	io:format("refreshing...~n"),
	HNUpdate = refresh(State),
	case HNUpdate of
		{ok, NState} -> 
			{noreply, NState};
		_ -> {noreply, State}
	end;
handle_info(Info, State) ->
	io:format("I received a wired info here: ~p~nState: ~p~n",[Info, State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_item(ItenNo) ->
	{ok, {_,_, ItemJSON}} = httpc:request("https://hacker-news.firebaseio.com/v0/item/" ++ io_lib:format("~p",[ItenNo]) ++ ".json"),
	Item = jiffy:decode(ItemJSON, [return_maps]),
	{ok, Item}.

refresh(State) when is_record(State, state) ->
  Now = os:system_time(),
  {ok, {_,_, HNListJSON}} = httpc:request(?HNURL),
  HNList = lists:sublist(jiffy:decode(HNListJSON),30),
  remove_old_entries(State#state.hn_list -- HNList),
  update_items(HNList),
  NState  = State#state{last_update = Now, hn_list = HNList},
  erlang:send_after(?FETCHINTERVAL, self(), refresh),
  {ok, NState};

refresh(State) ->
	io:format("refresh(): state kaputt: ~p~n",[State]),
	erlang:send_after(?FETCHINTERVAL, self(), refresh),
	{ok, #state{}}.

remove_old_entries([]) ->
	nothing_to_remove;
remove_old_entries([Item | RemovableItems]) ->
	gen_server:cast(?SERVER, {remove_item, Item}),
	remove_old_entries(RemovableItems).

print_status_report(State) when is_record(State, state) ->
	io:format("Status: ~p~n",[State#state.last_update]),
	io:format("Items in store: ~p~n",[State#state.item_store]),
	ok;	
print_status_report(State) ->
	io:format("Status seems to be broked: ~p~n",[State]),
	ok.

update_items([]) ->
		up2date;
update_items([HNListEntry| HNList]) ->
	gen_server:cast(?SERVER, {update_item, HNListEntry}),
	update_items(HNList).
