-module(nohn_handler).
-behavior(cowboy_handler).

-export([init/2]).

-record(state, {last_update,
				hn_list=[], 
				item_store=#{}
			}
		).

init(Req0, State) ->
	Cookies = cowboy_req:parse_cookies(Req0),
	Token = case  lists:keyfind(<<"last_visit">>, 1, Cookies) of
		false -> <<"">>;
		{_, T} -> T
	end,
	io:format("~p~n",[Token]),
	Req1 = cowboy_req:set_resp_cookie(<<"last_visit">>, integer_to_list(os:system_time()), Req0),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
		gen_nohn_table(Cookies),
        Req1),
    {ok, Req, State}.


gen_nohn_table(Cookie) ->
	Cookie,
	{ok, HNStatus} = nohn_fetcher:get_status(),
	[<<"<!DOCTYPE html><html><body><h1>No old hacker news!</h1>\n">>,
		<<"<table>\n">>,		
		gen_nohn_table(1, HNStatus#state.hn_list, HNStatus),
		<<"</table></body></html>\n">>	
		].

gen_nohn_table(_ItenNo, [], _HNStatus) ->
	<<"">>;
gen_nohn_table(ItenNo, [CurrentItemNo | HNList], HNStatus) ->
	Item = maps:get(CurrentItemNo, HNStatus#state.item_store, #{}),
	ItemTitle =  maps:get(<<"title">>, Item, <<"Not yet in itemstore">>),
	ItemURL =  maps:get(<<"url">>, Item, <<"Not yet in itemstore">>),
	ItemLink = [<<"<a href=\"">>,ItemURL,<<"\">">>, ItemTitle, <<"</a>">>],
	ItemScore = maps:get(<<"score">>, Item, <<"Not yet in itemstore">>),
	[<<"<tr>">>,
			<<"<td>">>, list_to_binary(integer_to_list(ItenNo)), <<".</td>">>,
			<<"<td>">>, ItemLink, <<"</td>">>,
			<<"<td>Score: ">>, integer_to_list(ItemScore), <<"</td>">>,
		<<"</tr>">>
		] 
		++ gen_nohn_table(ItenNo + 1, HNList, HNStatus).
