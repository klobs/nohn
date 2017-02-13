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
	LastVisitCookie = case  lists:keyfind(<<"last_visit">>, 1, Cookies) of
		false -> 0;
		{_, T} -> list_to_integer(binary_to_list(T))
	end,
	Req1 = cowboy_req:set_resp_cookie(<<"last_visit">>, integer_to_list(os:system_time()), Req0),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
		gen_nohn_table(LastVisitCookie),
        Req1),
    {ok, Req, State}.


gen_nohn_table(LastVisitCookie) ->
	LastVisitCookie,
	{ok, HNStatus} = nohn_fetcher:get_status(),
	LastUpdate = integer_to_list((os:system_time() - HNStatus#state.last_update) div (60*60*100000)),
	[<<"<!DOCTYPE html><html>">>,
			<<"<head>\n<style>\n">>,
			<<".hidden {opacity: 0.2;}\n">>,
			<<".top_scorer {opacity: 0.8;}\n">>,
			<<"</style></head>">>,
		<<"<body>\n<h1>No old hacker news!</h1>\n">>,
		<<"<table>\n">>,		
		gen_nohn_table(1, HNStatus#state.hn_list, LastVisitCookie, HNStatus),
		<<"</table><p>Last update ">>, LastUpdate,<<" seconds ago.</p></body></html>\n">>
		].

gen_nohn_table(_ItenNo, [], _LastVisitCookie, _HNStatus) ->
	<<"">>;
gen_nohn_table(ItenNo, [CurrentItemNo | HNList], LastVisitCookie, HNStatus) ->
	Item = maps:get(CurrentItemNo, HNStatus#state.item_store, #{}),
	ItemTitle =  maps:get(<<"title">>, Item, <<"Not yet in itemstore">>),
	ItemURL =  maps:get(<<"url">>, Item, <<"Not yet in itemstore">>),
	ItemLink = [<<"<a href=\"">>,ItemURL,<<"\">">>, ItemTitle, <<"</a>">>],
	ItemScore = maps:get(<<"score">>, Item, <<"Not yet in itemstore">>),
	ItemClass = case LastVisitCookie > maps:get(nohn_timestamp, Item, false) of
		true -> <<"hidden">>;
		false -> <<"shown">>
	end,
	TopScorerClass = get_top_scorer(maps:get(<<"score">>, Item, 0)),
	[<<"<tr class=\"">>,ItemClass, TopScorerClass,<<"\">">>,
			<<"<td>">>, list_to_binary(integer_to_list(ItenNo)), <<".</td>">>,
			<<"<td>">>, ItemLink, <<"</td>">>,
			<<"<td>Score: ">>, integer_to_list(ItemScore), <<"</td>">>,
		<<"</tr>">>
		] 
		++ gen_nohn_table(ItenNo + 1, HNList, LastVisitCookie, HNStatus).

get_top_scorer(Score) ->
	case Score < 200 of
		true ->	<<"">>;
		false -> <<" top_scorer">>
	end.
