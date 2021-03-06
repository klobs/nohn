-module(nohn_handler).
-behavior(cowboy_handler).

-export([init/2]).

-record(nohn_fetcher_state, {last_update,
				hn_list=[], 
				item_store=#{}
			}
		).

init(Req0, State) ->
	Cookies = cowboy_req:parse_cookies(Req0),
	T1 = case  lists:keyfind(<<"last_visit">>, 1, Cookies) of
		false -> 0;
		{_, T} -> binary_to_list(T)
	end,
	LastVisitCookie = case T1 of
		_ when is_integer(T1) -> T1;
		[] -> 0;
		_ when is_list(T1) -> list_to_integer(T1)
	end,
	Req1 = cowboy_req:set_resp_cookie(<<"last_visit">>, integer_to_list(os:system_time()), Req0),
    Req = cowboy_req:reply(200,
        #{
			<<"content-type">> => <<"text/html; charset=UTF-8">>
		},
		gen_nohn_table(LastVisitCookie),
        Req1),
    {ok, Req, State}.


gen_nohn_table(LastVisitCookie) ->
	LastVisitCookie,
	{ok, HNStatus} = nohn_fetcher:get_status(),
	LastUpdate = integer_to_list((os:system_time() - HNStatus#nohn_fetcher_state.last_update) div (60*60*100000)),
	[<<"<!DOCTYPE html><html>">>,
			<<"<head>\n<style>\n">>,
			<<".hidden {opacity: 0.2;}\n">>,
			<<".top_scorer {opacity: 0.8; background-color: yellow;}\n">>,
			<<".tooltip {position: relative; display: inline-block;}\n">>,
			<<".tooltip .tooltiptext {visibility: hidden; width: 400px; background-color: black; color: #fff; text-align: center; padding: 5px 0; border-radius: 6px; position: absolute; z-index: 1;}\n">>,
			<<".tooltip:hover .tooltiptext {visibility: visible;}\n">>,
			<<"</style>\n">>,
			<<"<script>\nfunction reset(){ document.cookie = \"last_visit=; expires=Thu, 01 Jan 1970 00:00:01 GMT\"; location.reload();}\n</script>\n">>,
			<<"<title>No old hacker news!</title>">>,
			<<"</head>">>,
		<<"<body>\n<h1 class=\"tooltip\">No old hacker news!<span class=\"tooltiptext\">Heavy hackernews consumer? Have a peek every few minutes for new top stories and not interrested in old ones? This service greys old ones out and so you can focus on the latest shit.</span></h1>\n">>,
		<<"<table>\n">>,		
		gen_nohn_table(1, HNStatus#nohn_fetcher_state.hn_list, LastVisitCookie, HNStatus),
		<<"</table><p>Last update ">>, LastUpdate,<<" seconds ago. <a href=\"javascript:reset();\">Reset</></p></body></html>\n">>
		].

gen_nohn_table(_ItenNo, [], _LastVisitCookie, _HNStatus) ->
	<<"">>;
gen_nohn_table(ItenNo, [CurrentItemNo | HNList], LastVisitCookie, HNStatus) ->
	Item = maps:get(CurrentItemNo, HNStatus#nohn_fetcher_state.item_store, #{}),
	ItemTitle =  maps:get(<<"title">>, Item, <<"Not yet in itemstore. Try refreshing.">>),
	ItemURL =  maps:get(<<"url">>, Item, <<"Not yet in itemstore. Try refreshing.">>),
	ItemLink = [<<"<a href=\"">>,ItemURL,<<"\">">>, ItemTitle, <<"</a>">>],
	ItemScore = maps:get(<<"score">>, Item, <<"Not yet in itemstore. Try refreshing.">>),
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
