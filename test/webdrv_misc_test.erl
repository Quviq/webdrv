%%% @author Hans Svensson <>
%%% @copyright (C) 2012, Hans Svensson
%%% @doc
%%% Webdriver implementation in Erlang
%%% @end
%%% Created : 20 Dec 2012 by Hans Svensson <>

-module(webdrv_misc_test).

-compile(export_all).
-include("../include/webdrv.hrl").
-define(SELENIUM, "http://localhost:4444/wd/hub/").
-define(CHROMEDRIVER, "http://localhost:9515/").

relative_mess() ->
  {ok, _Pid} = webdrv_session:start_session(test, ?SELENIUM, webdrv_cap:default_htmlunit(), 10000),
  webdrv_session:set_url(test, "http://localhost:8088/elements.html"),
  ElemSelects = [ X || {X, _} <- all_elements() ],
  ElemSelects2 = [ X || {X, _} <- all_elements2() ],
  %% Test that we can really find all of these
  Elems = lists:map(fun({St, V}) -> {ok, E} = webdrv_session:find_element(test, St, V), E end,
                    ElemSelects),
  io:format("Elems: ~p\n", [lists:zip(ElemSelects, Elems)]),
  Res = [ {E1, E2, find_rel(E,E2)} || {E, E1} <- lists:zip(Elems, ElemSelects),
                                      E2 <- ElemSelects2],
  io:format("\nRes:\n~p\n", [Res]),
  webdrv_session:stop_session(test).

find_rel(E, {St, V}) ->
  case webdrv_session:find_element_rel(test, E, St, V) of
    {ok, _} -> io:format("."), ok;
    _ ->   io:format("x"), fail
  end.

open_window(Session, Window) ->
  webdrv_session:set_url(Session, "http://localhost:8088/windows.html"),
  {ok, E} = webdrv_session:find_element(Session, "name", Window),
  webdrv_session:click_element(Session, E),
  webdrv_session:set_window_focus(Session, Window).

test5() ->
  R1 = webdrv_wire:get_status(#webdrv_opts{ url = ?SELENIUM }),
  io:format("Status: ~p\n", [R1]),

  {ok, _Pid} = webdrv_session:start_session(test, ?SELENIUM,
                                            webdrv_cap:default_htmlunit(), 10000),
  R = webdrv_session:get_status(test),
  io:format("Status: ~p\n", [R]),

  webdrv_session:stop_session(test).


test4() ->
  {ok, _Pid} = webdrv_session:start_session(test, ?CHROMEDRIVER,
                                     webdrv_cap:default_chrome(), 10000),
  webdrv_session:execute(test, <<"window.name = 'WINDOW-0';">>, []),
  open_window(test, "WINDOW-1"),
  %% webdrv_session:close_window(test),
  webdrv_session:set_window_focus(test, "WINDOW-0"),

  Res = webdrv_session:get_screenshot(test),
  io:format("Res: ~p\n", [Res]),

  webdrv_session:stop_session(test).


test3() ->
  {ok, _Pid} = webdrv_session:start_session(test, ?SELENIUM, webdrv_cap:default_htmlunit(), 10000),
  webdrv_session:set_url(test, "http://localhost:8088/elements.html"),
  Code = webdrv_session:get_page_source(test),
  io:format("CODE: ~p\n", [Code]),

  Res = webdrv_session:find_element(test, "class name", "class1"),
  io:format("RES: ~p\n", [Res]),

  webdrv_session:stop_session(test).


test2() ->
  {ok, _Pid} = webdrv_session:start_session(test, ?CHROMEDRIVER, webdrv_cap:default_chrome(), 10000),
  %% {ok, _Pid} = webdrv_session:start_session(test, ?SELENIUM, webdrv_cap:default_chrome(), 10000),

  R = webdrv_session:get_cache_status(test),
  io:format("~p\n", [R]),

  %% [ io:format("~p\n", [{element(1, webdrv_session:get_log(test, L)), L}])
  %% [ io:format("~p\n", [webdrv_session:get_log(test, L)])
    %% || L <- ["browser","driver","client","server"] ],

  webdrv_session:stop_session(test).

test() ->
  {ok, _Pid} = webdrv_session:start_session(test, ?SELENIUM, webdrv_cap:default_firefox(), 10000),

  Res = webdrv_session:execute(test, <<"window.name = 'WINDOW0';">>, []),
  io:format("Res: ~p\n", [Res]),

  Res2 = webdrv_session:set_window_maximize(test, "DFSJFLKSDJKLFJLKJF"),
  io:format("Res: ~p\n", [Res2]),

  webdrv_session:set_url(test, "http://80.252.210.134:8001/webtest/windows.html"),

  {ok, E0} = webdrv_session:find_element(test, "name", "WINDOW0"),
  webdrv_session:click_element(test, E0),

  {ok, E1} = webdrv_session:find_element(test, "name", "WINDOW1"),
  webdrv_session:click_element(test, E1),

  {ok, E2} = webdrv_session:find_element(test, "name", "WINDOW2"),
  webdrv_session:click_element(test, E2),

  webdrv_session:set_window_focus(test, "WINDOW0"),
  webdrv_session:set_url(test, "http://80.252.210.134:8001/webtest/page0.html"),

  webdrv_session:set_window_focus(test, "WINDOW1"),
  webdrv_session:set_url(test, "http://80.252.210.134:8001/webtest/page2.html"),

  webdrv_session:set_window_focus(test, "WINDOW2"),
  webdrv_session:set_url(test, "http://80.252.210.134:8001/webtest/page1.html"),

  webdrv_session:set_window_focus(test, "WINDOW2"),
  io:format("~p\n", [webdrv_session:get_page_source(test)]),

  webdrv_session:set_window_focus(test, "WINDOW1"),
  io:format("~p\n", [webdrv_session:get_page_source(test)]),

  webdrv_session:set_window_focus(test, "WINDOW0"),
  io:format("~p\n", [webdrv_session:get_page_source(test)]),


  webdrv_session:stop_session(test).


links_w_text(SessName) ->
  {ok, Elements} = webdrv_session:find_elements(test, "tag name", "a"),
  %% [ io:format("~p\n", [E2]) || E <- Elements, E2 <- webdrv_session:get_element_info(SessName, E) ],
  Hrefs = [ Href || E <- Elements, {ok, Href} <- [webdrv_session:element_attribute(SessName, E, "href")] ],
  Texts = [ Txt || E <- Elements, {ok, Txt} <- [webdrv_session:get_text(SessName, E)] ],
  io:format("Elements: ~p\n", [lists:zip3(Elements, Hrefs, Texts)]).

all_elements() ->
  [
   {{"id", "id1"}, 3},
   %% {{"id", "id2"}, 5},

   {{"name", "name1"}, 3},
   %% {{"name", "name2"}, 5},

   {{"css selector", "ul"}, 2},

   {{"class name", "class2"}, 5},

   {{"tag name", "ul"}, 2},

   {{"link text", "Link1"}, 18},

   {{"partial link text", "nk1"}, 18},

   {{"xpath", "//html"}, 1},
   {{"xpath", "/html/body/ul"}, 2}
  ].

all_elements2() ->
  [
   {{"id", "id1"}, 3},
   %% {{"id", "id2"}, 5},

   {{"name", "name1"}, 3},
   %% {{"name", "name2"}, 5},

   {{"css selector", "ul"}, 2},

   {{"class name", "class2"}, 5},

   {{"tag name", "ul"}, 2},

   {{"link text", "Link1"}, 18},

   {{"partial link text", "nk1"}, 18},

   {{"xpath", "//html"}, 1},
   {{"xpath", "/html/body/ul"}, 2},
   {{"xpath", "li"}, 20}
  ].


res_sel_html() ->
[{{"id","id1"},{"id","id1"},fail},
 {{"id","id1"},{"name","name1"},fail},
 {{"id","id1"},{"css selector","ul"},fail},
 {{"id","id1"},{"class name","class2"},fail},
 {{"id","id1"},{"tag name","ul"},fail},
 {{"id","id1"},{"link text","Link2"},fail},
 {{"id","id1"},{"partial link text","nk2"},fail},
 {{"id","id1"},{"xpath","html"},fail},
 {{"id","id1"},{"xpath","/html/body/ul"},ok},
 {{"id","id1"},{"xpath","/html/body/a[last()]"},ok},
 {{"name","name1"},{"id","id1"},fail},
 {{"name","name1"},{"name","name1"},fail},
 {{"name","name1"},{"css selector","ul"},fail},
 {{"name","name1"},{"class name","class2"},fail},
 {{"name","name1"},{"tag name","ul"},fail},
 {{"name","name1"},{"link text","Link2"},fail},
 {{"name","name1"},{"partial link text","nk2"},fail},
 {{"name","name1"},{"xpath","html"},fail},
 {{"name","name1"},{"xpath","/html/body/ul"},ok},
 {{"name","name1"},{"xpath","/html/body/a[last()]"},ok},
 {{"css selector","ul"},{"id","id1"},ok},
 {{"css selector","ul"},{"name","name1"},ok},
 {{"css selector","ul"},{"css selector","ul"},fail},
 {{"css selector","ul"},{"class name","class2"},ok},
 {{"css selector","ul"},{"tag name","ul"},fail},
 {{"css selector","ul"},{"link text","Link2"},fail},
 {{"css selector","ul"},{"partial link text","nk2"},fail},
 {{"css selector","ul"},{"xpath","html"},fail},
 {{"css selector","ul"},{"xpath","/html/body/ul"},ok},
 {{"css selector","ul"},{"xpath","/html/body/a[last()]"},ok},
 {{"class name","class2"},{"id","id1"},fail},
 {{"class name","class2"},{"name","name1"},fail},
 {{"class name","class2"},{"css selector","ul"},fail},
 {{"class name","class2"},{"class name","class2"},fail},
 {{"class name","class2"},{"tag name","ul"},fail},
 {{"class name","class2"},{"link text","Link2"},fail},
 {{"class name","class2"},{"partial link text","nk2"},fail},
 {{"class name","class2"},{"xpath","html"},fail},
 {{"class name","class2"},{"xpath","/html/body/ul"},ok},
 {{"class name","class2"},{"xpath","/html/body/a[last()]"},ok},
 {{"tag name","ul"},{"id","id1"},ok},
 {{"tag name","ul"},{"name","name1"},ok},
 {{"tag name","ul"},{"css selector","ul"},fail},
 {{"tag name","ul"},{"class name","class2"},ok},
 {{"tag name","ul"},{"tag name","ul"},fail},
 {{"tag name","ul"},{"link text","Link2"},fail},
 {{"tag name","ul"},{"partial link text","nk2"},fail},
 {{"tag name","ul"},{"xpath","html"},fail},
 {{"tag name","ul"},{"xpath","/html/body/ul"},ok},
 {{"tag name","ul"},{"xpath","/html/body/a[last()]"},ok},
 {{"link text","Link2"},{"id","id1"},fail},
 {{"link text","Link2"},{"name","name1"},fail},
 {{"link text","Link2"},{"css selector","ul"},fail},
 {{"link text","Link2"},{"class name","class2"},fail},
 {{"link text","Link2"},{"tag name","ul"},fail},
 {{"link text","Link2"},{"link text","Link2"},fail},
 {{"link text","Link2"},{"partial link text","nk2"},fail},
 {{"link text","Link2"},{"xpath","html"},fail},
 {{"link text","Link2"},{"xpath","/html/body/ul"},ok},
 {{"link text","Link2"},{"xpath","/html/body/a[last()]"},ok},
 {{"partial link text","nk2"},{"id","id1"},fail},
 {{"partial link text","nk2"},{"name","name1"},fail},
 {{"partial link text","nk2"},{"css selector","ul"},fail},
 {{"partial link text","nk2"},{"class name","class2"},fail},
 {{"partial link text","nk2"},{"tag name","ul"},fail},
 {{"partial link text","nk2"},{"link text","Link2"},fail},
 {{"partial link text","nk2"},{"partial link text","nk2"},fail},
 {{"partial link text","nk2"},{"xpath","html"},fail},
 {{"partial link text","nk2"},{"xpath","/html/body/ul"},ok},
 {{"partial link text","nk2"},{"xpath","/html/body/a[last()]"},ok},
 {{"xpath","html"},{"id","id1"},ok},
 {{"xpath","html"},{"name","name1"},ok},
 {{"xpath","html"},{"css selector","ul"},ok},
 {{"xpath","html"},{"class name","class2"},ok},
 {{"xpath","html"},{"tag name","ul"},ok},
 {{"xpath","html"},{"link text","Link2"},ok},
 {{"xpath","html"},{"partial link text","nk2"},ok},
 {{"xpath","html"},{"xpath","html"},fail},
 {{"xpath","html"},{"xpath","/html/body/ul"},ok},
 {{"xpath","html"},{"xpath","/html/body/a[last()]"},ok},
 {{"xpath","/html/body/ul"},{"id","id1"},ok},
 {{"xpath","/html/body/ul"},{"name","name1"},ok},
 {{"xpath","/html/body/ul"},{"css selector","ul"},fail},
 {{"xpath","/html/body/ul"},{"class name","class2"},ok},
 {{"xpath","/html/body/ul"},{"tag name","ul"},fail},
 {{"xpath","/html/body/ul"},{"link text","Link2"},fail},
 {{"xpath","/html/body/ul"},{"partial link text","nk2"},fail},
 {{"xpath","/html/body/ul"},{"xpath","html"},fail},
 {{"xpath","/html/body/ul"},{"xpath","/html/body/ul"},ok},
 {{"xpath","/html/body/ul"},{"xpath","/html/body/a[last()]"},ok},
 {{"xpath","/html/body/a[last()]"},{"id","id1"},fail},
 {{"xpath","/html/body/a[last()]"},{"name","name1"},fail},
 {{"xpath","/html/body/a[last()]"},{"css selector","ul"},fail},
 {{"xpath","/html/body/a[last()]"},{"class name","class2"},fail},
 {{"xpath","/html/body/a[last()]"},{"tag name","ul"},fail},
 {{"xpath","/html/body/a[last()]"},{"link text","Link2"},fail},
 {{"xpath","/html/body/a[last()]"},{"partial link text","nk2"},fail},
 {{"xpath","/html/body/a[last()]"},{"xpath","html"},fail},
 {{"xpath","/html/body/a[last()]"},{"xpath","/html/body/ul"},ok},
 {{"xpath","/html/body/a[last()]"},{"xpath","/html/body/a[last()]"},ok}]
.
