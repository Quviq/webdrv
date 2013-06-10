%%%-------------------------------------------------------------------
%%% @author Hans Svensson <hans.svensson@quviq.com>
%%% @copyright (C) 2013, Quviq AB
%%% @license
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use, copy,
%%% modify, merge, publish, distribute, sublicense, and/or sell copies
%%% of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(webdrv_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-import(eqc_statem, [eq/2]).

-compile(export_all).

%% Test definitions -- Adapt to local situation

-define(SELENIUM_URL,     "http://localhost:4444/wd/hub/").
-define(CHROMEDRIVER_URL, "http://localhost:9515/").
-define(HTTP_URL,         "http://localhost:8088/").

-define(ORELSE(X, Y), case X of
                        true -> true;
                        __R  -> case Y of
                                  true -> true;
                                  _    -> __R
                                end
                      end).

%% {Frequency, {Driver, Browser}}
-define(DRIVERS_AND_BROWSERS,
        [{2, {selenium, htmlunit}},
         {1, {selenium, chrome}},
         {1, {selenium, firefox}},
         {1, {chromedriver, chrome}} ]).

-define(WINDOWBASE,  "WINDOW-").
-define(SESSIONBASE, "SESS-").

-define(MAX_SESSIONS, 2).
-define(MAX_WINDOWS,  3).

%% Make sure there is margin to increase size and/or move
%% Chromedriver will refuse to move window off screen!
-define(DEFAULT_HEIGHT, 800).
-define(DEFAULT_WIDTH,  600).

%% ------ Test state

%% Note: Windows are Tabs in Chrome, thus setting the size of individual windows isn't
%% really meaningful there.

%% A browser window/tab
-record(window,  { name
                 , curr_page = unknown
                 , window_handle
                 , maximized = false
                 , size
                 , position }).
%% A webdriver session
-record(session, { name
                 , driver
                 , browser
                 , curr_window = ?WINDOWBASE ++ "0"
                 , windows = []
                 , ime_engines = []
                 , size
                 , position
                 , maximized = false }).

-record(state,   { sessions = [] }).

initial_state() ->
  #state{}.

%% ------ Test generators
fresh_session(Sessions) ->
  fresh_session([ Name || #session{ name = Name } <- Sessions], 0).

fresh_session(Used, N) ->
  case lists:member(mk_session_name(N), Used) of
    true  -> fresh_session(Used, N + 1);
    false -> mk_session_name(N) end.

driver_and_browser()  ->
  N = lists:sum([ N || {N, _} <-tl(?DRIVERS_AND_BROWSERS)]),
  weighted_default(hd(?DRIVERS_AND_BROWSERS),
                   {N, frequency(tl(?DRIVERS_AND_BROWSERS))}).

session_name(S) ->
  elements([ N || #session{ name = N } <- S#state.sessions ]).

window() ->
  ?LET(N, choose(1,?MAX_WINDOWS), ?WINDOWBASE ++ integer_to_list(N-1)).

window(Windows) ->
  elements([W || #window{ name = W } <- Windows]).

session_and_window(S) ->
  ?LET(Session, session_name(S),
  ?LET(Window,  weighted_default({3, window((get_session(S, Session))#session.windows)},
                                 {1, oneof([window(), return("current")])}),
       {Session, Window})).

window_size() ->
  {choose(?DEFAULT_WIDTH - 100, ?DEFAULT_WIDTH + 100),
   choose(?DEFAULT_HEIGHT - 100, ?DEFAULT_HEIGHT + 100)}.

rel_element_find() ->
  ?LET(Good, weighted_default({5,true}, {1, false}), rel_element_find(Good)).

rel_element_find(false) ->
  element_find(false);
rel_element_find(true) ->
  elements([{"tag name", "html", first},
            {"tag name", "hr", last}]).

element_find() ->
  ?LET(Good, weighted_default({5,true}, {1, false}), element_find(Good)).

element_find(Good) ->
  frequency([{1, element_id(Good)},
             {1, element_name(Good)},
             {1, element_css(Good)},
             {1, element_tag(Good)},
             {3, element_xpath(Good)},
             {1, element_link(Good)},
             {1, element_plink(Good)},
             {1, element_class(Good)}]).

element_id(false) ->
  {"id", elements(["this_doesnt_exist", "blafoo"]), false};
element_id(true) ->
  {"id", elements(["id1", "id2", "id3"]), true}.

element_name(false) ->
  {"name", elements(["this_doesnt_exist", "blafoo"]), false};
element_name(true) ->
  {"name", elements(["name1", "name2", "name3"]), true}.

%% TODO: More advanced css!
element_css(false) ->
  {"css selector", elements(["h2", "script"]), false};
element_css(true) ->
  {"css selector", elements(["ul", "li"]), true}.

element_class(false) ->
  {"class name", elements(["class9", "class14"]), false};
element_class(true) ->
  {"class name", elements(["class1", "class2"]), true}.

element_tag(false) ->
  {"tag name", elements(["h2", "script"]), false};
element_tag(true) ->
  {"tag name", elements(["ul", "li"]), true}.

element_link(false) ->
  {"link text", elements(["xxxxxxxx", "adfasdlfs"]), false};
element_link(true) ->
  {"link text", elements(["Link1", "Link2"]), true}.

element_plink(false) ->
  {"partial link text", elements(["xx", "zz"]), false};
element_plink(true) ->
  {"partial link text", elements(["k1", "nk2"]), true}.

element_xpath(false) ->
  frequency([{3, {"xpath", elements(["blafoo", "//foobar", "//@foo"]), false}},
             {3, {"xpath", elements(["html[invalid_expr()]", "html[price==14]"]), invalid}}]);
element_xpath(true) ->
  {"xpath", elements(["//body", "/html/body/ul", "/html/body/ul/li",
                      "//li", "//a[last()]"]), true}.

%% Some problem in Unity, can't position window at {0, 0} -- Menubar!
window_position() ->
  {choose(30, 100), choose(30, 100)}.

url() ->
  elements([ ?HTTP_URL ++ "page0.html"
           , ?HTTP_URL ++ "page1.html"
           , ?HTTP_URL ++ "page2.html" ]).

which_timeout() ->
  elements(["script", "implicit", "page load"]).

timeout_val() ->
  choose(100, 500).

implicit_timeout_val() ->
  weighted_default({3, 0}, {1, choose(1, 30)}).

%% ------ Common pre-/post-conditions
command_precondition_common(S, Cmd) ->
  length(S#state.sessions) > 0
    orelse lists:member(Cmd, [start_session]).

precondition_common(S, {call, _, Cmd, _}) ->
  length(S#state.sessions) > 0
    orelse lists:member(Cmd, [start_session]).

%% --- start_session
%% Starting a WebDriver session.
%% + Connect to browser
%% + Set a known start page
%% + Name the first window ?WINDOWBASE ++ "0"
%% + Set a reasonable size of the window
start_session(Session, {Driver, Browser}) ->
  try
    Res =
      case webdrv_session:start_session(Session, driver_url(Driver),
                                        webdrv_cap:default_browser(Browser), 8000) of
        {ok, _Pid}                       ->
          webdrv_session:set_url(Session, ?HTTP_URL ++ "page1.html"),
          webdrv_session:execute(Session, <<"window.name = '" ?WINDOWBASE "0';">>, []),
          webdrv_session:set_window_size(Session, ?DEFAULT_WIDTH, ?DEFAULT_HEIGHT),
          ok;
        {error, {already_started, _Pid}} -> {error, already_started};
        E -> E
      end,
    Res
  catch _:Err ->
      {error, Err}
  end.

start_session_args(S) ->
  [fresh_session(S#state.sessions), driver_and_browser()].

start_session_pre(S) ->
  length(S#state.sessions) < ?MAX_SESSIONS.

start_session_pre(S, [SessName, {_Driver, _Browser}]) ->
  length(S#state.sessions) < ?MAX_SESSIONS
    andalso not lists:member(SessName, [ N || #session{ name = N } <- S#state.sessions ]).

start_session_next(S, _Value, [SessName, {Driver, Browser}]) ->
  S#state{ sessions = [#session{ name    = SessName,
                                 driver  = Driver,
                                 browser = Browser,
                                 windows = [#window{ name = ?WINDOWBASE ++ "0",
                                                     curr_page = ?HTTP_URL ++ "page1.html" }] }
                       | S#state.sessions]
         }.

start_session_post(_S, [_SessNameState, _Driver_And_Browser], Res) ->
  eq(Res, ok).


%% --- stop_session
stop_session(Session) ->
  case (catch webdrv_session:stop_session(Session)) of
    {'EXIT', _} ->
      %% In case the gen_server is hung, be more brutal
      catch webdrv_session:stop(Session);
    _ -> ok
  end.

stop_session_args(S) ->
  [session_name(S)].

stop_session_next(S, _V, [Session]) ->
  S#state{ sessions = lists:keydelete(Session, #session.name, S#state.sessions) }.

%% --- set_url
set_url(Session, Url) ->
  webdrv_session:set_url(Session, Url).

set_url_args(S) ->
  [session_name(S), url()].

set_url_next(S, _V, [Session, Url]) ->
  set_curr_page(S, Session, Url).


%% --- get_url
get_url(Session) ->
  webdrv_session:get_url(Session).

get_url_args(S) ->
  [session_name(S)].

get_url_post(S, [Session], Res) ->
  case get_curr_page(S, Session) of
    unknown -> true;
    Page    -> eq(Res, {ok, Page}) end.


%% --- set_timeout
set_timeout(Session, WhichTimeout, Timeout) ->
  webdrv_session:set_timeout(Session, WhichTimeout, Timeout).

set_timeout_args(S) ->
  [session_name(S), which_timeout(), timeout_val()].

set_timeout_post(S, [Session, WhichTimeout, _Timeout], Res) ->
  ?ORELSE(eq(Res, ok),
          (is_chrome(S, Session)
           andalso is_selenium(S, Session)
           andalso WhichTimeout == "page load")).

%% --- set_async_script_timeout
set_async_script_timeout(Session, Timeout) ->
  webdrv_session:set_async_script_timeout(Session, Timeout).

set_async_script_timeout_args(S) ->
  [session_name(S), timeout_val()].

set_async_script_timeout_post(_S, [_Session, _Timeout], Res) ->
  eq(Res, ok).

%% --- set_implicit_wait_timeout
set_implicit_wait_timeout(Session, Timeout) ->
  webdrv_session:set_implicit_wait_timeout(Session, Timeout).

set_implicit_wait_timeout_args(S) ->
  [session_name(S), implicit_timeout_val()].

set_implicit_wait_timeout_post(_S, [_Session, _Timeout], Res) ->
  eq(Res, ok).

%% --- get_window_handle
get_window_handle(Session) ->
  webdrv_session:get_window_handle(Session).

get_window_handle_args(S) ->
  [session_name(S)].

get_window_handle_next(S, V, [Session]) ->
  set_curr_window_handle(S, Session, V).

get_window_handle_post(S, [Session], Res) ->
  WHandle = get_curr_window_handle(S, Session),
  ?ORELSE(eq(Res, WHandle),
          WHandle == undefined). %% If we think we know the handle it should match!

%% --- get_window_handles
get_window_handles(Session) ->
  webdrv_session:get_window_handles(Session).

get_window_handles_args(S) ->
  [session_name(S)].

%% Let's be happy if the length is ok. TODO: check the names
get_window_handles_post(S, [Session], Res) ->
  FixRes = case Res of
             {ok, Hs} when is_list(Hs) -> {ok, length(Hs)};
             _ -> Res end,
  eq(FixRes, {ok, n_windows(S, Session)}).

%% --- forward
forward(Session) ->
  webdrv_session:forward(Session).

forward_args(S) ->
  [session_name(S)].

forward_next(S, _V, [Session]) ->
  clear_curr_page(S, Session).

forward_post(_S, [_Session], Res) ->
  eq(Res, ok).

%% --- back
back(Session) ->
  webdrv_session:back(Session).

back_args(S) ->
  [session_name(S)].

back_next(S, _V, [Session]) ->
  clear_curr_page(S, Session).

back_post(_S, [_Session], Res) ->
  eq(Res, ok).

%% --- refresh
refresh(Session) ->
  webdrv_session:refresh(Session).

refresh_args(S) ->
  [session_name(S)].

refresh_post(_S, [_Session], Res) ->
  eq(Res, ok).

%% --- get_cookies
get_cookies(Session) ->
  webdrv_session:get_cookies(Session).

get_cookies_args(S) ->
  [session_name(S)].

get_cookies_next(S, _V, [Session]) ->
  clear_curr_page(S, Session).

get_cookies_post(_S, [_Session], Res) ->
  eq(Res, {ok, []}). %% Until we model add_cookie ;-)

%% TODO: add_cookie, delete_cookie(s)

%% --- get_page_source
get_page_source(Session) ->
  webdrv_session:get_page_source(Session).

get_page_source_args(S) ->
  [session_name(S)].

get_page_source_next(S, _V, [Session]) ->
  clear_curr_page(S, Session).

%% TODO: really check we know what to expect...
get_page_source_post(_S, [_Session], Res) ->
  res_ok(Res).

%% --- get_page_title
get_page_title(Session) ->
  webdrv_session:get_page_title(Session).

get_page_title_args(S) ->
  [session_name(S)].

get_page_title_next(S, _V, [Session]) ->
  clear_curr_page(S, Session).

%% TODO: really check we know what to expect...
get_page_title_post(_S, [_Session], Res) ->
  res_ok(Res).

%% --- set_window_focus
set_window_focus({Session, Window}) ->
  webdrv_session:set_window_focus(Session, Window).

set_window_focus_args(S) ->
  [session_and_window(S)].

set_window_focus_pre(S, [{Session, _Window}]) ->
  is_session(S, Session).

set_window_focus_next(S, _V, [{Session, Window}]) ->
  case get_window(S, Session, Window) of
    not_found -> S;
    W = #window{} -> set_curr_window(S, Session, W#window.name) end.

set_window_focus_post(_S, [{_Session, "current"}], Res) ->
  res_error(Res, 'NoSuchWindow');
set_window_focus_post(S, [{Session, Window}], Res) ->
  case get_window(S, Session, Window) of
    not_found -> res_error(Res, 'NoSuchWindow');
    #window{} -> eq(Res, ok) end.

%% --- get_window_size
get_window_size({Session, Window}) ->
  timer:sleep(50), %% Allow time for size change to be made.
  if Window == "current" ->
      webdrv_session:get_window_size(Session);
     true ->
      webdrv_session:get_window_size(Session, Window)
  end.

get_window_size_args(S) ->
  [session_and_window(S)].

get_window_size_pre(S, [{Session, _Window}]) ->
  case get_session(S, Session) of
    false -> false;
    #session{browser = B, driver = D, windows = Ws } ->
      %% There is a bug in the version of chromedriver used by selenium!
      (length(Ws) == 1 orelse B /= chrome orelse D /= selenium)
  end.

get_window_size_post(S, [{Session, Window}], Res) ->
  case get_window(S, Session, Window) of
    not_found -> %% This *should* be an error, but both Selenium and Chromedriver
                 %% returns the size of the *current* window!?
                 %% res_error(Res, 'NoSuchWindow');
      res_ok(Res);
    #window{ } ->
      Size = get_size(S, Session),
      case Size of
        undefined -> res_ok(Res);
        Size -> eq(Res, {ok, Size}) end
  end.

get_size(S, Session) ->
  case has_tabs(S, Session) of
    true  ->
      Sess = get_session(S, Session),
      Sess#session.size;
    false ->
      Win = get_curr_window(S, Session),
      Win#window.size
  end.

%% --- set_window_size
set_window_size({Session, Window}, {Width, Height}) ->
  if Window == "current" ->
      webdrv_session:set_window_size(Session, Width, Height);
     true ->
      webdrv_session:set_window_size(Session, Window, Width, Height)
  end.

set_window_size_args(S) ->
  [session_and_window(S), window_size()].

set_window_size_pre(S, [{Session, _Window}, _Size]) ->
  is_session(S, Session).

set_window_size_next(S, _V, [{Session, _Window}, Size]) ->
  case has_tabs(S, Session) of
    true ->
      Sess = get_session(S, Session),
      set_session(S, Sess#session{ size = Size, maximized = false });
    false ->
      Win = get_curr_window(S, Session),
      set_window(S, Session, Win#window{ size = Size, maximized = false })
  end.

set_window_size_post(_S, [{_Session, _Window}, _Size], Res) ->
  eq(Res, ok).

%% --- get_window_position
get_window_position({Session, Window}) ->
  timer:sleep(50), %% Allow time for position change to be made.
  if Window == "current" ->
      webdrv_session:get_window_position(Session);
     true ->
      webdrv_session:get_window_position(Session, Window)
  end.

get_window_position_args(S) ->
  [session_and_window(S)].

get_window_position_pre(S, [{Session, _Window}]) ->
  case get_session(S, Session) of
    false -> false;
    #session{browser = B, driver = D, windows = Ws } ->
      %% There is a bug in the version of chromedriver used by selenium!
      (length(Ws) == 1 orelse B /= chrome orelse D /= selenium)
  end.

get_window_position_post(S, [{Session, Window}], Res) ->
  case get_window(S, Session, Window) of
    not_found -> %% This *should* be an error, but both Selenium and Chromedriver
                 %% returns the position of the *current* window!?
                 %% res_error(Res, 'NoSuchWindow');
      res_ok(Res);
    #window{ } -> Pos = get_position(S, Session),
                  case Pos of
                    undefined -> res_ok(Res);
                    Position -> eq(Res, {ok, Position}) end
  end.

get_position(S, Session) ->
  case has_tabs(S, Session) of
    true  ->
      Sess = get_session(S, Session),
      Sess#session.position;
    false ->
      Win = get_curr_window(S, Session),
      Win#window.position
  end.

%% --- set_window_position
set_window_position({Session, Window}, {Width, Height}) ->
  if Window == "current" ->
      webdrv_session:set_window_position(Session, Width, Height);
     true ->
      webdrv_session:set_window_position(Session, Window, Width, Height)
  end.

set_window_position_args(S) ->
  [session_and_window(S), window_position()].

set_window_position_pre(S, [{Session, _Window}, _Position]) ->
  is_session(S, Session).

set_window_position_next(S, _V, [{Session, _Window}, Position]) ->
  case has_tabs(S, Session) of
    true ->
      Sess = get_session(S, Session),
      set_session(S, Sess#session{ position = Position, maximized = false });
    false ->
      Win = get_curr_window(S, Session),
      set_window(S, Session, Win#window{ position = Position, maximized = false })
  end.

set_window_position_post(_S, [{_Session, _Window}, _Position], Res) ->
  eq(Res, ok).

%% --- open_window
open_window(Session, Window) ->
  webdrv_session:set_url(Session, ?HTTP_URL ++ "windows.html"),
  {ok, E} = webdrv_session:find_element(Session, "name", Window),
  webdrv_session:click_element(Session, E),
  webdrv_session:back(Session),
  webdrv_session:set_window_focus(Session, Window).

open_window_args(S) ->
  [session_name(S), window()].

open_window_next(S, _V, [Session, Window]) ->
  Sess = get_session(S, Session),
  S1 = set_session(S, Sess#session{ curr_window = Window }),
  set_window(S1, Session, #window{ name = Window }).

%% --- close_window
%% Make sure not to close the 'last' window; that is stop_session!
close_window(Session, Window) ->
  Res = webdrv_session:close_window(Session),
  ok = webdrv_session:set_window_focus(Session, Window),
  Res.

close_window_args(S) ->
  ?LET({Session, Ws},
       elements([ {N, Ws} || #session{ name = N, windows = Ws } <- S#state.sessions,
                             length(Ws) > 1 ]),
       [Session, elements([ W || #window{ name = W } <- Ws] --
                            [get_curr_window(S, Session)])]).

close_window_pre(S) ->
  [] /= [ ok || #session{ windows = Ws } <- S#state.sessions,
                length(Ws) > 1 ].

close_window_pre(S, [Session, Window]) ->
  case get_session(S, Session) of
    false -> false;
    #session{windows = Ws, curr_window = W } ->
      length(Ws) > 1 andalso W /= Window
  end.

close_window_next(S, _V, [Session, Window]) ->
  Sess = get_session(S, Session),
  set_session(S, Sess#session{ windows = lists:keydelete(Sess#session.curr_window,
                                                         #window.name,
                                                         Sess#session.windows)
                             , curr_window = Window }).

close_window_post(_S, [_Session, _Window], Res) ->
  eq(Res, ok).

%% --- maximize_window
set_window_maximize({Session, Window}) ->
  if Window == "current" ->
      webdrv_session:set_window_maximize(Session);
     true ->
      webdrv_session:set_window_maximize(Session, Window)
  end.

set_window_maximize_args(S) ->
  [ session_and_window(S) ].

set_window_maximize_pre(S, [{Session, _Window}]) ->
  case get_session(S, Session) of
    false -> false;
    #session{ browser = B, driver = D, maximized = M } ->
      %% Bug in chromedriver used by selenium. Hangs if maximized twice!
      not M orelse B /= chrome orelse D /= selenium
  end.

set_window_maximize_next(S, _V, [{Session, _Window}]) ->
  case has_tabs(S, Session) of
    true ->
      Sess = get_session(S, Session),
      set_session(S, Sess#session{ size = undefined,
                                   position = undefined,
                                   maximized = true });
    false ->
      Win = get_curr_window(S, Session),
      set_window(S, Session, Win#window{ size = undefined,
                                         position = undefined,
                                         maximized = true})
  end.

set_window_maximize_post(_S, [{_Session, _Window}], Res) ->
  eq(Res, ok).

%% --- get_screenshot
get_screenshot(Session) ->
  webdrv_session:get_screenshot(Session).

get_screenshot_args(S) ->
  [session_name(S)].

get_screenshot_pre(S, [Session]) ->
  case get_session(S, Session) of
    false -> false;
    #session{browser = B, driver = D, windows = Ws } ->
      B /= htmlunit
      %% Bug in Chromedriver2, only the last opened window can be screenshot.
      %% This is a bit more restrictive, but let's be happy with that...
        andalso (B /= chrome orelse D /= chromedriver orelse length(Ws) == 1)
  end.

get_screenshot_post(_S, [_Session], Res) ->
  res_ok(Res).

%% --- get_cache_status
get_cache_status(Session) ->
  webdrv_session:get_cache_status(Session).

get_cache_status_args(S) ->
  [session_name(S)].

get_cache_status_pre(S, [Session]) ->
  case get_session(S, Session) of
    false -> false;
    #session{} -> not is_selenium(S, Session)
  end.

get_cache_status_post(_S, [_Session], Res) ->
  res_ok(Res).

%% --- find_element
find_element(Session, Strategy, Value) ->
  webdrv_session:set_url(Session, ?HTTP_URL ++ "elements.html"),
  Res = webdrv_session:find_element(Session, Strategy, Value),
  webdrv_session:back(Session),
  Res.

find_element_command(S) ->
  ?LET({Strategy, Value, IsOk}, element_find(),
       {call, ?MODULE, find_element, [session_name(S), Strategy, Value], IsOk}).

find_element_post(S, [Session, _Strategy, _Value], Res, IsOk) ->
  IsHtmlUnit = is_headless(S, Session),
  case IsOk of
    true                    -> res_ok(Res);
    false                   -> res_error(Res, 'NoSuchElement');
    invalid when IsHtmlUnit -> res_error(Res, 'UnknownError');
    invalid                 -> res_error(Res, 'InvalidSelector')
  end.

%% --- find_elements
find_elements(Session, Strategy, Value) ->
  webdrv_session:set_url(Session, ?HTTP_URL ++ "elements.html"),
  Res = webdrv_session:find_elements(Session, Strategy, Value),
  webdrv_session:back(Session),
  Res.

find_elements_command(S) ->
  ?LET({Strategy, Value, IsOk}, element_find(),
       {call, ?MODULE, find_elements, [session_name(S), Strategy, Value], IsOk}).

find_elements_post(S, [Session, _Strategy, _Value], Res, IsOk) ->
  IsHtmlUnit = is_headless(S, Session),
  case IsOk of
    true                    -> case Res of
                                 {ok, Es} when is_list(Es) -> true;
                                 _ -> eq(Res, {ok, '<list-of-elements>'}) end;
    false                   -> eq(Res, {ok, []});
    invalid when IsHtmlUnit -> res_error(Res, 'UnknownError');
    invalid                 -> res_error(Res, 'InvalidSelector')
  end.

%% --- find_element_rel
find_element_rel(Session, {Strategy1, Value1}, {Strategy2, Value2}) ->
  webdrv_session:set_url(Session, ?HTTP_URL ++ "elements.html"),
  RelE = case webdrv_session:find_element(Session, Strategy1, Value1) of
           {ok, E} -> E;
           _       -> "not_likely_to_be_an_id"
         end,
  Res = webdrv_session:find_element_rel(Session, RelE, Strategy2, Value2),
  webdrv_session:back(Session),
  Res.

find_element_rel_command(S) ->
  ?LET({{Strategy1, Value1, IsOk1}, {Strategy2, Value2, IsOk2}},
       {rel_element_find(), element_find()},
       {call, ?MODULE, find_element_rel,
        [session_name(S), {Strategy1, Value1}, {Strategy2, Value2}], {IsOk1, IsOk2}}).

find_element_rel_post(S, [Session, _SV1, {Strategy2, Value2}], Res, {IsOk1, IsOk2}) ->
  IsHtmlUnit = is_headless(S, Session),
  IsSelenium = is_selenium(S, Session),
  XPathAbs   = case {Strategy2, Value2} of
                 {"xpath", [$/ | _]} -> true;
                 _                   -> false end,
  case {IsOk1, IsOk2} of
    {false, _} when IsSelenium   -> res_error(Res, 'UnknownError');
    {false, _}                   -> res_error(Res, 'StaleElementReference');
    {invalid, _} when IsSelenium -> res_error(Res, 'UnknownError');
    {invalid, _}                 -> res_error(Res, 'StaleElementReference');
    {first, true}                -> res_ok(Res);
    {last,  true} when XPathAbs  -> res_ok(Res);
    {last,  true}                -> res_error(Res, 'NoSuchElement');
    {_, false}                   -> res_error(Res, 'NoSuchElement');
    {_, invalid} when IsHtmlUnit -> res_error(Res, 'UnknownError');
    {_, invalid}                 -> res_error(Res, 'InvalidSelector')
  end.

%% --- find_elements_rel
find_elements_rel(Session, {Strategy1, Value1}, {Strategy2, Value2}) ->
  webdrv_session:set_url(Session, ?HTTP_URL ++ "elements.html"),
  RelE = case webdrv_session:find_element(Session, Strategy1, Value1) of
           {ok, E} -> E;
           _       -> "not_likely_to_be_an_id"
         end,
  Res = webdrv_session:find_elements_rel(Session, RelE, Strategy2, Value2),
  webdrv_session:back(Session),
  Res.

find_elements_rel_command(S) ->
  ?LET({{Strategy1, Value1, IsOk1}, {Strategy2, Value2, IsOk2}},
       {rel_element_find(), element_find()},
       {call, ?MODULE, find_elements_rel,
        [session_name(S), {Strategy1, Value1}, {Strategy2, Value2}], {IsOk1, IsOk2}}).

find_elements_rel_post(S, [Session, _SV1, {Strategy2, Value2}], Res, {IsOk1, IsOk2}) ->
  IsHtmlUnit = is_headless(S, Session),
  IsSelenium = is_selenium(S, Session),
  XPathAbs   = case {Strategy2, Value2} of
                 {"xpath", [$/ | _]} -> true;
                 _                   -> false end,
  case {IsOk1, IsOk2} of
    {false, _} when IsSelenium   -> res_error(Res, 'UnknownError');
    {false, _}                   -> res_error(Res, 'StaleElementReference');
    {invalid, _} when IsSelenium -> res_error(Res, 'UnknownError');
    {invalid, _}                 -> res_error(Res, 'StaleElementReference');
    {first, true}                -> res_ok(Res);
    {last,  true} when XPathAbs  -> res_ok(Res);
    {last,  true}                -> eq(Res, {ok, []});
    {_, false}                   -> eq(Res, {ok, []});
    {_, invalid} when IsHtmlUnit -> res_error(Res, 'UnknownError');
    {_, invalid}                 -> res_error(Res, 'InvalidSelector')
  end.

%% --- get_ime_available_engines
get_ime_available_engines(Session) ->
  webdrv_session:get_ime_available_engines(Session).

get_ime_available_engines_args(S) ->
  [session_name(S)].

get_ime_available_engines_post(S, [Session], Res) ->
  ime_error(S, Session, Res).

ime_error(S, Session, Res) ->
  case is_selenium(S, Session) of
    true  -> res_error(Res, 'UnknownError');
    false -> case Res of
               {html_error, _} -> true;
               _               -> eq(Res, {html_error, blablabla})
             end
  end.

%% --- get_ime_active_engine
get_ime_active_engine(Session) ->
  webdrv_session:get_ime_active_engine(Session).

get_ime_active_engine_args(S) ->
  [session_name(S)].

get_ime_active_engine_post(S, [Session], Res) ->
  ime_error(S, Session, Res).

%% --- get_ime_activated
get_ime_activated(Session) ->
  webdrv_session:get_ime_activated(Session).

get_ime_activated_args(S) ->
  [session_name(S)].

get_ime_activated_post(S, [Session], Res) ->
  ime_error(S, Session, Res).

%% --- get_ime_activate
%% get_ime_activate(Session, Engine) ->
%%   webdrv_session:get_ime_activate(Session).

%% get_ime_activate_args(S) ->
%%   [session_name(S), ime_engine()].

%% get_ime_activate_post(S, [Session, _Engine], Res) ->
%%   ime_error(S, Session, Res).


%% ------ Property
invariant(_S) ->
  true.

weight(_S, open_window) -> 6;
weight(_S, stop_session) -> 1;
weight(_S, _Cmd) -> 3.

prop_webdriver() ->
  ?SETUP(fun() -> catch webdrv_http_srv:start(), fun() -> ok end end,
  ?FORALL(Cmds, more_commands(4, commands(?MODULE)),
  begin
    {H, S, Res} = run_commands(?MODULE,Cmds),
    [ stop_session(Sess#session.name) || Sess <- S#state.sessions ],
    aggregate(command_names(Cmds),
    pretty_commands(?MODULE, Cmds, {H, S, Res},
                    Res == ok))
  end)).


the_prop() ->
  prop_webdriver().

test() ->
  test({20, s}).

test(N) when is_integer(N) ->
  eqc:quickcheck(eqc:numtests(N, the_prop()));
test({N, s}) ->
  eqc:quickcheck(eqc:testing_time(N, the_prop()));
test({N, min}) ->
  test({60*N, s});
test({N, m}) ->
  test({60*N, s}).

check() ->
  check(false).

check(WState) ->
  if WState ->
      eqc:check(eqc_statem:show_states(the_prop()));
     true ->
      eqc:check(the_prop())
  end.

recheck() ->
  eqc:recheck(the_prop()).


%% ------ State manipulation
get_session(State, Name) ->
  lists:keyfind(Name, #session.name, State#state.sessions).

set_session(State, Session) ->
  State#state{ sessions =  lists:keyreplace(Session#session.name, #session.name,
                                            State#state.sessions, Session) }.

get_window(State, SName, "current") ->
  get_curr_window(State, SName);
get_window(State, SName, WName) ->
  Session = lists:keyfind(SName, #session.name, State#state.sessions),
  case lists:keyfind(WName, #window.name, Session#session.windows) of
    false -> not_found;
    W -> W
  end.

set_window(State, SName, Window) ->
  Session = get_session(State, SName),
  set_session(State,
              Session#session{ windows =
                              lists:keystore(Window#window.name, #window.name,
                                             Session#session.windows, Window) }).

get_curr_window(State, Name) ->
  Session = lists:keyfind(Name, #session.name, State#state.sessions),
  lists:keyfind(Session#session.curr_window, #window.name, Session#session.windows).

set_curr_window(State, Name, Window) ->
  Session = get_session(State, Name),
  set_session(State, Session#session{ curr_window = Window }).

get_curr_window_handle(State, Name) ->
  (get_curr_window(State, Name))#window.window_handle.

set_curr_window_handle(State, Name, Handle) ->
  Win = get_curr_window(State, Name),
  set_window(State, Name, Win#window{ window_handle = Handle}).

get_curr_page(State, Name) ->
  Window = get_curr_window(State, Name),
  Window#window.curr_page.

set_curr_page(State, Name, Url) ->
  Window = get_curr_window(State, Name),
  set_window(State, Name, Window#window{ curr_page = Url }).

clear_curr_page(State, Name) -> set_curr_page(State, Name, unknown).

%% ------ Helper functions
mk_session_name(N) ->
  list_to_atom(lists:concat([?SESSIONBASE, N])).

driver_url(selenium) -> ?SELENIUM_URL;
driver_url(chromedriver) -> ?CHROMEDRIVER_URL.

is_session(S, Session) ->
  get_session(S, Session) /= false.

is_headless(S, Session) ->
  browser(S, Session) == htmlunit.

has_tabs(S, Session) ->
  is_chrome(S, Session).

is_chrome(S, Session) ->
  browser(S, Session) == chrome.

is_firefox(S, Session) ->
  browser(S, Session) == firefox.

is_selenium(S, Session) ->
  driver(S, Session) == selenium.

browser(S, Session) ->
  (get_session(S, Session))#session.browser.

driver(S, Session) ->
  (get_session(S, Session))#session.driver.

n_windows(S, Session) ->
  length((get_session(S, Session))#session.windows).

res_ok(Res) ->
  case Res of
    {ok, _} -> true;
    ok -> true;
    _ -> Res
  end.

res_error(Res, Err) ->
  case Res of
    {error, Err2} ->
      match_err(Err, Err2);
    {error, Err2, _} ->
      match_err(Err, Err2);
    _ -> eq(Res, {error, Err})
  end.

%% To get some meaningful postcondition...
match_err(Err1, Err2) ->
  case match_err_(Err1, Err2) of
    true  -> true;
    false -> eq({error, Err2}, {error, Err1})
  end.

match_err_(Err, Err) -> true;
match_err_(Err, L) when is_list(L) ->
  lists:member(Err, L);
match_err_(Err, T) when is_tuple(T) ->
  match_err_(Err, tuple_to_list(T));
match_err_(_, _) -> false.
