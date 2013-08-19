%%%-------------------------------------------------------------------
%%% @author Hans Svensson <hans.svensson@quviq.com>
%%% @copyright (C) 2013, Quviq AB.
%%%
%%% @doc WebDriver Wire Protocol
%%%
%%% The individual functions are not documented. They have type annotations, for more
%%% information on the particular functions, we refer to (<a
%%% href="https://code.google.com/p/selenium/wiki/JsonWireProtocol">The WebDriver
%%% Wire Protocol</a>).
%%%
%%% The errors reported are slightly more advanced that the basic error reporting
%%% prescribed in the protocol description; see the type {@link request_error()}.
%%%
%%% <h2>License</h2>
%%% <pre>
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
%%% SOFTWARE. </pre>
%%% @end
%%%-------------------------------------------------------------------

-module(webdrv_wire).

-export([get_status/1, start_session/2, start_session/3,
         get_sessions/1, session/1, stop_session/1,
         set_timeout/3, set_async_script_timeout/2, set_implicit_wait_timeout/2,
         get_window_handle/1, get_window_handles/1, get_url/1,
         set_url/2, forward/1, back/1,
         refresh/1, execute/3, execute_async/3,
         get_screenshot/1, get_ime_available_engines/1, get_ime_active_engine/1,
         get_ime_activated/1, ime_deactivate/1, ime_activate/2,
         set_frame/2, set_window_focus/2, close_window/1,
         set_window_size/3, set_window_size/4, get_window_size/1,
         get_window_size/2, set_window_position/3, set_window_position/4,
         get_window_position/1, get_window_position/2, set_window_maximize/1,
         set_window_maximize/2, get_cookies/1, add_cookie/2,
         delete_cookies/1, delete_cookie/2, get_page_source/1,
         get_page_title/1, find_element/3, find_elements/3,
         get_active_element/1, get_element_info/2, find_element_rel/4,
         find_elements_rel/4, click_element/2, submit/2,
         get_text/2, send_value/3, send_keys/2,
         element_name/2, clear_element/2, is_selected_element/2,
         is_enabled_element/2, element_attribute/3, are_elements_equal/3,
         is_displayed_element/2, element_location/2, is_element_location_in_view/2,
         element_size/2, element_css_property/3, get_browser_orientation/1,
         set_browser_orientation/2, get_alert_text/1, set_alert_text/2,
         accept_alert/1, dismiss_alert/1, moveto/4,
         click/2, buttondown/2, buttonup/2,
         doubleclick/1, get_log/2, get_log_types/1, get_cache_status/1]).

-include("../include/webdrv.hrl").

%% Helper functions for options
session_id(#webdrv_opts{ session_id = SId }) -> SId.
url(#webdrv_opts{ url = Url })               -> Url.
timeout(#webdrv_opts{ timeout = Timeout })   -> Timeout.

%% API
get_status(Opts) ->
  do_get_cmd(Opts, "status").

-spec start_session(#webdrv_opts{}, capability()) -> request_res().
start_session(Opts, Capability) ->
  start_session(Opts, Capability, null).

-spec start_session(#webdrv_opts{}, capability(), capability()) -> request_res().
start_session(Opts, Desired, Required) ->
  Params = [{desiredCapabilities, webdrv_cap:to_json(Desired)},
            {requiredCapabilities, webdrv_cap:to_json(Required)}],
  do_post_cmd(Opts, "session", Params).

-spec get_sessions(#webdrv_opts{}) -> request_res().
get_sessions(Opts) ->
  do_get_cmd(Opts, "sessions").

-spec session(#webdrv_opts{}) -> request_res().
session(Opts) ->
  do_get_cmd(Opts, "session/" ++ session_id(Opts)).

-spec stop_session(#webdrv_opts{}) -> request_res().
stop_session(Opts) ->
  do_delete_cmd(Opts, "session/" ++ session_id(Opts)).

-spec set_timeout(#webdrv_opts{}, term(), term()) -> request_res().
set_timeout(Opts, TimeoutType, Timeout) ->
  Params = [{type, TimeoutType}, {ms, Timeout}],
  do_post_scmd(Opts, "timeouts", Params).

-spec set_async_script_timeout(#webdrv_opts{}, number()) -> request_res().
set_async_script_timeout(Opts, Timeout) ->
  do_post_scmd(Opts, "timeouts/async_script", [{ms, Timeout}]).

-spec set_implicit_wait_timeout(#webdrv_opts{}, number()) -> request_res().
set_implicit_wait_timeout(Opts, Timeout) ->
  do_post_scmd(Opts, "timeouts/implicit_wait", [{ms, Timeout}]).

-spec get_window_handle(#webdrv_opts{}) -> request_res().
get_window_handle(Opts) ->
  do_get_scmd(Opts, "window_handle").

-spec get_window_handles(#webdrv_opts{}) -> request_res().
get_window_handles(Opts) ->
  do_get_scmd(Opts, "window_handles").

-spec get_url(#webdrv_opts{}) -> request_res().
get_url(Opts) ->
  do_get_scmd(Opts, "url").

-spec set_url(#webdrv_opts{}, jsonstr()) -> request_res().
set_url(Opts, Url) ->
  do_post_scmd(Opts, "url", [{url, Url}]).

-spec forward(#webdrv_opts{}) -> request_res().
forward(Opts) ->
  do_post_scmd(Opts, "forward", []).

-spec back(#webdrv_opts{}) -> request_res().
back(Opts) ->
  do_post_scmd(Opts, "back", []).

-spec refresh(#webdrv_opts{}) -> request_res().
refresh(Opts) ->
  do_post_scmd(Opts, "refresh", []).

-spec execute(#webdrv_opts{}, jsonstr(), jsonlist()) -> request_res().
execute(Opts, Script, Args) ->
  do_post_scmd(Opts, "execute", [{script, Script}, {args, Args}]).

-spec execute_async(#webdrv_opts{}, jsonstr(), jsonlist()) -> request_res().
execute_async(Opts, Script, Args) ->
  do_post_scmd(Opts, "execute_async", [{script, Script}, {args, Args}]).

-spec get_screenshot(#webdrv_opts{}) -> request_res().
get_screenshot(Opts) ->
  do_get_scmd(Opts, "screenshot").

-spec get_ime_available_engines(#webdrv_opts{}) -> request_res().
get_ime_available_engines(Opts) ->
  do_get_scmd(Opts, "ime/available_engines").

-spec get_ime_active_engine(#webdrv_opts{}) -> request_res().
get_ime_active_engine(Opts) ->
  do_get_scmd(Opts, "ime/active_engine").

-spec get_ime_activated(#webdrv_opts{}) -> request_res().
get_ime_activated(Opts) ->
  do_get_scmd(Opts, "ime/activated").

-spec ime_deactivate(#webdrv_opts{}) -> request_res().
ime_deactivate(Opts) ->
  do_post_scmd(Opts, "ime/deactivate", []).

-spec ime_activate(#webdrv_opts{}, jsonstr()) -> request_res().
ime_activate(Opts, Engine) ->
  do_post_scmd(Opts, "ime/activate", [{engine, Engine}]).

-spec set_frame(#webdrv_opts{}, frame_id()) -> request_res().
set_frame(Opts, Frame) ->
  do_post_scmd(Opts, "frame", [{id, Frame}]).

-spec set_window_focus(#webdrv_opts{}, jsonstr()) -> request_res().
set_window_focus(Opts, Window) ->
  do_post_scmd(Opts, "window", [{name, Window}]).

-spec close_window(#webdrv_opts{}) -> request_res().
close_window(Opts) ->
  do_delete_scmd(Opts, "window").

-spec set_window_size(#webdrv_opts{}, number(), number()) -> request_res().
set_window_size(Opts, Width, Height) ->
  set_window_size(Opts, "current", Width, Height).

-spec set_window_size(#webdrv_opts{}, jsonstr(), number(), number()) -> request_res().
set_window_size(Opts, WindowHandle, Width, Height) ->
  do_post_scmd(Opts, "window/" ++ WindowHandle ++ "/size",
               [{width, Width}, {height, Height}]).

-spec get_window_size(#webdrv_opts{}) -> request_res().
get_window_size(Opts) ->
  get_window_size(Opts, "current").

-spec get_window_size(#webdrv_opts{}, jsonstr()) -> request_res().
get_window_size(Opts, WindowHandle) ->
  do_get_scmd(Opts, "window/" ++ WindowHandle ++ "/size").

-spec set_window_position(#webdrv_opts{}, number(), number()) -> request_res().
set_window_position(Opts, X, Y) ->
  set_window_position(Opts, "current", X, Y).

-spec set_window_position(#webdrv_opts{}, jsonstr(), number(), number()) -> request_res().
set_window_position(Opts, WindowHandle, X, Y) ->
  do_post_scmd(Opts, "window/" ++ WindowHandle ++ "/position", [{x, X}, {y, Y}]).

-spec get_window_position(#webdrv_opts{}) -> request_res().
get_window_position(Opts) ->
  get_window_position(Opts, "current").

-spec get_window_position(#webdrv_opts{}, jsonstr()) -> request_res().
get_window_position(Opts, WindowHandle) ->
  do_get_scmd(Opts, "window/" ++ WindowHandle ++ "/position").

-spec set_window_maximize(#webdrv_opts{}) -> request_res().
set_window_maximize(Opts) ->
  set_window_maximize(Opts, "current").

-spec set_window_maximize(#webdrv_opts{}, jsonstr()) -> request_res().
set_window_maximize(Opts, WindowHandle) ->
  do_post_scmd(Opts, "window/" ++ WindowHandle ++ "/maximize", []).

-spec get_cookies(#webdrv_opts{}) -> request_res().
get_cookies(Opts) ->
  do_get_scmd(Opts, "cookie").

-spec add_cookie(#webdrv_opts{}, jsonobj()) -> request_res().
add_cookie(Opts, Cookie) ->
  do_post_scmd(Opts, "cookie", [{cookie, Cookie}]).

-spec delete_cookies(#webdrv_opts{}) -> request_res().
delete_cookies(Opts) ->
  do_delete_scmd(Opts, "cookie").

-spec delete_cookie(#webdrv_opts{}, jsonstr()) -> request_res().
delete_cookie(Opts, Name) ->
  do_delete_scmd(Opts, "cookie/" ++ Name).

-spec get_page_source(#webdrv_opts{}) -> request_res().
get_page_source(Opts) ->
  do_get_scmd(Opts, "source").

-spec get_page_title(#webdrv_opts{}) -> request_res().
get_page_title(Opts) ->
  do_get_scmd(Opts, "title").

-spec find_element(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
find_element(Opts, Strategy, Value) ->
  do_post_scmd(Opts, "element", [{using, Strategy}, {value, Value}]).

-spec find_elements(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
find_elements(Opts, Strategy, Value) ->
  do_post_scmd(Opts, "elements", [{using, Strategy}, {value, Value}]).

-spec get_active_element(#webdrv_opts{}) -> request_res().
get_active_element(Opts) ->
  do_post_scmd(Opts, "element/active", []).

%% Currently undefined
-spec get_element_info(#webdrv_opts{}, jsonstr()) -> request_res().
get_element_info(Opts, ElementId) ->
  do_get_scmd(Opts, "element/" ++ ElementId).

-spec find_element_rel(#webdrv_opts{}, jsonstr(), jsonstr(), jsonstr()) -> request_res().
find_element_rel(Opts, ElementId, Strategy, Value) ->
  do_post_ecmd(Opts, ElementId, "element", [{using, Strategy}, {value, Value}]).

-spec find_elements_rel(#webdrv_opts{}, jsonstr(), jsonstr(), jsonstr()) -> request_res().
find_elements_rel(Opts, ElementId, Strategy, Value) ->
  do_post_ecmd(Opts, ElementId, "elements", [{using, Strategy}, {value, Value}]).

-spec click_element(#webdrv_opts{}, jsonstr()) -> request_res().
click_element(Opts, ElementId) ->
  do_post_ecmd(Opts, ElementId, "click", []).

-spec submit(#webdrv_opts{}, jsonstr()) -> request_res().
submit(Opts, ElementId) ->
  do_post_ecmd(Opts, ElementId, "submit", []).

-spec get_text(#webdrv_opts{}, jsonstr()) -> request_res().
get_text(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "text").

-spec send_value(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
send_value(Opts, ElementId, Value) ->
  do_post_ecmd(Opts, ElementId, "value", [{value, [Value]}]).

-spec send_keys(#webdrv_opts{}, jsonstr()) -> request_res().
send_keys(Opts, Value) ->
  do_post_scmd(Opts, "keys", [{value, Value}]).

-spec element_name(#webdrv_opts{}, jsonstr()) -> request_res().
element_name(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "name").

-spec clear_element(#webdrv_opts{}, jsonstr()) -> request_res().
clear_element(Opts, ElementId) ->
  do_post_ecmd(Opts, ElementId, "name", []).

-spec is_selected_element(#webdrv_opts{}, jsonstr()) -> request_res().
is_selected_element(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "selected").

-spec is_enabled_element(#webdrv_opts{}, jsonstr()) -> request_res().
is_enabled_element(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "enabled").

-spec element_attribute(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
element_attribute(Opts, ElementId, Name) ->
  do_get_ecmd(Opts, ElementId, "attribute/" ++ Name).

-spec are_elements_equal(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
are_elements_equal(Opts, ElementId1, ElementId2) ->
  do_get_ecmd(Opts, ElementId1, "equals/" ++ ElementId2).

-spec is_displayed_element(#webdrv_opts{}, jsonstr()) -> request_res().
is_displayed_element(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "displayed").

-spec element_location(#webdrv_opts{}, jsonstr()) -> request_res().
element_location(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "location").

-spec is_element_location_in_view(#webdrv_opts{}, jsonstr()) -> request_res().
is_element_location_in_view(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "location_in_view").

-spec element_size(#webdrv_opts{}, jsonstr()) -> request_res().
element_size(Opts, ElementId) ->
  do_get_ecmd(Opts, ElementId, "size").

-spec element_css_property(#webdrv_opts{}, jsonstr(), jsonstr()) -> request_res().
element_css_property(Opts, ElementId, Prop) ->
  do_get_ecmd(Opts, ElementId, "css/" ++ Prop).

-spec get_browser_orientation(#webdrv_opts{}) -> request_res().
get_browser_orientation(Opts) ->
  do_get_scmd(Opts, "orientation").

-spec set_browser_orientation(#webdrv_opts{}, jsonstr()) -> request_res().
set_browser_orientation(Opts, Dir) ->
  do_post_scmd(Opts, "orientation", [{orientation, Dir}]).

-spec get_alert_text(#webdrv_opts{}) -> request_res().
get_alert_text(Opts) ->
  do_get_scmd(Opts, "alert_text").

-spec set_alert_text(#webdrv_opts{}, jsonstr()) -> request_res().
set_alert_text(Opts, Str) ->
  do_post_scmd(Opts, "alert_text", [{text, Str}]).

-spec accept_alert(#webdrv_opts{}) -> request_res().
accept_alert(Opts) ->
  do_post_scmd(Opts, "accept_alert", []).

-spec dismiss_alert(#webdrv_opts{}) -> request_res().
dismiss_alert(Opts) ->
  do_post_scmd(Opts, "dismiss_alert", []).

-spec moveto(#webdrv_opts{}, jsonstr(), number(), number()) -> request_res().
moveto(Opts, Elem, XOffSet, YOffSet) ->
  do_post_scmd(Opts, "moveto",
               [{element, Elem}, {xoffset, XOffSet}, {yoffset, YOffSet}]).

-spec click(#webdrv_opts{}, number()) -> request_res().
click(Opts, Button) ->
  do_post_scmd(Opts, "click", [{button, Button}]).

-spec buttondown(#webdrv_opts{}, number()) -> request_res().
buttondown(Opts, Button) ->
  do_post_scmd(Opts, "buttondown", [{button, Button}]).

-spec buttonup(#webdrv_opts{}, number()) -> request_res().
buttonup(Opts, Button) ->
  do_post_scmd(Opts, "buttonup", [{button, Button}]).

-spec doubleclick(#webdrv_opts{}) -> request_res().
doubleclick(Opts) ->
  do_post_scmd(Opts, "doubleclick", []).

%% SKIP Touch, Geo and Local storage for now...

-spec get_log(#webdrv_opts{}, jsonstr()) -> request_res().
get_log(Opts, LogType) ->
  do_post_scmd(Opts, "log", [{type, LogType}]).

-spec get_log_types(#webdrv_opts{}) -> request_res().
get_log_types(Opts) ->
  do_get_scmd(Opts, "log/types").

-spec get_cache_status(#webdrv_opts{}) -> request_res().
get_cache_status(Opts) ->
  do_get_scmd(Opts, "application_cache/status").


%% -------------
do_post_ecmd(Opts, ElementId, Cmd, Params) ->
  do_post_scmd(Opts,  "element/" ++ ElementId ++ "/" ++ Cmd, Params).

do_get_ecmd(Opts, ElementId, Cmd) ->
  do_get_scmd(Opts, "element/" ++ ElementId ++ "/" ++ Cmd).

do_post_scmd(Opts, Cmd, Params) ->
  do_post_cmd(Opts, "session/" ++ session_id(Opts) ++ "/" ++ Cmd, Params).

do_get_scmd(Opts, Cmd) ->
  do_get_cmd(Opts, "session/" ++ session_id(Opts) ++ "/" ++ Cmd).

do_post_cmd(Opts, Cmd, Params) ->
  URL = url(Opts) ++ Cmd,
  do_post(Opts, URL, {obj, Params}).

do_get_cmd(Opts, Cmd) ->
  do_get(Opts, url(Opts) ++ Cmd).

do_delete_scmd(Opts, Cmd) ->
  do_delete_cmd(Opts, "session/" ++ session_id(Opts) ++ "/" ++ Cmd).

do_delete_cmd(Opts, Cmd) ->
  do_delete(Opts, url(Opts) ++ Cmd).

%% HTML / HTTP functions
do_post(Opts, Url, JSONParams) ->
  {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Url),
  JSON = json:encode(JSONParams),
  Len  = length(JSON),
  request(Opts, post,
          {Url,
           [{"Content-Length", integer_to_list(Len)},
            {"Content-Type", json:mime_type()},
            {"host", Host ++ ":" ++ integer_to_list(Port)},
            {"connection", "keep-alive"}
           ],
           json:mime_type(),
           JSON}).

do_get(Opts, Url) ->
  request(Opts, get, {Url, [{"Accept", "application/json"}]}).

do_delete(Opts, URL) ->
  request(Opts, delete, {URL, []}).

-spec request(#webdrv_opts{}, httpc:method(), httpc:request()) ->
                 {ok, session_id(), jsonobj()} | request_error().
request(Opts, Method, Request) ->
  %% io:format("REQ: ~p\n", [{Method, Request}]),
  Res = httpc_request_bug_fix(Opts, Method, Request),
  case parse_httpc_result(Res) of
    {error, {txt, Err}} ->
      {html_error, Err};
    {error, {json, JErr}} ->
      {json_error, JErr};
    {cmd_fail, {ok, JSON}} ->
      {cmd_error, JSON};
    {cmd_fail, {error, {json, JErr}}} ->
      {json_error, JErr}; %% This one could be discussed
    ok -> {ok, null, {obj, []}};
    {ok, JsonTerm} -> check_json_response(JsonTerm)
  end.

check_json_response(JsonTerm) ->
  case parse_response_json(JsonTerm) of
    {0, SessId, Value} ->
      {ok, SessId, Value};
    {N, SessId, _Value} ->
      {error, wire_error(N), SessId};
    {error, JErr} ->
      {json_error, JErr}
  end.

parse_httpc_result({ok, Result}) ->
  %% io:format("Res: ~p\n", [Result]),
  case Result of
    {{_Vsn, Code, Reason}, Hdrs, Body} ->
      if Code == 200 ->
          json_decode(Body);
         Code == 204 -> %% No Content
          ok;
         (Code >= 400 andalso Code < 500) orelse Code == 501 ->
          {error, {txt, Body}};
         Code == 500 -> json_decode(Body);
         true ->
          case proplists:get_value("content-type", Hdrs, undefined)
                 == json:mime_type() of
            true  -> {cmd_fail, json_decode(Body)};
            false -> {error, {txt, lists:concat(
                                     ["Incorrect response ", Code, " - ", Reason])}}
          end
      end;
    {Code, Body} ->
      {error, {txt, lists:concat(["Illformed response ", Code, " - ", Body])}};
    _ ->
      {error, {txt, "Illformed response, expected normal response got just request_id"}}
  end;
parse_httpc_result({error, Reason}) ->
  {error, {txt, Reason}}.

json_decode(Body) ->
  case json:decode(Body) of
    {ok, Json, []} -> {ok, Json};
    {ok, _PJson, Rest} ->
      {error, {json, "Only partial decode possible, remaining: " ++ Rest}};
    {error, Reason} -> {error, {json, Reason}}
  end.

parse_response_json(JSON) ->
  case JSON of
    {obj, Dict} ->
      SessId = proplists:get_value("sessionId", Dict, null),
      Status = proplists:get_value("status", Dict, -1),
      Value  = proplists:get_value("value", Dict, none),
      if Status < 0 ->
          {error, "JSON object contained no status field"};
         Value == none ->
          {error, "JSON object contained no value"};
         true ->
          {Status, SessId, Value}
      end;
    _ ->
      {error, "JSON response is not of object type"}
  end.

%% WIRE Protocol Errors
-spec wire_error(integer()) -> {atom(), string()}.
wire_error(6)  -> {'NoSuchDriver'              , "A session is either terminated or not started"};
wire_error(7)  -> {'NoSuchElement'             , "An element could not be located on the page using the given search parameters."};
wire_error(8)  -> {'NoSuchFrame'               , "A request to switch to a frame could not be satisfied because the frame could not be found."};
wire_error(9)  -> {'UnknownCommand'            , "The requested resource could not be found, or a request was received using an HTTP method that is not supported by the mapped resource."};
wire_error(10) -> {'StaleElementReference'     , "An element command failed because the referenced element is no longer attached to the DOM."};
wire_error(11) -> {'ElementNotVisible'         , "An element command could not be completed because the element is not visible on the page."};
wire_error(12) -> {'InvalidElementState'       , "An element command could not be completed because the element is in an invalid state (e.g. attempting to click a disabled element)."};
wire_error(13) -> {'UnknownError'              , "An unknown server-side error occurred while processing the command."};
wire_error(15) -> {'ElementIsNotSelectable'    , "An attempt was made to select an element that cannot be selected."};
wire_error(17) -> {'JavaScriptError'           , "An error occurred while executing user supplied JavaScript."};
wire_error(19) -> {'XPathLookupError'          , "An error occurred while searching for an element by XPath."};
wire_error(21) -> {'Timeout'                   , "An operation did not complete before its timeout expired."};
wire_error(23) -> {'NoSuchWindow'              , "A request to switch to a different window could not be satisfied because the window could not be found."};
wire_error(24) -> {'InvalidCookieDomain'       , "An illegal attempt was made to set a cookie under a different domain than the current page."};
wire_error(25) -> {'UnableToSetCookie'         , "A request to set a cookie's value could not be satisfied."};
wire_error(26) -> {'UnexpectedAlertOpen'       , "A modal dialog was open, blocking this operation."};
wire_error(27) -> {'NoAlertOpenError'          , "An attempt was made to operate on a modal dialog when one was not open."};
wire_error(28) -> {'ScriptTimeout'             , "A script did not complete before its timeout expired."};
wire_error(29) -> {'InvalidElementCoordinates' , "The coordinates provided to an interactions operation are invalid."};
wire_error(30) -> {'IMENotAvailable'           , "IME was not available."};
wire_error(31) -> {'IMEEngineActivationFailed' , "An IME engine could not be started."};
wire_error(32) -> {'InvalidSelector'           , "Argument was an invalid selector (e.g. XPath/CSS)."};
wire_error(33) -> {'SessionNotCreatedException', "A new session could not be created."};
wire_error(34) -> {'MoveTargetOutOfBounds'     , "Target provided for a move action is out of bounds."}.


%% BUG in httpc:request, does not follow 303 when POST:ing
%% in R16 it correctly follows 303 redirects, but fails to
%% get the relative location correct... Sigh...
%% TODO: Make the fix more general??
%%  (non-relative location etc...)
httpc_request_bug_fix(Opts, post, Request={_Url, Headers, _, _}) ->
  Url = "http://" ++ proplists:get_value("host", Headers) ++  "/",
  Timeout = {timeout, timeout(Opts)},
  case httpc:request(post, Request,
                     [Timeout, {autoredirect, false}],
                     [{headers_as_is, true}]) of
    _Res = {ok, {{_, 303, _}, OutHdrs, _Body}} ->
      NewLoc = proplists:get_value("location", OutHdrs, " "),
      Res = httpc_request_bug_fix(Opts, get, {Url ++ tl(NewLoc), []}),
      Res;
    % Fix selenium
    _Res = {ok, {{_, 302, _}, OutHdrs, _Body}} ->
      Redirect = proplists:get_value("location", OutHdrs, ""),
      httpc:request(get, {Redirect, [{"Accept", "application/json"}]}, [Timeout], []);
    % Fix ios-driver
    _Res = {ok, {{_, 301, _}, OutHdrs, _Body}} ->
      Redirect = proplists:get_value("location", OutHdrs, ""),
      httpc:request(get, {Redirect, [{"Accept", "application/json"}]}, [Timeout], []);
    Res ->
      Res
  end;
httpc_request_bug_fix(Opts, Method, Request) ->
  httpc:request(Method, Request, [{timeout, timeout(Opts)}, {autoredirect, true}], []).

