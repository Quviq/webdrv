%%%-------------------------------------------------------------------
%%% @author Hans Svensson <hans.svensson@quviq.com>
%%% @copyright (C) 2013, Quviq AB
%%%
%%% @doc {@link gen_server} wrapper for a WebDriver session.
%%%
%%% A WebDriver session (normally the interaction with one browser instance) is associated
%%% to a <tt>SessionId</tt>, and the <tt>URI</tt> of the WebDriver server-side. It quickly
%%% gets painful to carry the id and the URL around, therefore this wrapper module was
%%% created. By using this module, each session is associated to a name (an {@link
%%% atom()}), simplifiying issuing a WebDriver command.
%%%
%%% The implementation is a {@link gen_server}, which keeps the SessionId and the URL (and
%%% a timeout) in its state. A typical interaction with this module could be:
%%%<pre>
%%% webdrv_session:start_session(test, Selenium, webdrv_cap:default_htmlunit(), 10000),
%%% webdrv_session:set_url(test, SomeUrl),
%%% PageSource = webdrv_session:get_page_source(test),
%%% io:format("Source: ~p\n", [PageSource]),
%%% webdrv_session:stop_session(test). </pre>
%%%
%%% The individual functions are not documented. They have type annotations, for more
%%% information on the particular functions, we refer to (<a
%%% href="https://code.google.com/p/selenium/wiki/JsonWireProtocol">The WebDriver
%%% Wire Protocol</a>).
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
-module(webdrv_session).

-include("../include/webdrv.hrl").

-behaviour(gen_server).

-define(TIMEOUT, 30000).

%% API
-export([execute/3, execute_async/3, get_screenshot/1, get_status/1,
         get_window_handle/1, get_window_handles/1,
         get_ime_available_engines/1, get_ime_active_engine/1, get_ime_activated/1,
         ime_deactivate/1, ime_activate/2,
         set_frame/2, set_window_focus/2, close_window/1, set_window_size/3,
         set_window_size/4, get_window_size/1, get_window_size/2, set_window_position/3,
         set_window_position/4, get_window_position/1, get_window_position/2,
         set_window_maximize/1, set_window_maximize/2, get_cookies/1, add_cookie/2,
         delete_cookies/1, delete_cookie/2, get_page_source/1, get_page_title/1,
         find_element/3, find_elements/3, get_active_element/1, find_element_rel/4,
         find_elements_rel/4, click_element/2, submit/2, get_text/2, send_value/3,
         send_keys/2, element_name/2, clear_element/2, is_selected_element/2,
         is_enabled_element/2, element_attribute/3, are_elements_equal/3,
         is_displayed_element/2, get_element_info/2,
         element_location/2, is_element_location_in_view/2, element_size/2,
         element_css_property/3, get_browser_orientation/1, set_browser_orientation/2,
         get_alert_text/1, set_alert_text/2, accept_alert/1, dismiss_alert/1, moveto/4,
         click/2, buttondown/2, buttonup/2, doubleclick/1, get_log/2,
         get_log_types/1, get_cache_status/1, set_timeout/3, set_async_script_timeout/2,
         set_implicit_wait_timeout/2, forward/1, back/1, refresh/1, set_url/2, get_url/1,
         start_session/3, start_session/4, stop_session/1, stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { url
               , capabilities
               , session_id
               , timeout}).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv start_session(Name, Url, DesiredCapabilities, null, 5000)
-spec start_session(atom(), url(), capability()) ->
                       {ok, pid()} | request_error().
start_session(Name, Url, DesiredCapabilities) ->
  start_session(Name, Url, DesiredCapabilities, null, 5000).

%% @equiv start_session(Name, Url, DesiredCapabilities, null, ConnTimeout)
-spec start_session(atom(), url(), capability(), number()) ->
                       {ok, pid()} | request_error().
start_session(Name, Url, DesiredCapabilities, ConnTimeout) ->
  start_session(Name, Url, DesiredCapabilities, null, ConnTimeout).

%% @doc
%% Starts the server (initializing a session)
%%
%% @end
-spec start_session(atom(), url(), capability(), capability(), number()) ->
                       {ok, pid()} | request_error().
start_session(Name, Url, DesiredCapabilities, RequiredCapabilities, ConnTimeout) ->
  case webdrv_wire:start_session(#webdrv_opts{ url = Url, timeout = ConnTimeout },
                                 DesiredCapabilities, RequiredCapabilities) of
    {ok, SessionId, Capabilities} ->
      gen_server:start_link({local, Name}, ?MODULE, [binary_to_list(SessionId), Url,
                                                     Capabilities, ConnTimeout], []);

    Err -> {error, Err}
  end.


-spec stop_session(atom()) -> ok.
stop_session(Name) ->
  _Res = gen_server:call(Name, stop_session),
  ok.

-spec stop(atom()) -> ok.
stop(Name) ->
  _Res = gen_server:call(Name, stop_session_no_cleanup),
  ok.

-spec get_status(atom()) -> {ok, json()} | request_error().
get_status(Name) ->
  mkRes(gen_server:call(Name, {get_status, sid}, ?TIMEOUT), fun(X) -> X end).

-spec set_timeout(atom(), string(), number()) -> ok | request_error().
set_timeout(Name, Type, Timeout) ->
  mkRes(gen_server:call(Name, {set_timeout, sid, bs(Type), Timeout}, ?TIMEOUT), ok).

-spec set_async_script_timeout(atom(), number()) -> ok | request_error().
set_async_script_timeout(Name, Timeout) ->
  mkRes(gen_server:call(Name, {set_async_script_timeout, sid, Timeout}, ?TIMEOUT), ok).

-spec set_implicit_wait_timeout(atom(), number()) -> ok | request_error().
set_implicit_wait_timeout(Name, Timeout) ->
  mkRes(gen_server:call(Name, {set_implicit_wait_timeout, sid, Timeout}, ?TIMEOUT), ok).

-spec get_window_handle(atom()) -> {ok, string()} | request_error().
get_window_handle(Name) ->
  mkRes(gen_server:call(Name, {get_window_handle, sid}, ?TIMEOUT), fun check_str/1).

-spec get_window_handles(atom()) -> {ok, [string()]} | request_error().
get_window_handles(Name) ->
  mkRes(gen_server:call(Name, {get_window_handles, sid}, ?TIMEOUT),
        fun(WHs) -> check_list(WHs, fun check_str/1) end).

-spec set_url(atom(), string()) -> ok | request_error().
set_url(Name, Url) ->
  mkRes(gen_server:call(Name, {set_url, sid, bs(Url)}, ?TIMEOUT), ok).

-spec get_url(atom()) -> {ok, string()} | request_error().
get_url(Name) ->
  mkRes(gen_server:call(Name, {get_url, sid}, ?TIMEOUT), fun check_str/1).

-spec forward(atom()) -> ok | request_error().
forward(Name) ->
  mkRes(gen_server:call(Name, {forward, sid}, ?TIMEOUT), ok).

-spec back(atom()) -> ok | request_error().
back(Name) ->
  mkRes(gen_server:call(Name, {back, sid}, ?TIMEOUT), ok).

-spec refresh(atom()) -> ok | request_error().
refresh(Name) ->
  mkRes(gen_server:call(Name, {refresh, sid}, ?TIMEOUT), ok).

%% Better for this function to work with pure json data!
-spec execute(atom(), jsonstr(), jsonlist()) -> {ok, json()} | request_error().
execute(Name, Script, Args) ->
  mkRes(gen_server:call(Name, {execute, sid, Script, Args}, ?TIMEOUT), fun(X) -> X end).

-spec execute_async(atom(), jsonstr(), jsonlist()) -> {ok, json()} | request_error().
execute_async(Name, Script, Args) ->
  mkRes(gen_server:call(Name, {execute_async, sid, Script, Args}, ?TIMEOUT), fun(X) -> X end).

-spec get_screenshot(atom()) -> {ok, [char()]} | request_error().
get_screenshot(Name) ->
  mkRes(gen_server:call(Name, {get_screenshot, sid}, ?TIMEOUT), fun check_str/1).

-spec get_ime_available_engines(atom()) -> {ok, [string()]} | request_error().
get_ime_available_engines(Name) ->
  mkRes(gen_server:call(Name, {get_ime_available_engines, sid}, ?TIMEOUT),
        fun(Es) -> check_list(Es, fun check_str/1) end).

-spec get_ime_active_engine(atom()) -> {ok, string()} | request_error().
get_ime_active_engine(Name) ->
  mkRes(gen_server:call(Name, {get_ime_active_engine, sid}, ?TIMEOUT), fun check_str/1).

-spec get_ime_activated(atom()) -> {ok, boolean()} | request_error().
get_ime_activated(Name) ->
  mkRes(gen_server:call(Name, {get_ime_activated, sid}, ?TIMEOUT),
        fun(B) -> check_bool(B) end).

-spec ime_deactivate(atom()) -> ok | request_error().
ime_deactivate(Name) ->
  mkRes(gen_server:call(Name, {ime_deactivate, sid}, ?TIMEOUT), ok).

-spec ime_activate(atom(), string()) -> ok | request_error().
ime_activate(Name, Engine) ->
  mkRes(gen_server:call(Name, {ime_activate, sid, bs(Engine)}, ?TIMEOUT), ok).

%% Uses raw json!?
-spec set_frame(atom(), json()) -> ok | request_error().
set_frame(Name, Frame) ->
  mkRes(gen_server:call(Name, {set_frame, sid, Frame}, ?TIMEOUT), ok).

-spec set_window_focus(atom(), string()) -> ok | request_error().
set_window_focus(Name, Window) ->
  mkRes(gen_server:call(Name, {set_window_focus, sid, bs(Window)}, ?TIMEOUT), ok).

-spec close_window(atom()) -> ok | request_error().
close_window(Name) ->
  mkRes(gen_server:call(Name, {close_window, sid}, ?TIMEOUT), ok).

-spec set_window_size(atom(), number(), number()) -> ok | request_error().
set_window_size(Name, Width, Height) ->
  mkRes(gen_server:call(Name, {set_window_size, sid, "current", Width, Height}, ?TIMEOUT), ok).

-spec set_window_size(atom(), string(), number(), number()) -> ok | request_error().
set_window_size(Name, WindowHandle, Width, Height) ->
  mkRes(gen_server:call(Name, {set_window_size, sid, WindowHandle, Width, Height}, ?TIMEOUT), ok).

-spec get_window_size(atom()) -> {ok, {number(), number()}} | request_error().
get_window_size(Name) ->
  get_window_size(Name, "current").

-spec get_window_size(atom(), string()) -> {ok, {number(), number()}} | request_error().
get_window_size(Name, WindowHandle) ->
  mkRes(gen_server:call(Name, {get_window_size, sid, WindowHandle}, ?TIMEOUT),
        fun({obj, Dict}) ->
            Width = proplists:get_value("width", Dict, 0),
            Height = proplists:get_value("height", Dict, 0),
            {Width, Height};
           (X) -> {type_error, exp_obj, X}
        end).

-spec set_window_position(atom(), number(), number()) -> ok | request_error().
set_window_position(Name, X, Y) ->
  mkRes(gen_server:call(Name, {set_window_position, sid, X, Y}, ?TIMEOUT), ok).

-spec set_window_position(atom(), string(), number(), number()) -> ok | request_error().
set_window_position(Name, WindowHandle, X, Y) ->
  mkRes(gen_server:call(Name, {set_window_position, sid, WindowHandle, X, Y}, ?TIMEOUT), ok).

-spec get_window_position(atom()) -> {ok, {number(), number()}} | request_error().
get_window_position(Name) ->
  get_window_position(Name, "current").

-spec get_window_position(atom(), string()) -> {ok, {number(), number()}} | request_error().
get_window_position(Name, WindowHandle) ->
  mkRes(gen_server:call(Name, {get_window_position, sid, WindowHandle}, ?TIMEOUT),
        fun({obj, Dict}) ->
            X = proplists:get_value("x", Dict, 0),
            Y = proplists:get_value("y", Dict, 0),
            {X, Y};
           (X) -> {type_error, exp_obj, X}
        end).

-spec set_window_maximize(atom()) -> ok | request_error().
set_window_maximize(Name) ->
  mkRes(gen_server:call(Name, {set_window_maximize, sid}, ?TIMEOUT), ok).

-spec set_window_maximize(atom(), string()) -> ok | request_error().
set_window_maximize(Name, WindowHandle) ->
  mkRes(gen_server:call(Name, {set_window_maximize, sid, WindowHandle}, ?TIMEOUT), ok).

-spec get_cookies(atom()) -> {ok, [cookie()]} | request_error().
get_cookies(Name) ->
  mkRes(gen_server:call(Name, {get_cookies, sid}, ?TIMEOUT), fun(X) -> X end).

-spec add_cookie(atom(), cookie()) -> ok | request_error().
add_cookie(Name, Cookie) ->
  mkRes(gen_server:call(Name, {add_cookie, sid, Cookie}, ?TIMEOUT), ok).

-spec delete_cookies(atom()) -> ok | request_error().
delete_cookies(Name) ->
  mkRes(gen_server:call(Name, {delete_cookies, sid}, ?TIMEOUT), ok).

-spec delete_cookie(atom(), string()) -> ok | request_error().
delete_cookie(Name, CookieName) ->
  mkRes(gen_server:call(Name, {delete_cookie, sid, CookieName}, ?TIMEOUT), ok).

-spec get_page_source(atom()) -> {ok, string()} | request_error().
get_page_source(Name) ->
  mkRes(gen_server:call(Name, {get_page_source, sid}, ?TIMEOUT), fun check_str/1).

-spec get_page_title(atom()) -> {ok, string()} | request_error().
get_page_title(Name) ->
  mkRes(gen_server:call(Name, {get_page_title, sid}, ?TIMEOUT), fun check_str/1).

-spec find_element(atom(), string(), string()) -> {ok, string()} | request_error().
find_element(Name, Strategy, Value) ->
  V = unicode:characters_to_binary(Value, unicode, utf8),
  mkRes(gen_server:call(Name, {find_element, sid, bs(Strategy), V}, ?TIMEOUT),
        fun parse_web_element/1).

-spec find_elements(atom(), string(), string()) -> {ok, [string()]} | request_error().
find_elements(Name, Strategy, Value) ->
  V = unicode:characters_to_binary(Value, unicode, utf8),
  mkRes(gen_server:call(Name, {find_elements, sid, bs(Strategy), V}, ?TIMEOUT),
        fun(Es) -> check_list(Es, fun parse_web_element/1) end).


-spec get_active_element(atom()) -> {ok, string()} | request_error().
get_active_element(Name) ->
  mkRes(gen_server:call(Name, {get_active_element, sid}, ?TIMEOUT),
        fun parse_web_element/1).

%% Currently undefined return value
-spec get_element_info(atom(), string()) -> {ok, json()} | request_error().
get_element_info(Name, ElementId) ->
  mkRes(gen_server:call(Name, {get_element_info, sid, ElementId}, ?TIMEOUT), fun(X) -> X end).

-spec find_element_rel(atom(), string(), string(), string()) ->
                          {ok, string()} | request_error().
find_element_rel(Name, ElementId, Strategy, Value) ->
  mkRes(gen_server:call(Name, {find_element_rel, sid, ElementId, bs(Strategy), bs(Value)}, ?TIMEOUT),
        fun parse_web_element/1).

-spec find_elements_rel(atom(), string(), string(), string()) ->
                          {ok, [string()]} | request_error().
find_elements_rel(Name, ElementId, Strategy, Value) ->
  mkRes(gen_server:call(Name, {find_elements_rel, sid, ElementId, bs(Strategy), bs(Value)}, ?TIMEOUT),
        fun(Es) -> check_list(Es, fun parse_web_element/1) end).

-spec click_element(atom(), string()) -> ok | request_error().
click_element(Name, ElementId) ->
  mkRes(gen_server:call(Name, {click_element, sid, ElementId}, ?TIMEOUT), ok).

-spec submit(atom(), string()) -> ok | request_error().
submit(Name, ElementId) ->
  mkRes(gen_server:call(Name, {submit, sid, ElementId}, ?TIMEOUT), ok).

%% Return type missing in spec!?
-spec get_text(atom(), string()) -> {ok, string()} | request_error().
get_text(Name, ElementId) ->
  mkRes(gen_server:call(Name, {get_text, sid, ElementId}, ?TIMEOUT), fun check_str/1).

-spec send_value(atom(), string(), string()) -> ok | request_error().
send_value(Name, ElementId, Value) ->
  mkRes(gen_server:call(Name, {send_value, sid, ElementId, bs(Value)}, ?TIMEOUT), ok).

-spec send_keys(atom(), string()) -> ok | request_error().
send_keys(Name, Value) ->
  mkRes(gen_server:call(Name, {send_keys, sid, bs(Value)}, ?TIMEOUT), ok).

-spec element_name(atom(), string()) -> {ok, string()} | request_error().
element_name(Name, ElementId) ->
  mkRes(gen_server:call(Name, {element_name, sid, ElementId}, ?TIMEOUT), ok).

-spec clear_element(atom(), string()) -> ok | request_error().
clear_element(Name, ElementId) ->
  mkRes(gen_server:call(Name, {clear_element, sid, ElementId}, ?TIMEOUT), ok).

-spec is_selected_element(atom(), string()) -> {ok, boolean()} | request_error().
is_selected_element(Name, ElementId) ->
  mkRes(gen_server:call(Name, {is_selected_element, sid, ElementId}, ?TIMEOUT),
        fun(X) -> check_bool(X) end).

-spec is_enabled_element(atom(), string()) -> {ok, boolean()} | request_error().
is_enabled_element(Name, ElementId) ->
  mkRes(gen_server:call(Name, {is_enabled_element, sid, ElementId}, ?TIMEOUT),
        fun(X) -> check_bool(X) end).

-spec element_attribute(atom(), string(), string()) ->
                           {ok, string() | null} | request_error().
element_attribute(Name, ElementId, EName) ->
  mkRes(gen_server:call(Name, {element_attribute, sid, ElementId, EName}, ?TIMEOUT),
        fun(X) -> type_or(check_str(X), check_null(X)) end).

-spec are_elements_equal(atom(), string(), string()) -> {ok, boolean()} | request_error().
are_elements_equal(Name, ElementId1, ElementId2) ->
  mkRes(gen_server:call(Name, {element_attribute, sid, ElementId1, ElementId2}, ?TIMEOUT),
        fun(X) -> check_bool(X) end).

-spec is_displayed_element(atom(), string()) -> {ok, boolean()} | request_error().
is_displayed_element(Name, ElementId) ->
  mkRes(gen_server:call(Name, {is_displayed_element, sid, ElementId}, ?TIMEOUT),
        fun(X) -> check_bool(X) end).

-spec element_location(atom(), string()) -> {ok, {number(), number()}} | request_error().
element_location(Name, ElementId) ->
  mkRes(gen_server:call(Name, {element_location, sid, ElementId}, ?TIMEOUT),
        fun({obj, Dict}) ->
            X = proplists:get_value("x", Dict, 0),
            Y = proplists:get_value("y", Dict, 0),
            {X, Y};
           (X) -> {type_error, exp_obj, X}
        end).

-spec is_element_location_in_view(atom(), string()) ->
                                     {ok, {number(), number()}} | request_error().
is_element_location_in_view(Name, ElementId) ->
  mkRes(gen_server:call(Name, {is_element_location_in_view, sid, ElementId}, ?TIMEOUT),
        fun({obj, Dict}) ->
            X = proplists:get_value("x", Dict, 0),
            Y = proplists:get_value("y", Dict, 0),
            {X, Y};
           (X) -> {type_error, exp_obj, X}
        end).

-spec element_size(atom(), string()) -> {ok, {number(), number()}} | request_error().
element_size(Name, ElementId) ->
  mkRes(gen_server:call(Name, {element_size, sid, ElementId}, ?TIMEOUT),
        fun({obj, Dict}) ->
            Width = proplists:get_value("width", Dict, 0),
            Height = proplists:get_value("height", Dict, 0),
            {Width, Height};
           (X) -> {type_error, exp_obj, X}
        end).

-spec element_css_property(atom(), string(), string()) -> {ok, string()} | request_error().
element_css_property(Name, ElementId, Prop) ->
  mkRes(gen_server:call(Name, {element_css_property, sid, ElementId, Prop}, ?TIMEOUT), fun check_str/1).

-spec get_browser_orientation(atom()) -> {ok, orientation()} | request_error().
get_browser_orientation(Name) ->
  mkRes(gen_server:call(Name, {get_browser_orientation, sid}, ?TIMEOUT), fun parse_orientation/1).

-spec set_browser_orientation(atom(), orientation()) -> ok | request_error().
set_browser_orientation(Name, Dir) ->
  mkRes(gen_server:call(Name, {set_browser_orientation, sid, js_orientation(Dir)}, ?TIMEOUT), ok).

-spec get_alert_text(atom()) -> {ok, string()} | request_error().
get_alert_text(Name) ->
  mkRes(gen_server:call(Name, {get_alert_text, sid}, ?TIMEOUT), fun check_str/1).

-spec set_alert_text(atom(), string()) -> ok | request_error().
set_alert_text(Name, Str) ->
  mkRes(gen_server:call(Name, {set_alert_text, sid, bs(Str)}, ?TIMEOUT), ok).

-spec accept_alert(atom()) -> ok | request_error().
accept_alert(Name) ->
  mkRes(gen_server:call(Name, {accept_alert, sid}, ?TIMEOUT), ok).

-spec dismiss_alert(atom()) -> ok | request_error().
dismiss_alert(Name) ->
  mkRes(gen_server:call(Name, {dismiss_alert, sid}, ?TIMEOUT), ok).

-spec moveto(atom(), string(), number(), number()) -> ok | request_error().
moveto(Name, Elem, XOffSet, YOffSet) ->
  mkRes(gen_server:call(Name, {moveto, sid, bs(Elem), XOffSet, YOffSet}, ?TIMEOUT), ok).

-spec click(atom(), button()) -> ok | request_error().
click(Name, Button) ->
  mkRes(gen_server:call(Name, {click, sid, js_button(Button)}, ?TIMEOUT), ok).

-spec buttondown(atom(), button()) -> ok | request_error().
buttondown(Name, Button) ->
  mkRes(gen_server:call(Name, {buttondown, sid, js_button(Button)}, ?TIMEOUT), ok).

-spec buttonup(atom(), button()) -> ok | request_error().
buttonup(Name, Button) ->
  mkRes(gen_server:call(Name, {buttonup, sid, js_button(Button)}, ?TIMEOUT), ok).

-spec doubleclick(atom()) -> ok | request_error().
doubleclick(Name) ->
  mkRes(gen_server:call(Name, {doubleclick, sid}, ?TIMEOUT), ok).

%% SKIP Touch, Geo and Local storage for now...

-spec get_log(atom(), string()) -> {ok, log()} | request_error().
get_log(Name, LogType) ->
  mkRes(gen_server:call(Name, {get_log, sid, bs(LogType)}, ?TIMEOUT),
       fun(LogEntries) -> check_list(LogEntries, fun parse_log_entry/1) end).

-spec get_log_types(atom()) -> {ok, [string()]} | request_error().
get_log_types(Name) ->
  mkRes(gen_server:call(Name, {get_log_types, sid}, ?TIMEOUT),
       fun(Types) -> check_list(Types, fun check_str/1) end).

-spec get_cache_status(atom()) -> {ok, cache_status()} | request_error().
get_cache_status(Name) ->
  mkRes(gen_server:call(Name, {get_cache_status, sid}, ?TIMEOUT),
       fun parse_cache_status/1).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([SessionId, Url, Capabilities, ConnTimeout]) ->
  {ok, #state{ url = Url, capabilities = Capabilities,
               timeout = ConnTimeout, session_id = SessionId } }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({Fun, sid}, _From, S) ->
  Reply = webdrv_wire:Fun(mkOpts(S)),
  {reply, Reply, S};
handle_call({Fun, sid, Arg1}, _From, S) ->
  Reply = webdrv_wire:Fun(mkOpts(S), Arg1),
  {reply, Reply, S};
handle_call({Fun, sid, Arg1, Arg2}, _From, S) ->
  Reply = webdrv_wire:Fun(mkOpts(S), Arg1, Arg2),
  {reply, Reply, S};
handle_call({Fun, sid, Arg1, Arg2, Arg3}, _From, S) ->
  Reply = webdrv_wire:Fun(mkOpts(S), Arg1, Arg2, Arg3),
  {reply, Reply, S};
handle_call(stop_session, _From, S) ->
  Reply = webdrv_wire:stop_session(mkOpts(S)),
  {stop, normal, Reply, S#state{ session_id = undefined, url = undefined }};
handle_call(stop_session_no_cleanup, _From, S) ->
  {stop, normal, ok, S};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mkOpts(#state{ session_id = SId, timeout = T, url = Url }) ->
  #webdrv_opts{ url = Url, timeout = T, session_id = SId }.

parse_web_element({obj, [{"ELEMENT", BElem}]}) ->
  binary_to_list(BElem);
parse_web_element(X) -> {type_error, exp_element_obj, X}.


-spec parse_log_entry(jsonobj()) -> log_entry().
parse_log_entry({obj, Dict}) ->
  Timestamp = proplists:get_value("timestamp", Dict, -1),
  Level     = proplists:get_value("level", Dict, ""),
  Msg       = proplists:get_value("message", Dict, ""),
  {Timestamp, Level, Msg};
parse_log_entry(X) -> {type_error, exp_logentry_obj, X}.


-spec parse_cache_status(number()) -> cache_status().
parse_cache_status(0) -> uncached;
parse_cache_status(1) -> idle;
parse_cache_status(2) -> checking;
parse_cache_status(3) -> downloading;
parse_cache_status(4) -> update_ready;
parse_cache_status(5) -> obsolete;
parse_cache_status(X) -> {type_error, exp_cache_status, X}.

-spec parse_orientation(binary()) -> orientation().
parse_orientation(<<"PORTRAIT">>) -> portrait;
parse_orientation(<<"LANDSCAPE">>) -> landscape;
parse_orientation(X) -> {type_error, exp_orientation, X}.

bs(String) when is_list(String) ->
%  unicode:characters_to_binary(String, unicode, utf8);
  list_to_binary(String);
bs(BinString) when is_binary(BinString) ->
  BinString;
%% bs(X) when is_number(X) -> X;
%% bs(true)  -> true;
%% bs(false) -> false;
bs(null)  -> null.

-spec js_button(button()) -> number().
js_button(left)   -> 0;
js_button(middle) -> 1;
js_button(right)  -> 2;
js_button(N)      -> N.

-spec js_orientation(orientation()) -> binary().
js_orientation(portrait) -> <<"PORTRAIT">>;
js_orientation(landscape) -> <<"LANDSCAPE">>.

check_str(BinString) when is_binary(BinString) ->
  unicode:characters_to_list(BinString);
check_str(X) ->
  {type_error, exp_binstring, X}.

check_bool(X) when is_boolean(X) -> X;
check_bool(X) -> {type_error, exp_boolean, X}.

check_null(null) -> null;
check_null(X) -> {type_error, exp_null, X}.

check_list(X, F) when is_list(X) ->  mkList(X, F, []);
check_list(X, _) -> {type_error, exp_array, X}.

mkList([], _, Xs) -> lists:reverse(Xs);
mkList([X | Xs], F, Ys) ->
  case F(X) of
    E = {type_error, _E, _X} -> E;
    R -> mkList(Xs, F, [R | Ys])
  end.

type_or({type_error, E1, X}, {type_error, E2, _X}) ->
  {type_error, list_to_atom(lists:concat([E1, "_or_", E2])), X};
type_or({type_error, _, _}, X) -> X;
type_or(X, _) -> X.

mkRes({ok, _SId, _Res}, ok) -> ok;
mkRes({ok, _SId, Res}, Fun) ->
  case Fun(Res) of
    E = {type_error, _E, _X} -> E;
    X -> {ok, X}
  end;
mkRes(Res, _Fun) -> Res.
