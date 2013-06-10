%%%-------------------------------------------------------------------
%%% @author Hans Svensson <hans.svensson@quviq.com>
%%% @copyright (C) 2013, Quviq AB
%%%
%%% @doc
%%% Handling of capabilities for WebDriver protocol
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

-module(webdrv_cap).

-include("../include/webdrv.hrl").

-export([default/0, default_browser/1,
         to_json/1,
         default_firefox/0, default_chrome/0, default_htmlunit/0]).

%% @doc Return the default capability record.
-spec default() -> #capability{}.
default() ->
  #capability{ }.

%% @hidden Just for the tests
-spec default_browser(string() | atom()) -> #capability{}.
default_browser(Browser) when is_atom(Browser) ->
  (default())#capability{ browserName = list_to_binary(atom_to_list(Browser))};
default_browser(Browser) when is_list(Browser) ->
  (default())#capability{ browserName = list_to_binary(Browser)}.

%% @doc Return the default capability with browser set to <tt>chrome</tt>.
-spec default_chrome() -> #capability{}.
default_chrome() ->
  (default())#capability{ browserName = <<"chrome">>, version = <<"">> }.

%% @doc Return the default capability with browser set to <tt>htmlunit</tt>.
-spec default_htmlunit() -> #capability{}.
default_htmlunit() ->
  (default())#capability{ browserName = <<"htmlunit">>, version = <<"">> }.

%% @doc Return the default capability with browser set to <tt>firefox</tt>.
-spec default_firefox() -> #capability{}.
default_firefox() ->
  (default())#capability{ browserName = <<"firefox">> }.

%% @doc Convert a capability (possibly <tt>null</tt>) to JSON format. Function is
%% idempotent, i.e. converting an already converted object is fine.
-spec to_json(capability()) -> jsonobj() | null.
to_json(C = #capability{}) ->
  {obj,
   [ {Field, Arg1} || {Field, Arg1, Arg2} <- lists:zip3(record_info(fields, capability),
                                                        tl(tuple_to_list(C)),
                                                        tl(tuple_to_list(#capability{}))),
                      (Arg1 =/= Arg2 orelse Field == javascriptEnabled)]};
to_json(Obj = {obj, _Any}) -> Obj;
to_json(null) -> null.

