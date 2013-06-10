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
%%% Small http server - should be able to serve static pages just for the purpouse of
%%% testing WebDriver.
%%% @end
%%%-------------------------------------------------------------------

-module(webdrv_http_srv).

-export([start/0, stop/0, stop/1]).

conf() ->
  [{port, 8088},
   {server_root, "../"},
   {document_root, "../data"},
   {server_name, "localhost"}].

start() ->
  inets:start(),
  {ok, Pid} = inets:start(httpd, conf()),
  Pid.

stop(ServerPid) ->
  ok = inets:stop(httpd, ServerPid).

stop() ->
  ok = inets:stop(httpd, {{127,0,0,1}, 8088}).
