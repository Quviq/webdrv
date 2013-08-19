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
%%% Includes for WebDriver
%%% @end
%%%-------------------------------------------------------------------

-record(capability,
        { browserName
        %% , browserVersion
        , version = <<"">>
        %% , platformName
        , platform = <<"ANY">>
        %% , platformVersion
        , javascriptEnabled = true
        , device = <<"">>
        }).

-record(webdrv_opts, { url, timeout = 5000, session_id = null }).

-type session_id() :: string() | null.
-type url() :: string().

-type frame_id() :: jsonstr() | number() | null.

-type json()     :: jsonobj() | jsonlist() | jsonnum() | jsonstr() | true | false | null.
-type jsonobj()  :: {obj, [{jsonkey(), json()}]}.
-type jsonkey()  :: string().
-type jsonlist() :: [json()].
-type jsonnum()  :: integer() | float().
-type jsonstr()  :: binary().

-type request_error() :: {html_error, string()}
                       | {json_error, string()}
                       | {cmd_error, json()}
                       | {wire_error, {atom(), string()}, session_id()}
                       | {type_error, atom(), term()}.

-type request_ok() :: {ok, session_id(), jsonobj()}.

-type request_res() :: request_ok() | request_error().

-type capability() :: #capability{} | null | jsonobj().

-type orientation() :: landscape | portrait.
-type cookie() :: jsonobj().

-type button() :: integer() | left | middle | right.

-type log_entry() :: {number(), string(), string()}.
-type log() :: [log_entry()].

-type cache_status() ::
        uncached | idle | checking | downloading | update_ready | obsolete.

