

# Module elli_example_callback #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli example callback.

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="description"></a>

## Description ##
Your callback needs to implement two functions, [`handle/2`](#handle-2) and
[`handle_event/3`](#handle_event-3). For every request, Elli will call your handle
function with the request. When an event happens, like Elli
completed a request, there was a parsing error or your handler
threw an error, [`handle_event/3`](#handle_event-3) is called.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td>Send 10 separate chunks to the client.</td></tr><tr><td valign="top"><a href="#chunk_loop-2">chunk_loop/2*</a></td><td>If <code>N > 0</code>, send a chunk to the client, checking for errors,
as the user might have disconnected.</td></tr><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>Handle a <code>Req</code>uest.</td></tr><tr><td valign="top"><a href="#handle-3">handle/3*</a></td><td>Route <code>Method</code> and <code>Path</code> to the appropriate clause.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td>Handle Elli events, fired throughout processing a request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Ref) -> any()`

Equivalent to [`chunk_loop(Ref, 10)`](#chunk_loop-2).

Send 10 separate chunks to the client.

<a name="chunk_loop-2"></a>

### chunk_loop/2 * ###

`chunk_loop(Ref, N) -> any()`

If `N > 0`, send a chunk to the client, checking for errors,
as the user might have disconnected.
When `N == 0`, call [  elli_request:close_chunk(Ref)](elli_request.md#close_chunk-1).

<a name="handle-2"></a>

### handle/2 ###

<pre><code>
handle(Req, _Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>_Args = <a href="#type-callback_args">callback_args()</a></code></li><li><code>Result = <a href="elli_handler.md#type-result">elli_handler:result()</a></code></li></ul>

Handle a `Req`uest.
Delegate to our handler function.

__See also:__ [handle/3](#handle-3).

<a name="handle-3"></a>

### handle/3 * ###

<pre><code>
handle(Method, Path, Req) -&gt; <a href="elli_handler.md#type-result">elli_handler:result()</a>
</code></pre>

<ul class="definitions"><li><code>Method = <a href="elli.md#type-http_method">elli:http_method()</a></code></li><li><code>Path = [binary()]</code></li><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li></ul>

Route `Method` and `Path` to the appropriate clause.

`ok` can be used instead of `200` to signal success.

If you return any of the following HTTP headers, you can
override the default behaviour of Elli:

* **Connection**:     By default Elli will use `keep-alive` if the protocol
supports it, setting `<<"close">>` will close the
connection immediately after Elli has sent the
response. If the client has already sent pipelined
requests, these will be discarded.

* **Content-Length**: By default Elli looks at the size of the body you
returned to determine the `Content-Length` header.
Explicitly including your own `Content-Length` (with
the value as `integer()`, `binary()` or `list()`)
allows you to return an empty body. Useful for
implementing the `"304 Not Modified"` response.

__See also:__ [chunk_loop/1](#chunk_loop-1), [elli_request:chunk_ref/1](elli_request.md#chunk_ref-1), [elli_request:encode_range/2](elli_request.md#encode_range-2), [elli_request:get_arg/3](elli_request.md#get_arg-3), [elli_request:get_arg_decoded/3](elli_request.md#get_arg_decoded-3), [elli_request:get_args_decoded/1](elli_request.md#get_args_decoded-1), [elli_request:get_header/3](elli_request.md#get_header-3), [elli_request:get_range/1](elli_request.md#get_range-1), [elli_request:normalize_range/2](elli_request.md#normalize_range-2), [elli_request:post_arg/3](elli_request.md#post_arg-3), [elli_request:post_arg_decoded/3](elli_request.md#post_arg_decoded-3), [elli_util:file_size/1](elli_util.md#file_size-1).

<a name="handle_event-3"></a>

### handle_event/3 ###

<pre><code>
handle_event(Event, Args, Config) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Event = <a href="#type-elli_event">elli_event()</a></code></li><li><code>Args = [term()]</code></li><li><code>Config = [tuple()]</code></li></ul>

Handle Elli events, fired throughout processing a request.

`elli_startup` is sent when Elli is starting up. If you are
implementing a middleware, you can use it to spawn processes,
create ETS tables or start supervised processes in a supervisor
tree.

`request_complete` fires *after* Elli has sent the response to the
client. Timings contains timestamps of events like when the
connection was accepted, when request parsing finished, when the
user callback returns, etc. This allows you to collect performance
statistics for monitoring your app.

`request_throw`, `request_error` and `request_exit` events are sent if
the user callback code throws an exception, has an error or
exits. After triggering this event, a generated response is sent to
the user.

`invalid_return` is sent if the user callback code returns a term not
understood by elli, see [`elli_http:execute_callback/1`](elli_http.md#execute_callback-1).
After triggering this event, a generated response is sent to the user.

`chunk_complete` fires when a chunked response is completely
sent. It's identical to the `request_complete` event, except instead
of the response body you get the atom `client` or `server`
depending on who closed the connection.

`request_closed` is sent if the client closes the connection when
Elli is waiting for the next request on a keep alive connection.

`request_timeout` is sent if the client times out when
Elli is waiting for the request.

`request_parse_error` fires if the request is invalid and cannot be parsed by
[`erlang:decode_packet/3`][decode_packet/3] or it contains a path Elli cannot
parse or does not support.

[decode_packet/3]: http://erlang.org/doc/man/erlang.html#decode_packet-3

`client_closed` can be sent from multiple parts of the request
handling. It's sent when the client closes the connection or if for
any reason the socket is closed unexpectedly. The `Where` atom
tells you in which part of the request processing the closed socket
was detected: `receiving_headers`, `receiving_body` or `before_response`.

`client_timeout` can as with `client_closed` be sent from multiple
parts of the request handling. If Elli tries to receive data from
the client socket and does not receive anything within a timeout,
this event fires and the socket is closed.

`bad_request` is sent when Elli detects a request is not well
formatted or does not conform to the configured limits. Currently
the `Reason` variable can be `{too_many_headers, Headers}`
or `{body_size, ContentLength}`.

`file_error` is sent when the user wants to return a file as a
response, but for some reason it cannot be opened.

