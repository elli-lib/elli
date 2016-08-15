

# Module elli_http #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli HTTP request implementation.

<a name="description"></a>

## Description ##
An elli_http process blocks in elli_tcp:accept/2 until a client
connects. It then handles requests on that connection until it's
closed either by the client timing out or explicitly by the user.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-4">accept/4</a></td><td>Accept on the socket until a client connects.</td></tr><tr><td valign="top"><a href="#accept_timeout-1">accept_timeout/1*</a></td><td></td></tr><tr><td valign="top"><a href="#body_timeout-1">body_timeout/1*</a></td><td></td></tr><tr><td valign="top"><a href="#check_max_size-5">check_max_size/5*</a></td><td>To send a response, we must first receive anything the client is
sending.</td></tr><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td></td></tr><tr><td valign="top"><a href="#close_or_keepalive-2">close_or_keepalive/2*</a></td><td></td></tr><tr><td valign="top"><a href="#connection-2">connection/2*</a></td><td>Add appropriate connection header if the user did not add one already.</td></tr><tr><td valign="top"><a href="#connection_token-1">connection_token/1*</a></td><td></td></tr><tr><td valign="top"><a href="#content_length-2">content_length/2*</a></td><td></td></tr><tr><td valign="top"><a href="#do_get_body-5">do_get_body/5*</a></td><td></td></tr><tr><td valign="top"><a href="#do_send_file-4">do_send_file/4*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_headers-1">encode_headers/1*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_value-1">encode_value/1*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_binary-1">ensure_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#execute_callback-1">execute_callback/1*</a></td><td>Execute the user callback, translating failure into a proper response.</td></tr><tr><td valign="top"><a href="#get_body-5">get_body/5*</a></td><td>Fetch the full body of the request, if any is available.</td></tr><tr><td valign="top"><a href="#get_body_test-0">get_body_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_headers-5">get_headers/5*</a></td><td></td></tr><tr><td valign="top"><a href="#get_headers-6">get_headers/6*</a></td><td></td></tr><tr><td valign="top"><a href="#get_request-4">get_request/4*</a></td><td>Retrieve the request line.</td></tr><tr><td valign="top"><a href="#get_timings-0">get_timings/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_timings-1">get_timings/1*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-4">handle_event/4*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_request-4">handle_request/4</a></td><td>Handle a HTTP request that will possibly come on the socket.</td></tr><tr><td valign="top"><a href="#handle_response-3">handle_response/3*</a></td><td></td></tr><tr><td valign="top"><a href="#header_timeout-1">header_timeout/1*</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1*</a></td><td></td></tr><tr><td valign="top"><a href="#keepalive_loop-3">keepalive_loop/3</a></td><td>Handle multiple requests on the same connection, ie.</td></tr><tr><td valign="top"><a href="#keepalive_loop-5">keepalive_loop/5</a></td><td></td></tr><tr><td valign="top"><a href="#max_body_size-1">max_body_size/1*</a></td><td></td></tr><tr><td valign="top"><a href="#mk_req-7">mk_req/7</a></td><td></td></tr><tr><td valign="top"><a href="#parse_path-1">parse_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#request_timeout-1">request_timeout/1*</a></td><td></td></tr><tr><td valign="top"><a href="#send_bad_request-1">send_bad_request/1*</a></td><td>To send a response, we must first have received everything the
client is sending.</td></tr><tr><td valign="top"><a href="#send_chunk-2">send_chunk/2*</a></td><td></td></tr><tr><td valign="top"><a href="#send_file-5">send_file/5*</a></td><td>Send a HTTP response to the client where the body is the
contents of the given file.</td></tr><tr><td valign="top"><a href="#send_response-4">send_response/4</a></td><td>Generate a HTTP response and send it to the client.</td></tr><tr><td valign="top"><a href="#split_args-1">split_args/1</a></td><td>Split the URL arguments into a proplist.</td></tr><tr><td valign="top"><a href="#split_path-1">split_path/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start_chunk_loop-1">start_chunk_loop/1*</a></td><td>The chunk loop is an intermediary between the socket and the
user.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1*</a></td><td>Response code string.</td></tr><tr><td valign="top"><a href="#t-1">t/1*</a></td><td>Record the current time in the process dictionary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-4"></a>

### accept/4 ###

<pre><code>
accept(Server::pid(), ListenSocket::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>, Options::<a href="proplists.md#type-proplist">proplists:proplist()</a>, Callback::<a href="#type-callback">callback()</a>) -&gt; ok
</code></pre>
<br />

Accept on the socket until a client connects.
Handle the request, then loop if we're using keep alive or chunked transfer.
If [`elli_tcp:accept/3`](elli_tcp.md#accept-3) doesn't return a socket within a configurable
timeout, loop to allow code upgrades of this module.

<a name="accept_timeout-1"></a>

### accept_timeout/1 * ###

`accept_timeout(Opts) -> any()`

<a name="body_timeout-1"></a>

### body_timeout/1 * ###

`body_timeout(Opts) -> any()`

<a name="check_max_size-5"></a>

### check_max_size/5 * ###

`check_max_size(Socket, ContentLength, Buffer, Opts, X5) -> any()`

To send a response, we must first receive anything the client is
sending. To avoid allowing clients to use all our bandwidth, if the request
size is too big, we simply close the socket.

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Socket) -> any()`

<a name="close_or_keepalive-2"></a>

### close_or_keepalive/2 * ###

`close_or_keepalive(Req, UserHeaders) -> any()`

<a name="connection-2"></a>

### connection/2 * ###

`connection(Req, UserHeaders) -> any()`

Add appropriate connection header if the user did not add one already.

<a name="connection_token-1"></a>

### connection_token/1 * ###

`connection_token(Req) -> any()`

<a name="content_length-2"></a>

### content_length/2 * ###

`content_length(Headers, Body) -> any()`

<a name="do_get_body-5"></a>

### do_get_body/5 * ###

`do_get_body(Socket, Buffer, Opts, N, X5) -> any()`

<a name="do_send_file-4"></a>

### do_send_file/4 * ###

`do_send_file(Fd, X2, Req, Headers) -> any()`

<a name="encode_headers-1"></a>

### encode_headers/1 * ###

`encode_headers(H) -> any()`

<a name="encode_value-1"></a>

### encode_value/1 * ###

`encode_value(V) -> any()`

<a name="ensure_binary-1"></a>

### ensure_binary/1 * ###

`ensure_binary(Bin) -> any()`

<a name="execute_callback-1"></a>

### execute_callback/1 * ###

`execute_callback(Req) -> any()`

Execute the user callback, translating failure into a proper response.

<a name="get_body-5"></a>

### get_body/5 * ###

<pre><code>
get_body(Socket, Headers, Buffer, Opts, Callback) -&gt; FullBody
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>Headers = <a href="#type-headers">headers()</a></code></li><li><code>Buffer = binary()</code></li><li><code>Opts = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="#type-callback">callback()</a></code></li><li><code>FullBody = {<a href="#type-body">body()</a>, binary()}</code></li></ul>

Fetch the full body of the request, if any is available.

At the moment we don't need to handle large requests, so there is
no need for streaming or lazily fetching the body in the user
code. Fully receiving the body allows us to avoid the complex
request object threading in Cowboy and the caching in Mochiweb.

As we are always receiving whatever the client sends, we might have
buffered too much and get parts of the next pipelined request. In
that case, push it back in the buffer and handle the first request.

<a name="get_body_test-0"></a>

### get_body_test/0 * ###

`get_body_test() -> any()`

<a name="get_headers-5"></a>

### get_headers/5 * ###

<pre><code>
get_headers(Socket, V, Buffer, Opts, Callback) -&gt; {<a href="#type-headers">headers()</a>, any()}
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>V = <a href="#type-version">version()</a></code></li><li><code>Buffer = binary()</code></li><li><code>Opts = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="#type-callback">callback()</a></code></li></ul>

<a name="get_headers-6"></a>

### get_headers/6 * ###

`get_headers(Socket, Buffer, Headers, HeadersCount, Opts, Callback) -> any()`

<a name="get_request-4"></a>

### get_request/4 * ###

`get_request(Socket, Buffer, Options, Callback) -> any()`

Retrieve the request line.

<a name="get_timings-0"></a>

### get_timings/0 * ###

`get_timings() -> any()`

<a name="get_timings-1"></a>

### get_timings/1 * ###

`get_timings(X1) -> any()`

<a name="handle_event-4"></a>

### handle_event/4 * ###

`handle_event(Mod, Name, EventArgs, ElliArgs) -> any()`

<a name="handle_request-4"></a>

### handle_request/4 ###

<pre><code>
handle_request(Socket, PrevBin, Options, Callback) -&gt; ConnToken
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>PrevBin = binary()</code></li><li><code>Options = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="#type-callback">callback()</a></code></li><li><code>ConnToken = {keep_alive | close, binary()}</code></li></ul>

Handle a HTTP request that will possibly come on the socket.
Returns the appropriate connection token and any buffer containing (parts of)
the next request.

<a name="handle_response-3"></a>

### handle_response/3 * ###

`handle_response(Req, Buffer, X3) -> any()`

<a name="header_timeout-1"></a>

### header_timeout/1 * ###

`header_timeout(Opts) -> any()`

<a name="init-1"></a>

### init/1 * ###

`init(Req) -> any()`

<a name="keepalive_loop-3"></a>

### keepalive_loop/3 ###

`keepalive_loop(Socket, Options, Callback) -> any()`

Handle multiple requests on the same connection, ie. `"keep alive"`.

<a name="keepalive_loop-5"></a>

### keepalive_loop/5 ###

`keepalive_loop(Socket, NumRequests, Buffer, Options, Callback) -> any()`

<a name="max_body_size-1"></a>

### max_body_size/1 * ###

`max_body_size(Opts) -> any()`

<a name="mk_req-7"></a>

### mk_req/7 ###

<pre><code>
mk_req(Method, PathTuple, Headers, Body, V, Socket, Callback) -&gt; Req
</code></pre>

<ul class="definitions"><li><code>Method = <a href="#type-http_method">http_method()</a></code></li><li><code>PathTuple = {PathType::atom(), RawPath::binary()}</code></li><li><code>Headers = <a href="#type-headers">headers()</a></code></li><li><code>Body = <a href="#type-body">body()</a></code></li><li><code>V = <a href="#type-version">version()</a></code></li><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a> | undefined</code></li><li><code>Callback = <a href="#type-callback">callback()</a></code></li><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li></ul>

<a name="parse_path-1"></a>

### parse_path/1 ###

`parse_path(X1) -> any()`

<a name="request_timeout-1"></a>

### request_timeout/1 * ###

`request_timeout(Opts) -> any()`

<a name="send_bad_request-1"></a>

### send_bad_request/1 * ###

`send_bad_request(Socket) -> any()`

To send a response, we must first have received everything the
client is sending. If this is not the case, [`send_bad_request/1`](#send_bad_request-1)
might reset the client connection.

<a name="send_chunk-2"></a>

### send_chunk/2 * ###

`send_chunk(Socket, Data) -> any()`

<a name="send_file-5"></a>

### send_file/5 * ###

<pre><code>
send_file(Req, Code, Headers, Filename, Range) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>Code = <a href="#type-response_code">response_code()</a></code></li><li><code>Headers = <a href="#type-headers">headers()</a></code></li><li><code>Filename = <a href="file.md#type-filename">file:filename()</a></code></li><li><code>Range = <a href="#type-range">range()</a></code></li></ul>

Send a HTTP response to the client where the body is the
contents of the given file. Assumes correctly set response code
and headers.

<a name="send_response-4"></a>

### send_response/4 ###

`send_response(Req, Code, Headers, UserBody) -> any()`

Generate a HTTP response and send it to the client.

<a name="split_args-1"></a>

### split_args/1 ###

<pre><code>
split_args(Qs::binary()) -&gt; [{binary(), binary() | true}]
</code></pre>
<br />

Split the URL arguments into a proplist.
Lifted from `cowboy_http:x_www_form_urlencoded/2`.

<a name="split_path-1"></a>

### split_path/1 * ###

`split_path(Path) -> any()`

<a name="start_chunk_loop-1"></a>

### start_chunk_loop/1 * ###

`start_chunk_loop(Socket) -> any()`

The chunk loop is an intermediary between the socket and the
user. We forward anything the user sends until the user sends an
empty response, which signals that the connection should be
closed. When the client closes the socket, the loop exits.

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Server, ListenSocket, Options, Callback) -&gt; pid()
</code></pre>

<ul class="definitions"><li><code>Server = pid()</code></li><li><code>ListenSocket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>Options = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="#type-callback">callback()</a></code></li></ul>

<a name="status-1"></a>

### status/1 * ###

`status(B) -> any()`

Response code string. Lifted from `cowboy_http_req.erl`.

<a name="t-1"></a>

### t/1 * ###

`t(Key) -> any()`

Record the current time in the process dictionary.
This allows easily adding time tracing wherever,
without passing along any variables.

