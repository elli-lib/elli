

# Module elli_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-http_range">http_range()</a> ###


<pre><code>
http_range() = {First::non_neg_integer(), Last::non_neg_integer()} | {offset, Offset::non_neg_integer()} | {suffix, Length::pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send_chunk-2">async_send_chunk/2</a></td><td>Send a chunk asynchronously.</td></tr><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the <code>body</code>.</td></tr><tr><td valign="top"><a href="#body_qs-1">body_qs/1</a></td><td>Parse <code>application/x-www-form-urlencoded</code> body into a proplist.</td></tr><tr><td valign="top"><a href="#callback-1">callback/1</a></td><td>Return the <code>callback</code>.</td></tr><tr><td valign="top"><a href="#chunk_ref-1">chunk_ref/1</a></td><td>Return a reference that can be used to send chunks to the client.</td></tr><tr><td valign="top"><a href="#close_chunk-1">close_chunk/1</a></td><td>Explicitly close the chunked connection.</td></tr><tr><td valign="top"><a href="#get_arg-2">get_arg/2</a></td><td>Equivalent to <a href="#get_arg-3"><tt>get_arg(Key, Req, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_arg-3">get_arg/3</a></td><td>Equivalent to <a href="proplists.md#get_value-3"><tt>proplists:get_value(Key, Args, Default)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_arg_decoded-2">get_arg_decoded/2</a></td><td>Equivalent to <a href="#get_arg_decoded-3"><tt>get_arg_decoded(Key, Req, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_arg_decoded-3">get_arg_decoded/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_args-1">get_args/1</a></td><td>Return a proplist of keys and values of the original query string.</td></tr><tr><td valign="top"><a href="#get_args_decoded-1">get_args_decoded/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_header-2">get_header/2</a></td><td>Equivalent to <a href="proplists.md#get_value-2"><tt>proplists:get_value(Key, Headers)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_header-3">get_header/3</a></td><td>Equivalent to <a href="proplists.md#get_value-3"><tt>proplists:get_value(Key, Headers, Default)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_range-1">get_range/1</a></td><td>Parse the <code>Range</code> header from the request.</td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td>Return the <code>headers</code>.</td></tr><tr><td valign="top"><a href="#host-1">host/1</a></td><td>Return the <code>host</code>.</td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td>Return the <code>method</code>.</td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td>Return <code>path</code> split into binary parts.</td></tr><tr><td valign="top"><a href="#peer-1">peer/1</a></td><td></td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Return the <code>port</code>.</td></tr><tr><td valign="top"><a href="#post_arg-2">post_arg/2</a></td><td>Equivalent to <a href="#post_arg-3"><tt>post_arg(Key, Req, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#post_arg-3">post_arg/3</a></td><td></td></tr><tr><td valign="top"><a href="#post_arg_decoded-2">post_arg_decoded/2</a></td><td>Equivalent to <a href="#post_arg_decoded-3"><tt>post_arg_decoded(Key, Req, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#post_arg_decoded-3">post_arg_decoded/3</a></td><td></td></tr><tr><td valign="top"><a href="#post_args-1">post_args/1</a></td><td></td></tr><tr><td valign="top"><a href="#post_args_decoded-1">post_args_decoded/1</a></td><td></td></tr><tr><td valign="top"><a href="#query_str-1">query_str/1</a></td><td>Calculate the query string associated with a given <code>Request</code>
as a binary.</td></tr><tr><td valign="top"><a href="#raw_path-1">raw_path/1</a></td><td>Return the <code>raw_path</code>, i.e.</td></tr><tr><td valign="top"><a href="#scheme-1">scheme/1</a></td><td>Return the <code>scheme</code>.</td></tr><tr><td valign="top"><a href="#send_chunk-2">send_chunk/2</a></td><td>Send a chunk synchronously.</td></tr><tr><td valign="top"><a href="#set_body-2">set_body/2</a></td><td>Set the <code>body</code> of <code>Req</code> to <code>Body</code>.</td></tr><tr><td valign="top"><a href="#socket-1">socket/1</a></td><td>Return the <code>socket</code>.</td></tr><tr><td valign="top"><a href="#to_proplist-1">to_proplist/1</a></td><td>Serialize the <code>Req</code>uest record to a proplist.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Return the <code>version</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send_chunk-2"></a>

### async_send_chunk/2 ###

`async_send_chunk(Ref, Data) -> any()`

Send a chunk asynchronously.

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="elli.md#type-body">elli:body()</a>
</code></pre>
<br />

Return the `body`.

<a name="body_qs-1"></a>

### body_qs/1 ###

`body_qs(Req) -> any()`

Parse `application/x-www-form-urlencoded` body into a proplist.

<a name="callback-1"></a>

### callback/1 ###

<pre><code>
callback(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="elli_handler.md#type-callback">elli_handler:callback()</a>
</code></pre>
<br />

Return the `callback`.

<a name="chunk_ref-1"></a>

### chunk_ref/1 ###

`chunk_ref(Req) -> any()`

Return a reference that can be used to send chunks to the client.
If the protocol does not support it, return `{error, not_supported}`.

<a name="close_chunk-1"></a>

### close_chunk/1 ###

`close_chunk(Ref) -> any()`

Equivalent to [`send_chunk(Ref, close)`](#send_chunk-2).

Explicitly close the chunked connection.
Return `{error, closed}` if the client already closed the connection.

<a name="get_arg-2"></a>

### get_arg/2 ###

`get_arg(Key, Req) -> any()`

Equivalent to [`get_arg(Key, Req, undefined)`](#get_arg-3).

<a name="get_arg-3"></a>

### get_arg/3 ###

`get_arg(Key, Req, Default) -> any()`

Equivalent to [`proplists:get_value(Key, Args, Default)`](proplists.md#get_value-3).

<a name="get_arg_decoded-2"></a>

### get_arg_decoded/2 ###

`get_arg_decoded(Key, Req) -> any()`

Equivalent to [`get_arg_decoded(Key, Req, undefined)`](#get_arg_decoded-3).

<a name="get_arg_decoded-3"></a>

### get_arg_decoded/3 ###

`get_arg_decoded(Key, Req, Default) -> any()`

<a name="get_args-1"></a>

### get_args/1 ###

<pre><code>
get_args(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; QueryArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>
<br />

Return a proplist of keys and values of the original query string.
Both keys and values in the returned proplists will be binaries or the atom
`true` in case no value was supplied for the query value.

<a name="get_args_decoded-1"></a>

### get_args_decoded/1 ###

`get_args_decoded(Req) -> any()`

<a name="get_header-2"></a>

### get_header/2 ###

`get_header(Key, Req) -> any()`

Equivalent to [`proplists:get_value(Key, Headers)`](proplists.md#get_value-2).

<a name="get_header-3"></a>

### get_header/3 ###

`get_header(Key, Req, Default) -> any()`

Equivalent to [`proplists:get_value(Key, Headers, Default)`](proplists.md#get_value-3).

<a name="get_range-1"></a>

### get_range/1 ###

<pre><code>
get_range(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; [<a href="#type-http_range">http_range()</a>] | parse_error
</code></pre>
<br />

Parse the `Range` header from the request.
The result is either a `byte_range_set()` or the atom `parse_error`.
Use [`elli_util:normalize_range/2`](elli_util.md#normalize_range-2) to get a validated, normalized range.

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="elli.md#type-headers">elli:headers()</a>
</code></pre>
<br />

Return the `headers`.

<a name="host-1"></a>

### host/1 ###

<pre><code>
host(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; undefined | binary()
</code></pre>
<br />

Return the `host`.

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="elli.md#type-http_method">elli:http_method()</a>
</code></pre>
<br />

Return the `method`.

<a name="path-1"></a>

### path/1 ###

<pre><code>
path(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; [binary()]
</code></pre>
<br />

Return `path` split into binary parts.

<a name="peer-1"></a>

### peer/1 ###

<pre><code>
peer(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; undefined | binary()
</code></pre>
<br />

<a name="port-1"></a>

### port/1 ###

<pre><code>
port(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; undefined | 1..65535
</code></pre>
<br />

Return the `port`.

<a name="post_arg-2"></a>

### post_arg/2 ###

`post_arg(Key, Req) -> any()`

Equivalent to [`post_arg(Key, Req, undefined)`](#post_arg-3).

<a name="post_arg-3"></a>

### post_arg/3 ###

`post_arg(Key, Req, Default) -> any()`

<a name="post_arg_decoded-2"></a>

### post_arg_decoded/2 ###

`post_arg_decoded(Key, Req) -> any()`

Equivalent to [`post_arg_decoded(Key, Req, undefined)`](#post_arg_decoded-3).

<a name="post_arg_decoded-3"></a>

### post_arg_decoded/3 ###

`post_arg_decoded(Key, Req, Default) -> any()`

<a name="post_args-1"></a>

### post_args/1 ###

`post_args(Req) -> any()`

<a name="post_args_decoded-1"></a>

### post_args_decoded/1 ###

`post_args_decoded(Req) -> any()`

<a name="query_str-1"></a>

### query_str/1 ###

<pre><code>
query_str(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; QueryStr::binary()
</code></pre>
<br />

Calculate the query string associated with a given `Request`
as a binary.

<a name="raw_path-1"></a>

### raw_path/1 ###

<pre><code>
raw_path(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; binary()
</code></pre>
<br />

Return the `raw_path`, i.e. not split or parsed for query params.

<a name="scheme-1"></a>

### scheme/1 ###

<pre><code>
scheme(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; undefined | binary()
</code></pre>
<br />

Return the `scheme`.

<a name="send_chunk-2"></a>

### send_chunk/2 ###

`send_chunk(Ref, Data) -> any()`

Send a chunk synchronously.
If the referenced process is dead, return early with `{error, closed}`,
instead of timing out.

<a name="set_body-2"></a>

### set_body/2 ###

<pre><code>
set_body(Req::<a href="elli.md#type-req">elli:req()</a>, Body::<a href="elli.md#type-body">elli:body()</a>) -&gt; <a href="elli.md#type-req">elli:req()</a>
</code></pre>
<br />

Set the `body` of `Req` to `Body`.

<a name="socket-1"></a>

### socket/1 ###

<pre><code>
socket(X1::<a href="elli.md#type-req">elli:req()</a>) -&gt; undefined | <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>
</code></pre>
<br />

Return the `socket`.

<a name="to_proplist-1"></a>

### to_proplist/1 ###

<pre><code>
to_proplist(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>
<br />

Serialize the `Req`uest record to a proplist.
Useful for logging.

<a name="version-1"></a>

### version/1 ###

<pre><code>
version(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; <a href="elli_http.md#type-version">elli_http:version()</a>
</code></pre>
<br />

Return the `version`.

