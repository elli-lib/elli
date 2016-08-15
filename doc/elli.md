

# Module elli #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Elli acceptor manager.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
This gen_server owns the listen socket and manages the processes
accepting on that socket. When a process waiting for accept gets a
request, it notifies this gen_server so we can start up another
acceptor.

<a name="types"></a>

## Data Types ##




### <a name="type-req">req()</a> ###


<pre><code>
req() = #req{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_acceptors-1">get_acceptors/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_open_reqs-1">get_open_reqs/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_open_reqs-2">get_open_reqs/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_acceptor-2">remove_acceptor/2*</a></td><td></td></tr><tr><td valign="top"><a href="#required_opt-2">required_opt/2*</a></td><td></td></tr><tr><td valign="top"><a href="#set_callback-3">set_callback/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_add_acceptor-1">start_add_acceptor/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#valid_callback-1">valid_callback/1*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="get_acceptors-1"></a>

### get_acceptors/1 ###

`get_acceptors(S) -> any()`

<a name="get_open_reqs-1"></a>

### get_open_reqs/1 ###

`get_open_reqs(S) -> any()`

<a name="get_open_reqs-2"></a>

### get_open_reqs/2 ###

`get_open_reqs(S, Timeout) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="remove_acceptor-2"></a>

### remove_acceptor/2 * ###

`remove_acceptor(State, Pid) -> any()`

<a name="required_opt-2"></a>

### required_opt/2 * ###

`required_opt(Name, Opts) -> any()`

<a name="set_callback-3"></a>

### set_callback/3 ###

`set_callback(S, Callback, CallbackArgs) -> any()`

<a name="start_add_acceptor-1"></a>

### start_add_acceptor/1 * ###

`start_add_acceptor(State) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Opts) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(S) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="valid_callback-1"></a>

### valid_callback/1 * ###

`valid_callback(Mod) -> any()`

