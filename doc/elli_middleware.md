

# Module elli_middleware #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

HTTP request processing middleware.

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="description"></a>

## Description ##

This module offers both pre-processing of requests and post-processing of
responses. It can also be used to allow multiple handlers, where the first
handler to return a response short-circuits the request.
It is implemented as a plain elli handler.

Usage:
Config = [
{mods, [
{elli_example_middleware, []},
{elli_middleware_compress, []},
{elli_example_callback, []}
]}
],
elli:start_link([
...,
{callback, elli_middleware},
{callback_args, Config}
]).

The configured modules may implement the elli behaviour, in which case all
the callbacks will be used as normal. If [`handle/2`](#handle-2) returns `ignore`,
elli will continue on to the next callback in the list.

Pre-processing and post-processing is implemented in [`preprocess/2`](#preprocess-2)
and [`postprocess/3`](#postprocess-3). [`preprocess/2`](#preprocess-2) is called for each
middleware in the order specified, while [`postprocess/3`](#postprocess-3) is called in
the reverse order.

FIXME: Even if a middleware short-circuits the request,
all postprocess middlewares will be called.

`elli_middleware` does not add any significant overhead.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do_init-2">do_init/2*</a></td><td></td></tr><tr><td valign="top"><a href="#forward_event-3">forward_event/3*</a></td><td></td></tr><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#mods-1">mods/1*</a></td><td></td></tr><tr><td valign="top"><a href="#postprocess-3">postprocess/3*</a></td><td></td></tr><tr><td valign="top"><a href="#preprocess-2">preprocess/2*</a></td><td></td></tr><tr><td valign="top"><a href="#process-2">process/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="do_init-2"></a>

### do_init/2 * ###

`do_init(Req, Mods) -> any()`

<a name="forward_event-3"></a>

### forward_event/3 * ###

`forward_event(Event, Args, Mods) -> any()`

<a name="handle-2"></a>

### handle/2 ###

`handle(CleanReq, Config) -> any()`

<a name="handle_event-3"></a>

### handle_event/3 ###

`handle_event(Event, Args, Config) -> any()`

<a name="init-2"></a>

### init/2 ###

`init(Req, Args) -> any()`

<a name="mods-1"></a>

### mods/1 * ###

`mods(Config) -> any()`

<a name="postprocess-3"></a>

### postprocess/3 * ###

`postprocess(Req, Res, Mods) -> any()`

<a name="preprocess-2"></a>

### preprocess/2 * ###

`preprocess(Req, Mods) -> any()`

<a name="process-2"></a>

### process/2 * ###

`process(Req, Mods) -> any()`

