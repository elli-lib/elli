

# Module elli_middleware_compress #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Response compression as Elli middleware.

<a name="description"></a>

## Description ##
Postprocesses all requests and compresses bodies larger than
`compress_byte_size` (`1024` by default).<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accepted_encoding-1">accepted_encoding/1*</a></td><td></td></tr><tr><td valign="top"><a href="#compress-2">compress/2*</a></td><td></td></tr><tr><td valign="top"><a href="#postprocess-3">postprocess/3</a></td><td></td></tr><tr><td valign="top"><a href="#should_compress-2">should_compress/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accepted_encoding-1"></a>

### accepted_encoding/1 * ###

`accepted_encoding(Req) -> any()`

<a name="compress-2"></a>

### compress/2 * ###

`compress(Body, Req) -> any()`

<a name="postprocess-3"></a>

### postprocess/3 ###

`postprocess(Req, Res, Config) -> any()`

<a name="should_compress-2"></a>

### should_compress/2 * ###

`should_compress(Body, S) -> any()`

