

# Module elli_util #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#encode_range-2">encode_range/2</a></td><td> Encode Range to a Content-Range value.</td></tr><tr><td valign="top"><a href="#file_size-1">file_size/1</a></td><td> Get the size in bytes of the file.</td></tr><tr><td valign="top"><a href="#normalize_range-2">normalize_range/2</a></td><td> If a valid byte-range, or byte-range-set of size 1
is supplied, returns a normalized range in the format
{Offset, Length}.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="encode_range-2"></a>

### encode_range/2 ###

<pre><code>
encode_range(Range::<a href="#type-range">range()</a> | invalid_range, Size::non_neg_integer()) -&gt; ByteRange::iolist()
</code></pre>
<br />

Encode Range to a Content-Range value.

<a name="file_size-1"></a>

### file_size/1 ###

<pre><code>
file_size(Filename::<a href="file.md#type-name">file:name()</a>) -&gt; non_neg_integer() | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = badarg | <a href="file.md#type-posix">file:posix()</a></code></li></ul>

Get the size in bytes of the file.

<a name="normalize_range-2"></a>

### normalize_range/2 ###

<pre><code>
normalize_range(RangeOrSet::any(), Size::integer()) -&gt; <a href="#type-range">range()</a> | undefined | invalid_range
</code></pre>
<br />

If a valid byte-range, or byte-range-set of size 1
is supplied, returns a normalized range in the format
{Offset, Length}. Returns undefined when an empty byte-range-set
is supplied and the atom `invalid_range` in all other cases.

