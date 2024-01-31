# The `mjson` module

## Types
### <a name="type_val">val()</a>

Represents a JSON value.
<pre><code>-type <a href="#type_val">val()</a> :: <a href="#type_obj">obj()</a> | <a href="#type_array">array()</a> | <a href="#type_num">num()</a> | <a href="#type_str">str()</a> | <a href="#type_literal">literal()</a>.
</code></pre>

### <a name="type_enc_val">enc_val()</a>

Represents a JSON value for encoding. Just like a `val()`, but also
accepts atoms as strings, and iodata as object member names.
<pre><code>-type <a href="#type_enc_val">enc_val()</a> :: <a href="#type_val">val()</a> | <a href="#type_enc_obj">enc_obj()</a> | <a href="#type_enc_str">enc_str()</a>.
</code></pre>

### <a name="type_obj">obj()</a>

Represents a JSON object.
<pre><code>-type <a href="#type_obj">obj()</a>     :: #{<a href="#type_str">str()</a> => <a href="#type_val">val()</a>}.
</code></pre>

### <a name="type_array">array()</a>

Represents a JSON array.
<pre><code>-type <a href="#type_array">array()</a>   :: [<a href="#type_val">val()</a>].
</code></pre>

### <a name="type_num">num()</a>

Represents a JSON number.
<pre><code>-type <a href="#type_num">num()</a>     :: number().
</code></pre>

### <a name="type_str">str()</a>

Represents a JSON string.
<pre><code>-type <a href="#type_str">str()</a>     :: binary().
</code></pre>

### <a name="type_literal">literal()</a>

Represents a JSON literal.
<pre><code>-type <a href="#type_literal">literal()</a> :: true | false | null.
</code></pre>

### <a name="type_enc_obj">enc_obj()</a>

When encoding, object member names can be atoms and iodata.
<pre><code>-type <a href="#type_enc_obj">enc_obj()</a> :: #{<a href="#type_enc_str">enc_str()</a> | iodata() => <a href="#type_enc_val">enc_val()</a>}.
</code></pre>

### <a name="type_enc_str">enc_str()</a>

When encoding, atoms and binaries will be encoded as JSON strings.
<pre><code>-type <a href="#type_enc_str">enc_str()</a> :: binary() | atom().
</code></pre>

### <a name="type_encode_opts">encode_opts()</a>

Options for `encode/2`.
<pre><code>-type <a href="#type_encode_opts">encode_opts()</a> ::
        #{
           <span style="color:indianred">%% Set to `true` to encode without any whitespace.</span>
           compact => boolean()

           <span style="color:indianred">%% Indent all rows with this whitespace.</span>
         , indent => iodata()

           <span style="color:indianred">%% Print object members sorted by their name.</span>
         , sort_objects => boolean()
         }.
</code></pre>

## Functions
### <a name="func_decode">decode/1</a>

<pre><code>-spec decode(binary()) -> {ok, <a href="#type_val">val()</a>}.
</code></pre>
If `Bin` does not contain valid JSON, `decode/1` crashes.

### <a name="func_encode">encode/1</a>

<pre><code>-spec encode(<a href="#type_enc_val">enc_val()</a>) -> iodata().
</code></pre>
### <a name="func_encode">encode/1</a>

<pre><code>-spec encode(<a href="#type_enc_val">enc_val()</a>, <a href="#type_encode_opts">encode_opts()</a>) -> iodata().
</code></pre>

### <a name="func_encode">encode/2</a>

<pre><code>-spec encode(<a href="#type_enc_val">enc_val()</a>) -> iodata().
</code></pre>
### <a name="func_encode">encode/2</a>

<pre><code>-spec encode(<a href="#type_enc_val">enc_val()</a>, <a href="#type_encode_opts">encode_opts()</a>) -> iodata().
</code></pre>

