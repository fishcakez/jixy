Jixy
====

Wrapper for jiffy and jsx to allow seamless switching between both libraries.

Erlang/JSON conversions follow the jiffy rules. However it is possible to turn
json object keys into atoms (as with jsx).

Caveats/Notes
=============

- WARNING: Jiffy and jsx deal with some characters differently!!!!
- Jiffy and jsx encode floats with differing levels of precision.
- Streaming is always done by jsx, regardless of options.
- Jiffy and jsx pretty print differently.

Usage
=====

The `nif_size_encode/decode` option represents the maximum byte or external byte
size to use jiffy for. So `infinity` will always use jiffy; `0`, jsx.

```erlang
-spec jixy:encode(term()) -> {ok, iolist() | binary()} | {error, badarg}.
-spec jixy:encode(term(),
    [{nif_size_encode, non_neg_integer() | infinity} | {pretty, boolean()}]) ->
    {ok, iolist() | binary()} | {error, badarg}.

-spec jixy:decode(iolist() | binary()) -> {ok, term()} | {error, badarg}.
-spec jixy:decode(iolist() | binary(),
        [{nif_size_decode, non_neg_integer() | infinity} |
         {labels, binary | atom | existing_atom}]) ->
    {ok, term()} | {error, badarg}.

-spec jixy:decode_init([{labels, binary | atom | existing_atom}]) -> state().
-spec jixy:decode_stream(state(), iolist() | binary()) ->
    {ok, state()} | {error, badarg}.
-spec jixy:decpde_end(state()) -> {ok, term()} | {error, badarg}
```

License
=======

MIT License (see LICENSE)
