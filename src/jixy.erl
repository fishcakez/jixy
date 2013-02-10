-module(jixy).

-export([encode/1,
         encode/2,
         decode/1,
         decode/2]).

-export([decode_init/1,
         decode_stream/2,
         decode_end/1]).

-export([prettify/1,
         minify/1]).

-define(JSX_PRETTY, [{space, 1}, {indent, 2}]).

-type encode_option() ::
    pretty | {pretty, boolean()} |
    {nif_size_encode, 0 | infinity | pos_integer()}.
-type encode_options() :: [encode_option()].

-type labels() :: binary | atom | existing_atom.
-type decode_option() ::
    labels() | {labels, labels()} |
    {nif_size_decode, 0 | infinity | pos_integer()}.
-type decode_options() :: [decode_option()].

-type json_text() :: iolist() | binary().
-type json_term() ::
    {[{binary() | atom(), json_term()}]} | [json_term()] | true | false | null |
    integer() | float() | binary() | atom().

-opaque decode_state() :: jsx:decoder().

-export_type([encode_options/0,
              decode_options/0,
              json_text/0,
              json_term/0,
              decode_state/0]).

-spec encode(json_term()) -> {ok, json_text()} | {error, badarg}.
encode(Term) ->
    encode(Term, []).

-spec encode(json_term(), encode_options()) ->
    {ok, json_text()} | {error, badarg}.
encode(Term, Opts) when is_list(Opts) ->
    Module = select_module(encode, Term, Opts),
    Opts2 = parse_encode_options(Module, Opts),
    try Module:encode(Term, Opts2) of
        IoList ->
            {ok, IoList}
    catch
        throw:{error, _} ->
            {error, badarg};
        error:badarg ->
            {error, badarg}
    end.

-spec decode(json_text()) -> {ok, json_term()} | {error, badarg}.
decode(IoList) ->
    decode(IoList, []).

-spec decode(json_text(), decode_options()) ->
    {ok, json_term()} | {error, badarg}.
decode(IoList, Opts) when is_list(IoList) ->
    Binary = erlang:iolist_to_binary(IoList),
    decode(Binary, Opts);
decode(Binary, Opts) when is_list(Opts) ->
    Module = select_module(decode, Binary, Opts),
    {Opts2, PostFun} = parse_decode_options(Module, Opts),
    try do_decode(Module, Binary, Opts2, PostFun) of
        {incomplete, Fun} when is_function(Fun) ->
            {error, badarg};
        Term ->
            {ok, Term}
    catch
        throw:{error, _} ->
            {error, badarg};
        error:badarg ->
            {error, badarg}
    end.

-spec decode_init(decode_options()) -> decode_state().
decode_init(Opts) when is_list(Opts) ->
    {Opts2, _} = parse_decode_options(jsx, Opts),
    Opts3 = [explicit_end | Opts2],
    {incomplete, Fun} = jsx:decode(<<>>, Opts3),
    Fun.

-spec decode_stream(decode_state(), json_text()) ->
    {ok, decode_state()} | {error, badarg}.
decode_stream(Fun, IoList) when is_list(IoList) ->
    Binary = erlang:iolist_to_binary(IoList),
    decode_stream(Fun, Binary);
decode_stream(Fun, Binary) when is_function(Fun) andalso is_binary(Binary) ->
    try Fun(Binary) of
        {incomplete, Fun2} ->
            {ok, Fun2}
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec decode_end(decode_state()) -> {ok, json_term()} | {error, badarg}.
decode_end(Fun) when is_function(Fun) ->
    try Fun(end_stream) of
        Term ->
            {ok, Term}
    catch
        error:badarg ->
            {error, badarg}
    end.

-spec prettify(json_text()) -> json_text().
prettify(IoList) when is_list(IoList) ->
    Binary = erlang:iolist_to_binary(IoList),
    prettify(Binary);
prettify(Binary) when is_binary(Binary) ->
    jsx:prettify(Binary).

-spec minify(json_text()) -> json_text().
minify(IoList) when is_list(IoList) ->
    Binary = erlang:iolist_to_binary(IoList),
    minify(Binary);
minify(Binary) when is_binary(Binary) ->
    jsx:minify(Binary).

%% internal

select_module(Function, Term, Opts) ->
    case lists:keyfind(nif_size_key(Function), 1, Opts) of
        {_, 0} ->
            jsx;
        {_, Int} when is_integer(Int) andalso Int > 0 ->
            select_module_by_size(Function, Term, Int);
        {_, infinity} ->
            jiffy;
        _ ->
            jsx
    end.

nif_size_key(encode) ->
    nif_size_encode;
nif_size_key(decode) ->
    nif_size_decode.

select_module_by_size(encode, Term, JiffyMax) ->
    case erlang:external_size(Term, [{minor_version, 1}]) of
        Size when Size =< JiffyMax ->
            jiffy;
        _ ->
            jsx
    end;
select_module_by_size(decode, Binary, JiffyMax) ->
    case erlang:byte_size(Binary) of
        Size when Size =< JiffyMax ->
            jiffy;
        _ ->
            jsx
    end.

parse_encode_options(Module, Opts) ->
    case lists:keyfind(pretty, 1, Opts) of
        {_, true} when Module == jiffy ->
            [pretty];
        _ when Module == jiffy ->
            [];
        {_, true} when Module == jsx ->
            [{pre_encode, fun jsx_pre_encode/1} | ?JSX_PRETTY];
        _ when Module == jsx ->
            [{pre_encode, fun jsx_pre_encode/1}]
    end.

parse_decode_options(Module, Opts) ->
    case lists:keyfind(labels, 1, Opts) of
        {_, atom} when Module == jiffy ->
            {[], fun labels_atom/1};
        {_, existing_atom} when Module == jiffy ->
            {[], fun labels_existing_atom/1};
        _ when Module == jiffy ->
            {[], undefined};
        {_, Labels} when Module == jsx andalso (Labels == atom orelse
                                                Labels == existing_atom) ->
            {[{post_decode, fun jsx_post_decode/1}, {labels, Labels}],
             undefined};
        _ when Module == jsx ->
            {[{post_decode, fun jsx_post_decode/1}], undefined}
    end.

do_decode(jsx, Binary, Opts, undefined) ->
    jsx:decode(Binary, Opts);
do_decode(jiffy, Binary, _Opts, undefined) ->
    jiffy:decode(Binary);
do_decode(jiffy, Binary, _Opts, PostFun) ->
    Term = jiffy:decode(Binary),
    PostFun(Term).

labels_atom({PropList}) ->
    {[{binary_to_atom(Key, utf8), labels_atom(Value)} ||
            {Key, Value} <- PropList]};
labels_atom(List) when is_list(List) ->
    [labels_atom(Element) || Element <- List];
labels_atom(Term) ->
    Term.

labels_existing_atom({PropList}) ->
    {[{binary_to_existing_atom(Key, utf8), labels_existing_atom(Value)} ||
            {Key, Value} <- PropList]};
labels_existing_atom(List) when is_list(List) ->
    [labels_existing_atom(Element) || Element <- List];
labels_existing_atom(Term) ->
    Term.

jsx_pre_encode(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
jsx_pre_encode(Term) ->
    Term.

jsx_post_decode([{}]) ->
    {[]};
jsx_post_decode([Tuple | _Rest] = PropList) when is_tuple(Tuple) ->
    {PropList};
jsx_post_decode(Term) ->
    Term.
