-module(jixy_SUITE).

%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

%% Test cases
-export([encode_decode/1,
         encode_decode_labels_atom/1]).

-export([encode_equal/1,
         decode_equal/1,
         nif_size_encode/1,
         nif_size_decode/1]).

-export([decode_stream/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("triq/include/triq.hrl").

-define(RECURSIVE(Domain),
        ?DELAY(?SIZED(Size, resize(random:uniform((Size div 3) + 1), Domain)))).

-define(check(Prop), begin
        case check(Prop) of
            true ->
                ok;
            false ->
                Example = [{K, V} ||
                        {K, _, V, _ } <- get('triq:counterexample')],
                {failed, {badproperty, Example}};
            {'EXIT', Reason} ->
                {'EXIT', {Reason, erlang:get_stacktrace()}};
            Other ->
                Other
        end
    end).

all() ->
    [{group, jiffy}, {group, jsx}, {group, combined}, {group, stream}].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [{jiffy, [encode_decode,
              encode_decode_labels_atom]},
     {jsx, [encode_decode,
            encode_decode_labels_atom]},
     {combined, [encode_equal,
                 decode_equal,
                 nif_size_encode,
                 nif_size_decode]},
     {stream, [decode_stream]}].

init_per_suite(Config) ->
    ok = application:start(jiffy),
    ok = application:start(jsx),
    ok = application:start(jixy),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(jixy),
    ok = application:stop(jiffy),
    ok = application:stop(jsx),
    ok.

init_per_group(jiffy, Config) ->
    [{userdata,
      [{nif_size_decode, infinity}, {nif_size_encode, infinity}]} | Config];
init_per_group(jsx, Config) ->
    [{userdata,
      [{nif_size_decode, 0}, {nif_size_encode, 0}]} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

%% testcases

encode_decode(Config) ->
    Opts = get_options(Config),
    ?check(prop_encode_decode(Opts)).

prop_encode_decode(Opts) ->
    ?FORALL({Term, Pretty}, {json_term(), oneof([[{pretty, true}],
                                                 [{pretty, false}],
                                                 []])},
            begin
                {ok, Json} = jixy:encode(Term, Pretty ++ Opts),
                Term2 = atoms_to_binary(Term),
                {ok, Term2} == jixy:decode(Json, Opts)
            end).

encode_decode_labels_atom(Config) ->
    Opts = get_options(Config),
    ?check(prop_encode_decode_labels_atom(Opts)).

prop_encode_decode_labels_atom(Opts) ->
    ?FORALL(Term, json_term_safe(),
            begin
                {ok, Json} = jixy:encode(Term, Opts),
                Term2 = atoms_to_binary(Term),
                Term3 = atom_keys(Term2),
                {ok, Term4} = jixy:decode(Json, [{labels, atom} | Opts]),
                {ok, Term5} = jixy:decode(Json, [{labels, existing_atom} |
                                                  Opts]),
                Term3 == Term4 andalso Term4 == Term5
            end).

encode_equal(_) ->
    ?check(prop_encode_equal()).

prop_encode_equal() ->
    ?FORALL(Term, json_term_safe(),
            begin
                Jiffy = jixy:encode(Term, [{nif_size_encode, infinity}]),
                Jsx = jixy:encode(Term, [{nif_size_encode, 0}]),
                Jiffy == Jsx
            end).

decode_equal(_) ->
    ?check(prop_decode_equal()).

prop_decode_equal() ->
    ?FORALL({Json, Opts}, {json_text(),
                           oneof([[], [{labels, binary}], [{labels, atom}],
                                  [{labels, existing_atom}]])},
            begin
                Jiffy = jixy:decode(Json, [{nif_size_decode, infinity} | Opts]),
                Jsx = jixy:decode(Json, [{nif_size_decode, 0} | Opts]),
                Jiffy == Jsx
            end).

nif_size_encode(_) ->
    ?check(prop_nif_size_encode()).

prop_nif_size_encode() ->
    ?FORALL({Term, NifSizeEncode}, {json_term(), oneof([0, infinity,
                                                        pos_integer()])},
            begin
                Mod = case erlang:external_size(Term, [{minor_version, 1}]) of
                        Size when Size =< NifSizeEncode ->
                            jiffy;
                        _ ->
                            jsx
                    end,
                ok = start_trace(encode),
                Self = self(),
                Pid = spawn_link(fun() ->
                            jixy:encode(Term,
                                        [{nif_size_encode, NifSizeEncode}]),
                            Self ! done
                    end),
                receive
                    done ->
                        ok
                end,
                ok = check_trace(Pid, Mod),
                ok = stop_trace(encode),
                ok = flush_trace(),
                true
            end).

nif_size_decode(_) ->
    ?check(prop_nif_size_decode()).

prop_nif_size_decode() ->
    ?FORALL({Json, NifSizeDecode}, {json_text(), oneof([0, infinity,
                                                        pos_integer()])},
            begin
                Mod = case erlang:byte_size(Json) of
                        Size when Size =< NifSizeDecode ->
                            jiffy;
                        _ ->
                            jsx
                    end,
                ok = start_trace(decode),
                Self = self(),
                Pid = spawn_link(fun() ->
                            jixy:decode(Json,
                                        [{nif_size_decode, NifSizeDecode}]),
                            Self ! done
                    end),
                receive
                    done ->
                        ok
                end,
                ok = check_trace(Pid, Mod),
                ok = stop_trace(decode),
                ok = flush_trace(),
                true
            end).

decode_stream(_) ->
    ?check(prop_decode_stream()).

prop_decode_stream() ->
    ?FORALL(List, json_text_stream(),
            begin
                Decoder = jixy:decode_init([]),
                Fun = fun(Elem, Acc) ->
                        {ok, Acc2} = jixy:decode_stream(Acc, Elem),
                        Acc2
                    end,
                DecoderFinal = lists:foldl(Fun, Decoder, List),
                {ok, Term} = jixy:decode_end(DecoderFinal),
                {ok, Term} == jixy:decode(List)
            end).

%% generators

json_term() ->
    oneof([?RECURSIVE({list({?DELAY(oneof([utf8_binary(), atom()])),
                             ?DELAY(json_term())})}),
           ?RECURSIVE(list(json_term())), bool(), null,
           int(), real(), utf8_binary(), atom()]).

json_term_safe() ->
    oneof([?RECURSIVE({list({?DELAY(ascii_binary()),
                             ?DELAY(json_term_safe())})}),
           ?RECURSIVE(list(json_term_safe())), bool(), null,
           int(), ascii_binary()]).

utf8_binary() ->
    ?LET(Binary,
         ?SUCHTHAT(Binary, ?RECURSIVE(binary()),
                   begin
                    try jsx:encode(Binary) of
                        _Json ->
                            true
                    catch
                        error:badarg ->
                            false
                    end
                end),
         begin
            return(Binary)
        end).

ascii_binary() ->
    ?LET(List, list(?DELAY(choose(32,126))),
         begin
            return(erlang:list_to_binary(List))
        end).

json_text() ->
    ?LET(Term,
         ?SUCHTHAT(Term, json_term(),
                  begin
                    try jiffy:encode(Term) of
                        _Json ->
                            true
                    catch
                        throw:{error, _} ->
                            false
                    end
                end),
                begin
                    return(jiffy:encode(Term))
                end).

json_text_stream() ->
    ?LET(Binary, json_text(),
         begin
            Split = byte_size(Binary) div 2,
            {Binary2, Binary3} = erlang:split_binary(Binary, Split),
            return([Binary2, Binary3])
        end).

%% internal

get_options(Config) ->
    case lists:keyfind(userdata, 1, Config) of
        {userdata, Opts} ->
            Opts;
        false ->
            []
    end.

atom_keys({PropList}) ->
    {[{erlang:binary_to_atom(Key, utf8), atom_keys(Value)} ||
            {Key, Value} <- PropList]};
atom_keys(List) when is_list(List) ->
    [atom_keys(Element) || Element <- List];
atom_keys(Term) ->
    Term.

atoms_to_binary(true) ->
    true;
atoms_to_binary(false) ->
    false;
atoms_to_binary(null) ->
    null;
atoms_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
atoms_to_binary({PropList}) ->
    {[{atoms_to_binary(Key), atoms_to_binary(Value)} ||
            {Key, Value} <- PropList]};
atoms_to_binary(List) when is_list(List) ->
    [atoms_to_binary(Element) || Element <- List];
atoms_to_binary(Term) ->
    Term.

start_trace(Function) ->
    erlang:trace(all, true, [call]),
    erlang:trace_pattern({jiffy, Function, '_'}, true, [global]),
    erlang:trace_pattern({jsx, Function, '_'}, true, [global]),
    ok.

check_trace(Pid, Mod) ->
    Ref = erlang:trace_delivered(Pid),
    receive
        {trace_delivered, _, Ref} ->
            ok
    after
        1000 ->
            error(timeout)
    end,
    receive
        {trace, _, call, {Mod, _, _}} ->
            ok;
        {trace, _, call, {BadMod, _, _}} ->
            error({wrong_call, BadMod, Mod})
    after
        0 ->
            error({no_call, Mod})
    end.

flush_trace() ->
    receive
        _Msg ->
            flush_trace()
    after
        0 ->
            ok
    end.

stop_trace(Function) ->
    erlang:trace_pattern({jiffy, Function, '_'}, false, [global]),
    erlang:trace_pattern({jsx, Function, '_'}, false, [global]),
    erlang:trace(all, false, [call]),
    ok.
