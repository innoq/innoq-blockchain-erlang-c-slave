-module(block_finder).

-export([find_block/5, test/0]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/innoq-blockchain-erlang-c-slave", 0).

% Tries to find a prove value that creates a valid block
% Prefix, Suffix - binary holding the JSON structure without the prove
% From, To - bounds of potential prove space to search
% LeadingZeros - the number of leading zeros required
%
% Returns a Tuple of
% bool which is true if a block has been found,
% binary holding the complete JSON
% binary holding the calculated hash
find_block(Prefix, Suffix, From, To, LeadingZeros) ->
    case find_block_parts(Prefix, Suffix, From, To, LeadingZeros) of
        {false} -> {no_proof_found, "no matching hash found"};
        {true, Proof, Hash} ->
            {proof_found, <<Prefix/binary, Proof/binary, Suffix/binary>>, bin_to_hexstr(Hash)};
        _X -> erlang:error("Unexpected response from find_block_parts")
    end.

find_block_parts(Prefix, Suffix, From, To, LeadingZeros) ->
    {true, <<"123">>, <<"000000">>}.

bin_to_hexstr(Bin) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                                     X <- binary_to_list(Bin)])).

test() ->
    find_block(<<"{\"index\":1,\"timestamp\":0,\"proof\":">>, <<",\"transactions\":[{\"id\":\"b3c973e2-db05-4eb5-9668-3e81c7389a6d\",\"timestamp\":0,\"payload\":\"I am Heribert Innoq\"}],\"previousBlockHash\":\"0\"}">>, 1, 1917338, 6).
