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
    {true, <<'Hello world from Erlang'>>, <<'abcd'>>}.

test() ->
    find_block("foo", "bar", 1, 2, 3).
