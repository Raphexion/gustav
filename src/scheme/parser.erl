-module(parser).

-export([parse/1]).

parse(Program) ->
    reader:read(tokenizer:tokenize(Program)).
