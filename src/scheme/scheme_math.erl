-module(scheme_math).

-export([add/1,
	 sub/1]).

add(XS) ->
    lists:sum(XS).

sub([X]) ->
    -X;
sub([X|XS]) ->
    X - lists:sum(XS).
