-module(prop_split_combine).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_split_combine_1() ->
    ?FORALL(Data, binary(),
         begin
	     Data =:= gus_bin:combine(gus_bin:split(Data, 1))
	 end).

prop_split_combine_2() ->
    ?FORALL(Data, binary(),
	    begin
		Data =:= gus_bin:combine(gus_bin:split(Data, 2))
	    end).

prop_split_combine() ->
    ?FORALL({Data, Size}, {binary(), size()},
	    begin
		Data =:= gus_bin:combine(gus_bin:split(Data, Size))
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

size() ->
    ?SUCHTHAT(Size, integer(), Size > 0).
