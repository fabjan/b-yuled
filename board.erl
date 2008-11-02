-module(board).
-author('sempetmer@gmail.com').

-compile(export_all).

%% Generate a random board W by H
new(W, H) ->
    cols(W, H, []).
cols(_W, 0, Columns) -> Columns;
cols(W, H, Columns) ->
    cols(W, H - 1, [col(W) | Columns]).
col(W) ->
    col(W, []).
col(0, Column) -> Column;
col(W, Column) ->
    Tokens = [r, g, b],
    col(W - 1, [lists:nth(random:uniform(length(Tokens)), Tokens) | Column]).

%% Transpose a board.
transpose(Columns) -> % add accumulator and token
    transpose([token | Columns], []).
transpose([token,[] | _], Rows) -> % base case, we're done
    lists:reverse(lists:map(fun lists:reverse/1, Rows));
transpose([token, [Head | Tail] | Columns], Rows) -> % create a new column
    transpose(Columns ++ [token | [Tail]], [[Head] | Rows]);
transpose([[Head | Tail] | Columns], [Row | Rows]) -> % keep on truckin'
    transpose(Columns ++ [Tail], [[Head | Row] | Rows]).

%% Replace all groups of three or more in the board with x.
mark(Board) ->
    lists:zipwith(fun mask_line/2,
                  transpose(mark_rows(Board)), mark_cols(Board)).

%% Replace all elements in column groups of three or more with x.
mark_cols(Board) ->
    lists:map(fun mark_line/1, Board).

%% Replace all elements in row groups of three or more with x.
mark_rows(Board) ->
    lists:map(fun mark_line/1, transpose(Board)).

%% Replace all elements in groups of three or more with x in a line.
mark_line([]) -> [];
mark_line([E, E, E | Tail]) ->
    {Group, Rest} = lists:splitwith(fun (X) -> X == E end, Tail),
    [x, x, x | lists:map(fun (_) -> x end, Group)] ++ mark_line(Rest);
mark_line([E | Tail]) ->
    [E] ++ mark_line(Tail).

%% Mask a line with x.
mask_line([], []) -> [];
mask_line([x | Mask], [_ | Line]) ->
    [x | mask_line(Mask, Line)];
mask_line([_ | Mask], [E | Line]) ->
    [E | mask_line(Mask, Line)].

%% Compute the score for a marked board.
points(Board) ->
    length([ E || E <- lists:flatten(Board), E == x]).

%% Clear all x from a marked board and refill with new elements.
refill(Board) ->
    Height = length(hd(Board)),
    Cleared = lists:map(fun (C) -> [ E || E <- C, E /= x] end,
                        Board),
    lists:map(fun (L) -> col(Height - length(L)) ++ L end, Cleared).
