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
