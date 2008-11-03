-module(board).
-author('sempetmer@gmail.com').

-compile(export_all).

-define(TOKENS, [r, g, b]).

%% Generate a W by H board of elements in 1..N.
new(W, H, N) ->
    no_groups(cols(W, H, N), N).
cols(0, _H, _N) -> [];
cols(W, H, N) ->
    [col(H, N) | cols(W - 1, H, N)].
col(0, _N) -> [];
col(W, N) ->
    [random:uniform(N) | col(W - 1, N)].

%% Clear groups in a board and refill from 1..N recursively until
%% there are no groups.
no_groups(Board, N) ->
    Marked = mark(Board),
    case points(Marked) of
        0 -> Board;
        _ -> no_groups(refill(Marked, N), N)
    end.

%% Transpose a board.
transpose(Columns) -> % add accumulator and token
    transpose([token | Columns], []).
transpose([token,[] | _], Rows) -> % base case, we're done
    lists:reverse(lists:map(fun lists:reverse/1, Rows));
transpose([token, [Head | Tail] | Columns], Rows) -> % create a new column
    transpose(Columns ++ [token | [Tail]], [[Head] | Rows]);
transpose([[Head | Tail] | Columns], [Row | Rows]) -> % keep on truckin'
    transpose(Columns ++ [Tail], [[Head | Row] | Rows]).

%% Returns an element at a given coordinate in a board.
get_element(Board, {X, Y}) ->
    lists:nth(Y, lists:nth(X, Board)).

%% Apply a function on the Nth element in a list, and replace the
%% element with the result.
apply_nth([Head | Tail], 1, Fun) ->
    [Fun(Head) | Tail];
apply_nth([Head | Tail], N, Fun) when N > 1 ->
    [Head | apply_nth(Tail, N - 1, Fun)].

%% Sets the element at a given coordinate in a board to a new element.
set_element(Board, {X, Y}, Element) ->
    ReplaceInCol = fun (C) -> apply_nth(C, Y, fun (_) -> Element end) end,
    apply_nth(Board, X, ReplaceInCol).

%% Swaps two elements in a board.
swap(Board, {X1, Y1}, {X2, Y2}) ->
    A = get_element(Board, {X1, Y1}),
    B = get_element(Board, {X2, Y2}),
    set_element(set_element(Board, {X1, Y1}, B), {X2, Y2}, A).

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

%% Clear all x from a marked board and refill with new elements from 1..N.
refill(Board, N) ->
    Height = length(hd(Board)),
    Cleared = lists:map(fun (C) -> [ E || E <- C, E /= x] end,
                        Board),
    lists:map(fun (L) -> col(Height - length(L), N) ++ L end, Cleared).
