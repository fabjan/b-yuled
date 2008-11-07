-module(game).
-author('sempetmer@gmail.com').

-compile(export_all).

-record(game, {board, tokens, mark, points, state}).

new(W, H, N) ->
    new(W, H, N, #game{board = board:new(W, H, N), tokens = N, mark = nil,
                       points = 0, state = alive}).
new(W, H, N, Game) ->
    case moves_left(Game) of
        true -> Game;
        _    -> new(W, H, N)
    end.

mark(Game = #game{state = dead}, _Mark) ->
    {dead, Game};
mark(Game = #game{mark = nil}, Mark) ->
    {new, Game#game{mark = Mark}};
mark(Game = #game{board = Board, mark = Mark1}, Mark2) ->
    case board:swap(Board, Mark1, Mark2) of
        {ok, SwappedBoard} ->
            Tokens = Game#game.tokens,
            Points = Game#game.points,
            {Boards, NewPoints} = board:no_groups(SwappedBoard, Tokens),
            NewGame = Game#game{board = hd(Boards),
                                mark = nil,
                                points = Points + NewPoints},
            case moves_left(NewGame) of
                true ->
                    {swap, NewGame, Boards};
                _ ->
                    {dead, NewGame, Boards}
            end;
        _ ->
            {new, Game#game{mark = Mark2}}
    end.

board(#game{board = Board}) ->
    Board.

moves_left(Game) ->
    Board  = board(Game),
    Width  = length(Board),
    Height = length(hd(Board)),
    Coords = [{X, Y} || X <- lists:seq(1, Width),
                        Y <- lists:seq(1, Height)],
    LegalMoves = [{C1, C2} || C1 <- Coords,
                              C2 <- Coords,
                              legal_move(C1, C2, Board)],
    length(LegalMoves) > 0.

legal_move(C1, C2, Board) ->
    case board:swap(Board, C1, C2) of
        {ok, _} -> true;
        _ -> false
    end.
