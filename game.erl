-module(game).
-author('sempetmer@gmail.com').

-export([new/3, mark/2,
         marked/1, width/1, height/1, points/1, live/1,
         get_element/2]).

-record(game, {board, tokens, mark, points, live}).

%% Create a new game with N tokens.
new(W, H, N)
  when N > 1 ->
    new(W, H, N, #game{board = board:new(W, H, N), tokens = N, mark = nil,
                       points = 0, live = yes}).
new(W, H, N, Game) ->
    %% Make sure there are moves to do when the game starts.
    case moves_left(Game) of
        true -> Game;
        _    -> new(W, H, N)
    end.

%% Mark a token in the board. If a token is aleady marked, either swap
%% the marked tokens if allowed, or mark the new one.  Returns the new
%% game, or {NewGame, Games} where Games are the groups that were made
%% and removed if a swap was made.
mark(Game = #game{live = no}, _Mark) ->
    Game;
mark(Game = #game{mark = nil}, Mark) ->
    Game#game{mark = Mark};
mark(Game = #game{board = Board, mark = Mark1}, Mark2) ->
    case board:swap(Board, Mark1, Mark2) of
        {ok, SwappedBoard} ->
            Tokens = Game#game.tokens,
            Points = Game#game.points,
            %% Recursively remove any groups that were made, until
            %% there are none.
            {[NewBoard | Boards], NewPoints} = board:no_groups(SwappedBoard,
                                                               Tokens),
            NewGame = Game#game{board = NewBoard,
                                mark = nil,
                                points = Points + NewPoints},
            Moves = [Game#game{board = B, mark = nil} || B <- Boards],
            case moves_left(NewGame) of
                true ->
                    {NewGame, Moves};
                _ ->
                    {NewGame#game{live = no}, Moves}
            end;
        _ ->
            Game#game{mark = Mark2}
    end.

marked(#game{mark = Mark}) ->
    Mark.

width(#game{board = Board}) ->
    board:width(Board).

height(#game{board = Board}) ->
    board:height(Board).

points(#game{points = Points}) ->
    Points.

live(#game{live = Answer}) ->
    Answer.

get_element(#game{board = Board}, Coord) ->
    board:get_element(Board, Coord).

%% Check if there are any legal moves left in Board.
moves_left(#game{board = Board}) ->
    Width  = board:width(Board),
    Height = board:height(Board),
    Coords = [{X, Y} || X <- lists:seq(1, Width),
                        Y <- lists:seq(1, Height)],
    LegalMoves = [{C1, C2} || C1 <- Coords,
                              C2 <- Coords,
                              legal_move(C1, C2, Board)],
    length(LegalMoves) > 0.

%% Is swapping C1 and C2 in Board allowed?
legal_move(C1, C2, Board) ->
    case board:swap(Board, C1, C2) of
        {ok, _} -> true;
        _ -> false
    end.
