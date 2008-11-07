-module(gui).
-author('sempetmer@gmail.com').

-compile(export_all).
-record(state, {game, window, frame, dimensions, selected}).

start() ->
    spawn(?MODULE, init, []).

init() ->
    {A, B, C} = now(), random:seed(A, B, C),
    Server = gs:start(),
    W      = 200,
    H      = 200,
    WH     = [{width, W}, {height, H}],
    Window = gs:window(Server, [{configure,true} | WH]),
    Game   = game:new(8, 8, 4),
    Board  = game:board(Game),
    Frame  = frame(Board, Window),
    buttons(Board, Frame),
    gs:config(Window, {map,true}),
    gs:config(Frame,WH),
    loop(#state{game = Game, window = Window, frame = Frame, dimensions = {W, H}}).

loop(State = #state{selected = undefined}) ->
    receive
        {gs, Button, click, Data, _Args} ->
            io:format("Data: ~p~nState: ~p~n~n", [Data, State]),
            case game:mark(State#state.game, Data) of
                {new, NewGame} ->
                    mark(Button),
                    loop(State#state{game = NewGame,
                                     selected = Button});
                {swap, NewGame, _Boards} ->
                    Board    = game:board(NewGame),
                    NewFrame = frame(Board, State#state.window),
                    buttons(Board, NewFrame),
                    {W, H} = State#state.dimensions,
                    gs:config(NewFrame,[{width,W},{height,H}]),
                    gs:destroy(State#state.frame),
                    loop(State#state{game = NewGame,
                                     frame = NewFrame,
                                     selected = undefined})
            end;
        {gs,_Id,configure,_Data,[W,H|_]} ->
            gs:config(State#state.frame,[{width,W},{height,H}]),
            loop(State#state{dimensions = {W, H}})
    end;
loop(State = #state{selected = SButton}) ->
    receive
        {gs, Button, click, Data, _Args} ->
            io:format("Data: ~p~nState: ~p~n~n", [Data, State]),
            case game:mark(State#state.game, Data) of
                {new, NewGame} ->
                    unmark(SButton, board:get_element(game:board(NewGame), Data)),
                    mark(Button),
                    loop(State#state{game = NewGame,
                                     selected = Button});
                {swap, NewGame, _Boards} ->
                    Board    = game:board(NewGame),
                    NewFrame = frame(Board, State#state.window),
                    buttons(Board, NewFrame),
                    {W, H} = State#state.dimensions,
                    gs:config(NewFrame,[{width,W},{height,H}]),
                    gs:destroy(State#state.frame),
                    loop(State#state{game = NewGame,
                                     frame = NewFrame,
                                     selected = undefined})
            end;
        {gs,_Id,configure,_Data,[W,H|_]} ->
            gs:config(State#state.frame,[{width,W},{height,H}]),
            loop(State#state{dimensions = {W, H}})
    end.

frame(Board, Parent) ->
    Columns = lists:duplicate(length(Board), {stretch, 1, 16}),
    Rows = lists:duplicate(length(hd(Board)), {stretch, 1, 16}),
    gs:frame(Parent, [{packer_x, Columns}, {packer_y, Rows}]).

buttons(Board, Frame) ->
    Width  = length(Board),
    Height = length(hd(Board)),
    Coords = [{X,Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)],
    lists:foreach(fun (Coord) ->
                          Element = board:get_element(Board, Coord),
                          gs:button(Frame, [{data, Coord}, {pack_xy, Coord},
                                            {label, image(Element)},
                                            {bg, color(Element)}]) end, Coords).

color(N) ->
    lists:nth(N, [red, {0,128,0}, {192, 100, 0}, yellow]).

image(N) ->
    {image, integer_to_list(N) ++ ".xbm"}.

mark(Button) ->
    gs:config(Button, [{bg, white}]).

unmark(Button, N) ->
    gs:config(Button, [{bg, color(N)}]).
