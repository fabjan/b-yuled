-module(gui).
-author('sempetmer@gmail.com').

-record(display, {frame, width, height, buttons, points}).
-record(state, {game, display, window, dimensions}).

-export([start/0, init/0]).

start() ->
    spawn(?MODULE, init, []).

%% Create a new game, setup a window and begin listening to input.
init() ->
    {A, B, C} = now(), random:seed(A, B, C),
    Server  = gs:start(),
    W       = 200,
    H       = 200,
    WH      = [{width, W}, {height, H}],
    Window  = gs:window(Server, [{configure,true} | WH]),
    Game    = game:new(8, 8, 6),
    Display = display(Game, Window),
    config(Display,WH),
    gs:config(Window, {map,true}),
    loop(#state{game = Game, window = Window, display = Display}).

%% Listen for input from buttons or resizing of the window.
loop(State) ->
    receive
        %% We got a button click, mark the coordinates on the board
        %% and show the new state.
        {gs, _Button, click, {X, Y}, _Args} ->
            case game:mark(State#state.game, {X, Y}) of
                {NewGame, Moves} ->
                    animate(Moves, State#state.display),
                    update(State#state.display, NewGame),
                    loop(State#state{game = NewGame});
                NewGame ->
                    update(State#state.display, NewGame),
                    loop(State#state{game = NewGame})
            end;
        %% Resizing the window, make sure the display is updated.
        {gs,_Id,configure,_Data,[W,H|_]} ->
            WH = [{width,W},{height,H}],
            config(State#state.display,WH),
            loop(State);
        %% Create a new game when the new_game button is clicked.
        {gs, _Button, click, new_game, _Args} ->
            NewGame = game:new(8, 8, 6),
            update(State#state.display, NewGame),
            loop(State#state{game = NewGame});
        %% Exit when the quit button is clicked.
        {gs, _Button, click, quit, _Args} ->
            gs:destroy(State#state.window),
            exit(ok)
    end.

%% Create a display for Game in Window.
display(Game, Window) ->
    Width   = game:width(Game),
    Height  = game:height(Game),
    Columns = lists:duplicate(Width, {stretch, 1, 16}),
    Rows    = lists:duplicate(Height + 1, {stretch, 1, 16}),
    Frame   = gs:frame(Window, [{packer_x, Columns}, {packer_y, Rows}]),
    Coords  = [{X, Y} || X <- lists:seq(1, Width),
                         Y <- lists:seq(1, Height)],
    Buttons = [button(Coord, Frame, Game) || Coord <- Coords],
    Points  = gs:label(Frame, [{label, {text, "Score: 0"}},
                               {pack_xy, {{3, Width}, Height + 1}}]),
    gs:button(Frame, [{label, {text, "New game"}}, {data, new_game},
                      {pack_xy, {1, Height + 1}}]),
    gs:button(Frame, [{label, {text, "Quit"}}, {data, quit},
                      {pack_xy, {2, Height + 1}}]),
    #display{frame = Frame, width = Width, height = Height,
             buttons = Buttons, points = Points}.

%% Resize Display to WH.
config(#display{frame = Frame}, WH) ->
    gs:config(Frame, WH).

%% Create a button in Frame for the element at Coord in Game.
button(Coord, Frame, Game) ->
    Element = game:get_element(Game, Coord),
    gs:button(Frame, [{data, Coord}, {pack_xy, Coord},
                      {label, image(Element)}, {bg, color(Element)}]).

%% Update the buttons and score label to correspond with Game.
update(#display{buttons = Buttons, points = Points}, Game) ->
    lists:foreach(fun (Button) -> update_button(Button, Game) end, Buttons),
    Score = integer_to_list(game:points(Game)),
    case game:live(Game) of
        yes ->
            gs:config(Points, [{label, {text, "Score: " ++ Score}}]);
        _ ->
            gs:config(Points, [{label, {text, "GAME OVER, Score: " ++ Score}},
                               {bg, red}])
    end.

%% Update the color and image of Button to the correct element in Game.
update_button(Button, Game) ->
    Coord   = gs:read(Button, data),
    Element = game:get_element(Game, Coord),
    case {Element, game:marked(Game)} of
        {_, Coord} ->
            gs:config(Button, [{label, image(Element)}, {bg, orange}]);
        {x, _} ->
            gs:config(Button, [{label, {text, ""}}, {bg, orange}]);
        _ ->
            gs:config(Button, [{label, image(Element)}, {bg, color(Element)}])
    end.

animate(Moves, Display) ->
    lists:foreach(fun (Move) -> update(Display, Move), timer:sleep(100) end,
                  Moves).

color(N) ->
    lists:nth(N, [red, {0,128,0}, {192, 100, 0}, yellow, {64, 128, 255}, white]).

image(N) ->
    {image, integer_to_list(N) ++ ".xbm"}.
