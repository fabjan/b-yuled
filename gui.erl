-module(gui).
-author('sempetmer@gmail.com').

-record(display, {frame, width, height, buttons, points}).
-record(state, {game, display, window, dimensions}).

-export([start/0]).

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
    Game    = game:new(8, 8, 4),
    Display = display(Game, Window),
    config(Display,WH),
    gs:config(Window, {map,true}),
    loop(#state{game = Game, window = Window, display = Display}).

%% Listen for input from buttons or resizing of the window.
loop(State) ->
    receive
        %% We got a button click, mark the coordinates on the board
        %% and show the new state.
        {gs, _Button, click, Data, _Args} ->
            case game:mark(State#state.game, Data) of
                {NewGame, Moves} ->
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
            loop(State)
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
                               {pack_xy, {{1, Width}, Height}}]),
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
            gs:config(Points, [{label, {text, "GAME OVER, Score: " ++ Score}}])
    end.

%% Update the color and image of Button to the correct element in Game.
update_button(Button, Game) ->
    Coord   = gs:read(Button, data),
    Element = game:get_element(Game, Coord),
    case game:mark(Game) of
        Coord ->
            gs:config(Button, [{label, image(Element)}, {bg, white}]);
        _ ->
            gs:config(Button, [{label, image(Element)}, {bg, color(Element)}])
    end.

color(N) ->
    lists:nth(N, [red, {0,128,0}, {192, 100, 0}, yellow]).

image(N) ->
    {image, integer_to_list(N) ++ ".xbm"}.
