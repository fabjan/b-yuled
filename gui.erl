-module(gui).
-author('sempetmer@gmail.com').

-compile(export_all).
-record(display, {frame, width, height, buttons, points}).
-record(state, {game, display, window, dimensions}).

start() ->
    spawn(?MODULE, init, []).

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

loop(State) ->
    receive
        {gs, Button, click, Data, _Args} ->
            case game:mark(State#state.game, Data) of
                {NewGame, Moves} ->
                    update(State#state.display, NewGame),
                    loop(State#state{game = NewGame});
                NewGame ->
                    update(State#state.display, NewGame),
                    loop(State#state{game = NewGame})
            end;
        {gs,_Id,configure,_Data,[W,H|_]} ->
            WH = [{width,W},{height,H}],
            config(State#state.display,WH),
            loop(State)
    end.

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

config(#display{frame = Frame}, WH) ->
    gs:config(Frame, WH).

button(Coord, Frame, Game) ->
    Element = game:get_element(Game, Coord),
    gs:button(Frame, [{data, Coord}, {pack_xy, Coord},
                      {label, image(Element)}, {bg, color(Element)}]).

update(#display{buttons = Buttons, points = Points}, Game) ->
    lists:foreach(fun (Button) -> update_button(Button, Game) end, Buttons),
    gs:config(Points,
              [{label, {text, "Score: " ++ integer_to_list(game:points(Game))}}]).

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

mark(Button) ->
    gs:config(Button, [{bg, white}]).

unmark(Button, N) ->
    gs:config(Button, [{bg, color(N)}]).
