%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(heatmap).
-behaviour(gen_server).

-export([start/0]).
-export([stop/1]).
-export([insert/2]).
-export([move/3]).
-export([heat/2]).
-export([heatmap/1]).
-export([render/1]).
-export([update/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {cells :: dict(),
                range = 4 :: integer(),
                falloff = 2 :: integer()}).

-record(cell, {amt :: integer(),
               dissipation_cycles = 0 :: integer()}).

%-define(RANGE, 4).
%-define(FALLOFF, 2).

%% API

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, {}, _Options = []),
    Pid.

stop(Pid) ->
    gen_server:cast(Pid, stop).

insert(Pid, To) ->
    gen_server:cast(Pid, {To}).

move(Pid, From, To) ->
    gen_server:cast(Pid, {From, To}).

heat(Pid, XY) ->
    gen_server:call(Pid, {heat, XY}).

render(Pid) ->
    gen_server:call(Pid, render).

heatmap(Pid) ->
    gen_server:call(Pid, map).

update(Pid, KV) ->
    gen_server:cast(Pid, {update, KV}).

%% Internal

update_state(falloff, Value, State) ->
    io:format("Heatmap ~p updating falloff to ~p~n", [self(), Value]),
    State#state{falloff = list_to_integer(binary_to_list(Value))};
update_state(range, Value, State) ->
    io:format("Heatmap ~p updating range to ~p~n", [self(), Value]),
    State#state{range = list_to_integer(binary_to_list(Value))};
update_state(_Key, _, State) ->
    io:format("Heatmap ~p ignoring change to ~p~n", [self(), _Key]),
    State.

update(UpdateFunFun, {X, Y}, State = #state{falloff = Falloff, range = Range}) ->
    Amt = Range * Falloff,
    MaxMinXYs = [{X - N, Y - N, X + N, Y + N} || N <- lists:seq(0, Range - 1)],
    {_, _, State2}  = lists:foldl(fun update_square/2,
                                    {UpdateFunFun, Amt, State},
                                    MaxMinXYs),
    HotCells = dict:filter(fun(_, Cell) -> Cell#cell.amt > 0 end, State2#state.cells),
    State2#state{cells = HotCells}.

update_square(MaxMinXY, Acc = {UpdateFunFun, Amt, #state{falloff = Falloff}}) ->
    SquarePoints = square(MaxMinXY),
    {_, _, NewState} = lists:foldl(fun update_cell/2, Acc, SquarePoints),
    {UpdateFunFun, Amt - Falloff, NewState}.

square({X1, Y1, X2, Y2}) ->
    Xs = lists:seq(X1, X2),
    Ys = lists:seq(Y1, Y2),
    [{X, Y} || X <- Xs, Y <- Ys, X == X1 orelse X == X2 orelse Y == Y1 orelse Y == Y2].

update_cell(Point, {UpdateFunFun, Amt, State}) ->
    NewCells = dict:update(Point, UpdateFunFun(Amt, State), #cell{amt = Amt}, State#state.cells),
    {UpdateFunFun, Amt, State#state{cells = NewCells}}.

add_fun(Add, _State) ->
    fun(Cell = #cell{amt = Amt}) ->
        Cell#cell{amt = Amt + Add}
    end.

rem_fun(Remove, #state{falloff = Falloff}) ->
    fun(Cell = #cell{amt = Amt}) ->
        NewAmt = Amt - round((Remove / 2)),
        NewDissipationCycles = round(NewAmt / Falloff) + 1,
        Cell#cell{amt = NewAmt,
                  dissipation_cycles = NewDissipationCycles}
    end.

render_cells(#state{cells = Cells, falloff = Falloff}) ->
    Fun = fun(Key, Cell, Acc) ->
              render_cell(Key, Cell, Falloff, Acc)
          end,
    RenderedCells = dict:fold(Fun, [], Cells),
    [{canvas, <<"heatmap">>}, {objs, RenderedCells}].

render_cell({X, Y}, #cell{amt = Amt}, Falloff, Objects) ->
    Red = max(0, 255 - Amt),
    Cell = shape:shape(rectangle,
                       {X * 10, Y * 10},
                       10,
                       {Red, 0, 0, 1.0},
                       gradient,
                       {Red + Falloff, 0, 0, 1.0}),
    [Cell | Objects].

heat_({X, Y} = Point, Cells) ->
    InnerSquare = square({X - 1, Y - 1, X + 1, Y + 1}),
    OuterSquare = square({X - 2, Y - 2, X + 2, Y + 2}),
    InnerCells = square_cells(InnerSquare, Cells),
    OuterCells = square_cells(OuterSquare, Cells),
    Multiples1 = multiples(Point, InnerCells),
    Multiples2 = multiples(Point, OuterCells),
    Multiples1 ++ Multiples2.

square_cells(Square, Cells) ->
    [{Point, Cell} || Point <- Square, {ok, Cell} <- [dict:find(Point, Cells)]].

multiples(CenterPoint, Cells) ->
    [{multiple(CenterPoint, Point), Cell#cell.amt} || {Point, Cell} <- Cells].

multiple({X1, Y1}, {X2, Y2}) ->
    {multiple(X1, X2), multiple(Y1, Y2)};
multiple(A, A) ->
    0;
multiple(A, B) when A < B ->
    1;
multiple(_, _) ->
    -1.

dissipate(State = #state{cells = Cells, falloff = Falloff}) ->
    Fun = fun(_Key, Cell) ->
              dissipate_cell(Cell, Falloff)
          end,
    NewCells = dict:filter(fun has_amount/2, dict:map(Fun, Cells)),
    State#state{cells = NewCells}.

dissipate_cell(Cell = #cell{amt = Amt,
                            dissipation_cycles = DissipationCycles},
               Falloff)
    when is_integer(DissipationCycles), DissipationCycles > 0 ->

    Dissipation = Falloff,
    NewAmt = case Amt - Dissipation of
                 X when X < 0 ->
                     0;
                 X ->
                     X
             end,
    Cell#cell{amt = NewAmt,
              dissipation_cycles = max(0, DissipationCycles - 1)};
dissipate_cell(Cell, _Falloff) ->
    Cell.

has_amount(_Key, #cell{amt = Amt}) ->
    Amt > 0.

cycle_time() ->
    {ok, CycleTime} = application:get_env(erl_boids, cycle_time),
    CycleTime.

%% gen_server

init({}) ->
    self() ! dissipate,
    {ok, #state{cells = dict:new()}}.

handle_call({heat, {X, Y}}, _From, State = #state{cells = Cells}) ->
    {reply, heat_({X, Y}, Cells), State};
handle_call(map, _From, State) ->
    {reply, State#state.cells, State};
handle_call(render, _From, State) ->
    {reply, render_cells(State), State};
handle_call(Request, From, State) ->
    io:format("heatmap:handle_call(~p, ~p, State)~n", [Request, From]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({update, {Key, Value}}, State) ->
    {noreply, update_state(Key, Value, State)};
handle_cast({To}, State) ->
    {noreply, update(fun add_fun/2, To, State)};
handle_cast({From, To}, State) ->
    NewState = update(fun add_fun/2,
                      To,
                      update(fun rem_fun/2, From, State)),
    {noreply, NewState};
handle_cast(Msg, State) ->
    io:format("heatmap ~p received unrecognized message ~p~n", [self(), Msg]),
    {noreply, State}.

handle_info(dissipate, State) ->
    erlang:send_after(cycle_time(), self(), dissipate),
    {noreply, dissipate(State)};
handle_info(Info, State) ->
    io:format("heatmap:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, State, _Version) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.
