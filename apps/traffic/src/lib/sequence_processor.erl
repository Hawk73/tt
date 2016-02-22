-module(sequence_processor).

-include("recdef.hrl").

-export([
  start/0,
  stop/0,
  create_uuid/0,
  delete/1,
  last_item/1,
  append_data/1,
  data/1
]).

start() ->
  %% @todo: заменить ETS на DETS
  ets:new(?TAB, [duplicate_bag, public, named_table]).


stop() ->
  ets:delete(?TAB).


create_uuid() ->
  %%@todo: может заюзать uuid4
  Uuid = uuid:uuid1(),
  UuidString = uuid:to_string(Uuid),
  UuidBitString = list_to_bitstring(UuidString),
  Created = ets:insert(?TAB, {UuidBitString, first}),
  {UuidBitString, Created}.


delete(Uuid) ->
  ets:delete(?TAB, Uuid).


last_item(Uuid) ->
  %% @todo: может найти долее дешевый способcase data(Uuid) of
  case data(Uuid) of
    [] -> undefined;
    Data -> lists:last(Data)
  end.


append_data(_Data = #indication{uuid=Uuid, color = <<"green">>, first_e_digit=FirstEDigit, second_e_digit=SecondEDigit}) ->
  ets:insert(?TAB, {Uuid, FirstEDigit, SecondEDigit});
append_data(_Data = #indication{uuid=Uuid, color = <<"red">>, first_e_digit=_, second_e_digit=_}) ->
  ets:insert(?TAB, {Uuid, finished}).


data(Uuid) ->
  ets:lookup(?TAB, Uuid).
