-module(sequence_processor).

-include("recdef.hrl").

-export([
  init/0,
  create_uuid/0,
  exists_uuid/1,
  append_data/3,
  data/1
]).

init() ->
  %% @todo: заменить ETS на DETS
  ets:new(?TAB, [duplicate_bag, public, named_table]).


create_uuid() ->
  %%@todo: может заюзать uuid4
  Uuid = uuid:uuid1(),
  UuidString = uuid:to_string(Uuid),
  UuidBitString = list_to_bitstring(UuidString),
  Created = ets:insert(?TAB, {UuidBitString, []}),
  {UuidBitString, Created}.


exists_uuid(Uuid) ->
%%  @todo: заюзать info или lookup_element
  [] =/= ets:lookup(?TAB, Uuid).


append_data(Uuid, FirstDigit, SecondDigit) ->
  %% @todo: привести к integer
  %% @todo: запоминать цвет ? надо выдавать ошибку "The red observation should be the last"
  ets:insert(?TAB, {Uuid, FirstDigit, SecondDigit}).


data(Uuid) ->
  ets:lookup(?TAB, Uuid).
