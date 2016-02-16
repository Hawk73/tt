-module(sequence_processor).

-include("recdef.hrl").

-export([
  create_uuid/0,
  exists_uuid/1,
  append_data/2
]).


create_uuid() ->
  %%@todo: может заюзать uuid4
  BinaryUuid = uuid:uuid1(),
  Uuid = uuid:to_string(BinaryUuid),
  Created = ets:insert(?TAB, {Uuid, []}),
  {Uuid, Created}.


exists_uuid(Uuid) ->
  UuidString = binary_to_list(Uuid),
  [] =/= ets:lookup(?TAB, UuidString).


append_data(_Numbers, _Uuid) ->
  %% @todo: реализовать вставку данных
  ok.
