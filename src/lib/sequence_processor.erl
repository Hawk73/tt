-module(sequence_processor).

-include("recdef.hrl").

-define(DUMP_FILE_NAME, "traffic_ets.dump").

-export([
  start/0,
  stop/0,
  clear/0,
  save/0,
  create_uuid/0,
  delete/1,
  last_item/1,
  append_data/1,
  data/1
]).

start() ->
  case ets:file2tab(?DUMP_FILE_NAME) of
    {ok, ?TAB} -> ok;
    _ ->
      file:delete(?DUMP_FILE_NAME),
      ets:new(?TAB, [duplicate_bag, public, named_table])
  end.


stop() ->
  ets:delete(?TAB).


clear() ->
  file:delete(?DUMP_FILE_NAME),
  ets:delete_all_objects(?TAB).


save() ->
  ets:tab2file(?TAB, ?DUMP_FILE_NAME).


create_uuid() ->
  Uuid = uuid:get_v4(),
  UuidString = uuid:uuid_to_string(Uuid),
  UuidBitString = list_to_bitstring(UuidString),
  Created = ets:insert(?TAB, {UuidBitString, empty}),
  {UuidBitString, Created}.


delete(Uuid) ->
  ets:delete(?TAB, Uuid).


last_item(Uuid) ->
  %% @todo: может найти долее дешевый способ
  case data(Uuid) of
    [] -> undefined;
    Data -> lists:last(Data)
  end.


append_data(_Data = #indication{uuid=Uuid, color=green, first_e_digit=FirstEDigit, second_e_digit=SecondEDigit}) ->
  ets:insert(?TAB, {Uuid, FirstEDigit, SecondEDigit});
append_data(_Data = #indication{uuid=Uuid, color=red, first_e_digit=_, second_e_digit=_}) ->
  ets:insert(?TAB, {Uuid, finished}).


data(Uuid) ->
  ets:lookup(?TAB, Uuid).
