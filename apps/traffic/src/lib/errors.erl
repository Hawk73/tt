-module(errors).

-export([
  no_sequence/0,
  no_data/0,
  no_solutions/0,
  data_after_red/0,
  invalid_data/0,
  internal_error/1
]).

no_sequence() -> {error, <<"The sequence isn't found">>}.

no_data() -> {error, <<"There isn't enough data">>}.

no_solutions() -> {error, <<"No solutions found">>}.

data_after_red() -> {error, <<"The red observation should be the last">>}.

invalid_data() -> {error, <<"Invalid data">>}.

internal_error(code) ->
  {error, iolist_to_binary([<<"Internal error: code ">>, code])}.
