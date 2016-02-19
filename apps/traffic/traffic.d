src/numbers.erl:: include/numbers.hrl; @touch $@
src/observation_handler.erl:: include/recdef.hrl; @touch $@
src/observation_processor.erl:: include/numbers.hrl include/recdef.hrl; @touch $@
src/sequence_processor.erl:: include/recdef.hrl; @touch $@
src/traffic_sup.erl:: include/recdef.hrl; @touch $@

COMPILE_FIRST +=
