
%%  0   1   2   3   4   5   6   7   8   9
%%  -       -   -       -   -   -   -   -
%% | |   |   |   | | | |   |     | | | | |
%%          -   -   -   -   -       -   -
%% | |   | |     |   |   | | |   | | |   |
%%  -       -   -       -   -       -   -

%% 1_1_1_0_1_1_1 = 119
-define(ZERO, 2#1110111).
%% 0_0_1_0_0_1_0 = 18
-define(ONE, 2#0010010).
%% 1_0_1_1_1_0_1 = 93
-define(TWO, 2#1011101).
%% 1_0_1_1_0_1_1 = 91
-define(THREE, 2#1011011).
%% 0_1_1_1_0_1_0 = 58
-define(FOUR, 2#0111010).
%% 1_1_0_1_0_1_1 = 107
-define(FIVE, 2#1101011).
%% 1_1_0_1_1_1_1 = 111
-define(SIX, 2#1101111).
%% 1_0_1_0_0_1_0 = 82
-define(SEVEN, 2#1010010).
%% 1_1_1_1_1_1_1 = 127
-define(EIGHT, 2#1111111).
%% 1_1_1_1_0_1_1 = 123
-define(NINE, 2#1111011).


-define(POSSIBLE_FIRST_NUMBERS, [?ZERO, ?ONE, ?TWO, ?THREE, ?FOUR, ?FIVE, ?SIX, ?SEVEN, ?EIGHT, ?NINE]).
-define(POSSIBLE_SECOND_NUMBERS, [?ONE, ?TWO, ?THREE, ?FOUR, ?FIVE, ?SIX, ?SEVEN, ?EIGHT, ?NINE]).


decode_digit(Encoded) ->
  case Encoded of
    ?ZERO -> 0;
    ?ONE -> 1;
    ?TWO -> 2;
    ?THREE -> 3;
    ?FOUR -> 4;
    ?FIVE -> 5;
    ?SIX -> 6;
    ?SEVEN -> 7;
    ?EIGHT -> 8;
    ?NINE -> 9
  end.
