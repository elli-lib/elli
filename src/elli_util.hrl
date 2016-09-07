-define(I2L(I), integer_to_list(I)).
-define(B2I(I), list_to_integer(binary_to_list(I))).

-define(ERROR(Str), error_logger:error_msg(Str)).
-define(ERROR(Format,Data), error_logger:error_msg(Format, Data)).

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).
