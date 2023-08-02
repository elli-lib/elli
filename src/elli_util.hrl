-include_lib("kernel/include/logger.hrl").

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).
