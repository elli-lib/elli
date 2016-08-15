%%% Helpers

status({{_, Status, _}, _, _}) -> Status.
body({_, _, Body})             -> Body.
headers({_, Headers, _})       -> lists:sort(Headers).
