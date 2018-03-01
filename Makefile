compile:       ; rebar3 do compile, xref
eunit:         ; rebar3 eunit
init_dialyzer: ; rebar3 dialyzer -s false
dialyzer:      ; rebar3 dialyzer -u false
check:         ; rebar3 do xref, dialyzer, eunit
travis: check  ; echo rebar3 coveralls send
