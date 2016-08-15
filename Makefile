compile:       ; rebar3 do compile, xref
eunit:         ; rebar3 eunit
init_dialyzer: ; rebar3 dialyzer -s false
dialyzer:      ; rebar3 dialyzer -u false
# travis:        ; rebar3 do lint, xref, dialyzer, eunit
travis: eunit
