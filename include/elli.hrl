-ifndef(elli_ctx).

-record(req, {method   :: elli:http_method(),
              path     :: [binary()],
              args     :: [{binary(), any()}],
              raw_path :: binary(),
              version  :: elli_http:version(),
              headers  :: elli:headers(),
              body     :: elli:body(),
              pid      :: pid(),
              socket   :: undefined | elli_tcp:socket(),
              callback :: elli_handler:callback()
             }).

-endif.


-define(EXAMPLE_CONF, [{callback, elli_example_callback},
                       {callback_args, []}]).
