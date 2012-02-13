-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).

-record(req, {
          method,
          path,
          args,
          version,
          headers,
          body,
          pid
}).
