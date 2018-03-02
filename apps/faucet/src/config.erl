-module(config).

-compile(export_all).

log_level() -> info.

log_modules() ->
    [login, wf, n2o_async, n2o_stream, index].

wallets() -> [testch0].

faucet_settings(testch0) ->
    #{address => <<128, 1, 64, 0, 0, 0, 0, 4>>,
      key =>
	  <<194, 124, 65, 109, 233, 236, 108, 24, 50, 151, 189,
	    216, 23, 42, 215, 220, 24, 240, 248, 115, 150, 54, 239,
	    58, 218, 221, 145, 246, 158, 15, 210, 165>>,
      url => "http://127.0.0.1:43280"}.
