-module(naga_lang_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> naga_lang_sup:start_link().
stop(_State) -> ok.
