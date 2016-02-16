-module(naga_lang_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) -> 
          { ok, { { one_for_one, 100, 10 }, 
                [
                  ?CHILD(naga_lang, worker, [])
                ] 
          }}.