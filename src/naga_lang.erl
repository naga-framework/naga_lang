-module(naga_lang).
-description('NAGA LANG OTP Application Server').
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([table/1,lang_dir/1,lang_file/2,
         create_lang/2,delete_lang/2,
         lookup/2,
         language_list/1,
         update_po/1,
         extract_dtl/1,
         base_dir/1
         ]).

-record(state,{tables}).

%% ---- server %%
init(_)             -> {ok, #state{}}.
start_link()        -> start_link([]).
start_link(Args)    -> gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
handle_cast(_, S)   -> {noreply, S}.
handle_info(_, S)   -> {noreply, S}.
terminate(_, _)     -> ok.
code_change(_, S, _)-> {ok, S}.


handle_call({new, App}, _From, #state{tables=Tables}=State) -> 
    case proplists:get_value(App, Tables) of
        undefined -> 
            TableName = table(App),
            TableName = ets:new(TableName,[set, named_table, public, {read_concurrency, true}]),
            %error_logger:info("Translation Tables ~p created for ~p application~n",[TableName,App]),
            case language_list(App) of 
                [] -> ok;
                _ -> load_table(App) end,            
            NewTables = [{App,TableName}|Tables],
            {reply, {ok, TableName}, State#state{tables=NewTables}};
        Table ->
            {reply, {ok, Table}, State}
    end;
handle_call(_, _, State) ->  {reply, ok, State}.



env(A,K)           -> application:get_env(A,K,[]).
env(A,K,D)         -> application:get_env(A,K,D).
table(App)         -> list_to_atom(atom_to_list(App) ++ "_translations").
%FIXME case window?
lang_dir(A)        -> case env(A,lang_dir) of
                        Dir -> case Dir of 
                                "./" ++ Rest -> absolute(A,Rest);
                                "/" ++ _ -> Dir;
                                Dir -> absolute(A,Dir) end end.
base_dir(A)        -> case code:priv_dir(A) of
                        {error, _} -> {ok, Cwd} = file:get_cwd(),
                                      {B,D} = lists:foldr(fun(_,{true,P}) -> {true, P};
                                                             (X,{false,Acc}) ->
                                                                P = filename:join([Cwd,X,wf:to_list(A)]),
                                                                {filelib:is_dir(P), P}
                                                          end,{false,[]},["apps","deps"]),
                                      case B of true -> D;_-> {error, not_found} end;
                        D -> filename:join(lists:reverse(lists:reverse(filename:split(D)) -- ["priv"])) 
                      end.                                              
absolute(A,D)      -> case lists:prefix("priv",D) of
                        true -> filename:join(base_dir(A),D);
                        false -> filename:join(code:priv_dir(A),D)
                      end.

lang_file(A,L)     -> Dir = lang_dir(A),
                      Name = "strings." ++ L ++ ".po",
                      filename:join(Dir,Name).

create_lang(A,L)   -> File = lang_file(A,L),
                      {ok, IO} = file:open(File, [write]),
                      file:close(IO).

delete_lang(A,L)   -> File=lang_file(A,L), 
                      ok=file:delete(File).

lookup(A,{L,K})    -> case ets:lookup(table(A), {K, L}) of
                           [] -> undefined;[{_, Trans}] ->  Trans end.

load_table(App)    -> FunLoad = fun(X) -> load_table(table(App), App, X) end,
                      lists:map(FunLoad, language_list(App)).

language_list(App) -> Match = filename:join(lang_dir(App), "*.po"),
                      case filelib:wildcard(Match) of [] -> [];
                            Files -> language_list(App, Files, []) end.

update_po(App)     -> Fun = fun(X) -> update_po(App, X, all, []) end,
                      lists:map(Fun, language_list(App)).


language_list(_,[],Acc) -> Acc;
language_list(App, [F|T], Acc ) -> 
    case filename:basename(F, ".po") of
        "strings." ++ Lang -> language_list(App, T, [Lang|Acc]);
        _ -> language_list(App, T, Acc) end.            


load_table(TableName, App, Lang) ->
    {Untranslated, Translated} = extract_strings(App, Lang),
    lists:map(fun({O, T}) ->
                      Tr = case T of [] -> undefined; _-> T end,
                      ets:insert(TableName, {{O, Lang}, Tr})
              end, Translated).


%% @doc Update the po file with all untranslated messages for the given language (filled with blanks)
%% @spec update_po( Lang::string(), all::atom(), [] ) -> ok | {error, Reason}
update_po(App, Lang, all, []) ->
    {Untranslated, _Translated} = extract_strings(App, Lang),
    case Untranslated of
        [] -> ok;
        _ ->
            Translations = lists:map(fun(X) -> [{"orig", X}, {"trans", ""}] end, Untranslated),
            update_po(App, Lang, all, Translations)
    end;

%% @doc Update the po file for a given Language
%% @spec update_po( Lang::string(), Mode::atom(all|filled), Translation::TupleList([{"orig", "x"}, {"trans", "y"}]) ) -> ok | {error, Reason}
update_po(App, Lang, Mode, Translations) ->
    LangFile = lang_file(App, Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),  
    lists:map(fun(Message) ->
                      Original = proplists:get_value("orig", Message),
                      Translation = proplists:get_value("trans", Message),
                      case Translation of
                          "" -> 
                              case Mode of
                                  filled -> ok;
                                  all -> lang_write_to_file(IODevice, Original, Translation)
                              end;
                          _ -> lang_write_to_file(IODevice, Original, Translation)
                      end
              end, Translations),
    file:close(IODevice).

lang_write_to_file(IODevice, Original, Translation) ->
    OriginalEncoded = escape_quotes(Original),
    TranslationEncoded = escape_quotes(Translation),
    OriginalLines = re:split(OriginalEncoded,"\r\n", [{return, list}]),
    TranslationLines = re:split(TranslationEncoded,"\r\n", [{return, list}]),
    case length(OriginalLines) > 0 of
        true ->
            file:write(IODevice, io_lib:format("\nmsgid \"~s\"\n", [""])),
            lang_write_multiline_to_file(IODevice, OriginalLines);
        false ->
            file:write(IODevice, io_lib:format("\nmsgid \"~ts\"\n",[OriginalEncoded]))
    end,
    case length(TranslationLines) > 0 of
        true ->
            file:write(IODevice, io_lib:format("msgstr \"~s\"\n", [""])),
            lang_write_multiline_to_file(IODevice, TranslationLines);
        false ->
            file:write(IODevice, io_lib:format("msgstr \"~ts\"\n",[TranslationEncoded]))
    end.

lang_write_multiline_to_file(_IODevice, []) -> ok;
lang_write_multiline_to_file(IODevice, [Token|Rest]) ->
    ParsedToken = case Token of [] -> "";_ -> Token end,
    case Rest of
        [] -> file:write(IODevice, io_lib:format("\"~ts\"\n", [ParsedToken]));
        _ -> file:write(IODevice, io_lib:format("\"~ts~c~c\"\n", [ParsedToken, 92, 110]))
    end,
    lang_write_multiline_to_file(IODevice, Rest).

extract_strings(App) ->
    ModuleStrings = extract_module_strings(App),
    lists:usort(ModuleStrings).

extract_strings(App, Lang) ->
    AllStrings = extract_strings(App),
    PoStrings  = extract_po_strings(App, Lang),
    Fun = fun(S) -> case proplists:get_value(S, PoStrings) of
                         undefined -> true;_ -> false end end,
    Untranslated = lists:filter(Fun, lists:usort(AllStrings)),       
    {Untranslated, PoStrings}.

extract_po_strings(App, Lang) ->
    LangFile = lang_file(App, Lang),
    Tokens = po_scanner:scan(LangFile),
    process_po_tokens(Tokens, []).

process_po_tokens([], Acc) -> lists:reverse(Acc);
process_po_tokens([{comment, _MsgComment}|Rest], Acc) ->
    process_po_tokens(Rest, Acc);
process_po_tokens([{id, MsgId}, {str, MsgStr}|Rest], Acc) ->
    process_po_tokens(Rest, [{MsgId, MsgStr}|Acc]);
process_po_tokens([_|Rest], Acc) ->
    process_po_tokens(Rest, Acc).

extract_module_strings(App) ->
    {ok, Modules} = application:get_key(App,modules), 
    lists:foldl(fun(M, Acc) ->                                 
                        case lists:keysearch(translatable_strings, 1, M:module_info(exports)) of
                            {value, {translatable_strings, 0}} -> 
                                case lists:keysearch(translatable_blocks, 1, M:module_info(exports)) of
                                 false -> M:translatable_strings() ++ Acc;
                                 {value, {translated_blocks, 0}} -> 
                                    M:translated_blocks() ++ M:translatable_strings() ++ Acc end;
                            {value, {translatable_strings, _}} -> Acc;                                
                            false -> Acc
                        end
                end,[],Modules).
    
%% --------------------------------------------------------------------------------------
%% internal function
%% --------------------------------------------------------------------------------------
to_dict(App) ->    
    StringDictionaryList = 
        lists:map(fun(Lang) ->
                          {Lang, dict:from_list(extract_po_strings(App, Lang))}
                  end, language_list(App)),
    dict:from_list(StringDictionaryList).

process_view_file_blocks(ViewFile) ->
    {ok, BlockStrings} = blocktrans_extractor:extract(ViewFile),
    BlockStrings.

process_view_file(ViewFile) ->
    {ok, Contents} = file:read_file(ViewFile),
    {ok, Tokens} = erlydtl_scanner:scan(binary_to_list(Contents)),
    process_view_file_tokens(Tokens, []).

process_view_file_tokens([], Acc) ->
    Acc;
process_view_file_tokens([{trans_keyword, _, _}, {string_literal, _, String}|Rest], Acc) ->
    process_view_file_tokens(Rest, 
                             [unescape_string_literal(string:strip(String, both, $"))|Acc]);
process_view_file_tokens([{'_', _}, {'(', _}, {string_literal, _, String}, {')', _}|Rest], Acc) ->
    process_view_file_tokens(Rest, 
                             [unescape_string_literal(string:strip(String, both, $"))|Acc]);
process_view_file_tokens([_|Rest], Acc) ->
    process_view_file_tokens(Rest, Acc).

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).

escape_quotes(String) ->
    escape_quotes(String, []).

escape_quotes([], Acc) ->
    lists:reverse(Acc);
escape_quotes([$"|Rest], Acc) ->
    escape_quotes(Rest, [$", $\\|Acc]);
escape_quotes([H|T], Acc) ->
    escape_quotes(T, [H|Acc]).

extract_dtl(File) ->
    case catch sources_parser:parse_file(File) of
        {error, _} -> skip;
        List -> R = lists:reverse(List),
                [begin
                     [Line, Col, _, Str] = sources_parser:phrase_info([line, col, file, msgid], P)
                         ,{Line, Col, File, Str}
                 end || P <- R, is_tuple(P)]
    end.

