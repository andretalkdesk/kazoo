%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledgers).

-include("kzl.hrl").

-export([list_source/2
        ,list_source/4
        ]).
-export([total_source/2
        ,total_source/4
        ]).
-export([available_ledgers/1
        ]).

-define(DEFAULT_AVIALABLE_LEDGERS,
        [kz_json:from_list([{<<"name">>, <<"per-minute-voip">>}
                           ,{<<"friendly_name">>, <<"Per Minute VoIP">>}
                           ,{<<"markup_type">>, [<<"percentage">>]}
                           ])
        ]
       ).

-type ledgers() :: [kz_ledger:ledger()].
-export_type([ledgers/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list_source(kz_term:ne_binary(), kz_term:ne_binary()) ->
                 {'ok', kz_json:objects()} |
                 {'error', atom()}.
list_source(Account, Source) ->
    Options = [{'startkey', [Source]}
              ,{'endkey', [Source, kz_json:new()]}
              ],
    case kazoo_modb:get_results(Account, ?LIST_BY_SOURCE, Options) of
        {'error', _} = Error -> Error;
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]}
    end.

-spec list_source(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:seconds(), kz_time:seconds()) ->
                          {'ok', kz_json:objects()} |
                          {'error', atom()}.
list_source(Account, Source, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    Options = [{'databases', MODBs}
              ,{'startkey', [Source, CreatedFrom]}
              ,{'endkey', [Source, CreatedTo]}
              ],
    get_ranged(?LIST_BY_SOURCE, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total_source(kz_term:ne_binary(), kz_term:ne_binary()) ->
                 {'ok', kz_json:object()} |
                 {'error', atom()}.
total_source(Account, Source) ->
    Options = [{'startkey', [Source]}
              ,{'endkey', [Source, kz_json:new()]}
              ,'group'
              ],
    kazoo_modb:get_results(Account, ?TOTAL_BY_SOURCE, Options).


-spec total_source(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:seconds(), kz_time:seconds()) ->
                          {'ok', kz_json:object()} |
                          {'error', atom()}.
total_source(Account, Source, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    Options = [{'databases', MODBs}
              ,{'startkey', [Source, CreatedFrom]}
              ,{'endkey', [Source, CreatedTo]}
              ,'group'
              ],
    get_ranged(?TOTAL_BY_SOURCE, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_ranged(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:objects()} | {'error', any()}.
get_ranged(View, Options) ->
    MODBs = props:get_value('databases', Options, []),
    case MODBs =:= [] of
        'true' -> {'error', 'no_account_db'};
        'false' ->
            ViewOptions = props:filter_undefined([{'group', 'true'}
                                                 ,{'group_level', 0}
                                                 ,{'reduce', 'true'}
                                                  | props:delete('databases', Options)
                                                 ]),
            lager:debug("getting ledgers starting from ~p to ~p from dbs: ~p"
                       ,[props:get_value('startkey', ViewOptions)
                        ,props:get_value('endkey', ViewOptions)
                        ,MODBs
                        ]),
            get_ranged(View, Options, MODBs, [])
    end.

-spec get_ranged(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries(), kz_json:objects()) ->
                        {'ok', kz_json:objects()} |
                        {'error', any()}.
get_ranged(_View, _Options, [], Results) -> {'ok', Results};
get_ranged(View, Options, [MODB|MODBs], Results) ->
    case kazoo_modb:get_results(MODB, View, Options) of
        {'error', _Reason} = Error -> Error;
        {'ok', JObjs} ->
            get_ranged(View
                      ,Options
                      ,MODBs
                      ,[kz_json:get_value(<<"value">>, JObj)
                        || JObj <- JObjs
                       ] ++ Results
                      )
    end.

-spec available_ledgers(kz_term:api_binary()) -> kz_json:objects().
available_ledgers(AccountId) ->
    kapps_account_config:get_global(AccountId, <<"ledgers">>, <<"registered_ledgers">>, ?DEFAULT_AVIALABLE_LEDGERS).
