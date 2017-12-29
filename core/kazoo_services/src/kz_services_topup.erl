%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_topup).

-export([init/2]).
-export([should_topup/1
        ,should_topup/2
        ]).
-export([top_up/2]).

-include("services.hrl").

-type error() :: 'topup_disabled' |
                 'topup_undefined' |
                 'amount_undefined' |
                 'threshold_undefined' |
                 'balance_above_threshold' |
                 'amount_and_threshold_undefined' |
                 'topup_daily_limit' |
                 atom().


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(kz_term:api_binary(), integer()) ->
                  {'ok', kz_json:object()} |
                  {'error', error()}.
init(Account, AvailableUnits) ->
    case get_top_up(Account) of
        {'error', _}=E -> E;
        {'ok', ReplinishUnits, ThresholdUnits} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[Account
                       ,wht_util:units_to_dollars(AvailableUnits)
                       ,wht_util:units_to_dollars(ThresholdUnits)
                       ]),
            AccountId = kz_util:format_account_id(Account, 'raw'),
            maybe_top_up(AccountId, AvailableUnits, ReplinishUnits, ThresholdUnits)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_topup(kz_term:ne_binary()) -> boolean().
should_topup(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', AvailableUnits} -> should_topup(AccountId, AvailableUnits);
        {'error', _} -> 'false'
    end.

-spec should_topup(kz_term:ne_binary(), integer()) -> boolean().
should_topup(AccountId, AvailableUnits) ->
    case get_top_up(AccountId) of
        {'error', _} -> 'false';
        {'ok', _ReplinishUnits, ThresholdUnits} ->
            lager:info("checking if account ~s balance $~w is below top up threshold $~w"
                      ,[AccountId
                       ,wht_util:units_to_dollars(AvailableUnits)
                       ,wht_util:units_to_dollars(ThresholdUnits)
                       ]),
            should_topup(AccountId, AvailableUnits, ThresholdUnits) =:= 'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_topup(kz_term:ne_binary(), integer(), integer()) ->
                          'true' |
                          {'error', error()}.
should_topup(AccountId, AvailableUnits, ThresholdUnits) when AvailableUnits =< ThresholdUnits ->
    case find_auto_topup_transactions(AccountId) =/= []
        orelse find_auto_topup_ledgers(AccountId) =/= []
    of
        'true' ->
            lager:info("today auto top up for ~s already done, skipping..."
                      ,[AccountId]),
            {'error', 'topup_daily_limit'};
        'false' ->
            lager:info("no top up transactions or ledgers found for ~s, processing..."
                      ,[AccountId]),
            'true'
    end;
should_topup(_AccountId, _AvailableUnits, _ThresholdUnits) ->
    lager:debug("balance (~p) is still > to threshold (~p) for account ~s"
               ,[wht_util:units_to_dollars(_AvailableUnits)
                ,wht_util:units_to_dollars(_ThresholdUnits)
                ,_AccountId
                ]
               ),
    {'error', 'balance_above_threshold'}.

-spec find_auto_topup_transactions(kz_term:ne_binary()) -> boolean().
find_auto_topup_transactions(AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason} = Error ->
            lager:warning("failed to fetch recent transactions for ~s: ~p", [AccountId, _Reason]),
            Error;
        {'ok', Transactions} ->
            kz_transactions:filter_by_reason(wht_util:topup(), Transactions)
    end.

-spec find_auto_topup_ledgers(kz_term:ne_binary() | kz_ledgers:ledgers()) -> boolean().
find_auto_topup_ledgers(?NE_BINARY=AccountId) ->
    To = kz_time:now_s(),
    From = To - ?SECONDS_IN_DAY,
    %% TODO: filter out usage in the db query...
    case kz_ledgers:list_source(AccountId, <<"kazoo-services">>, From, To) of
        {'error', _Reason} = Error ->
            lager:warning("failed to fetch recent ledgers for ~s: ~p", [AccountId, _Reason]),
            Error;
        {'ok', Ledgers} -> find_auto_topup_ledgers(Ledgers)
    end;
find_auto_topup_ledgers(Ledgers) ->
    [Ledger || Ledger <- Ledgers
                   ,is_auto_topup_ledger(Ledger)              
    ].

-spec is_auto_topup_ledger(kz_json:object()) -> boolean().
is_auto_topup_ledger(Ledger) ->
    kazoo_ledger:source_service(Ledger) =:= <<"kazoo-services">>
        andalso kazoo_ledger:metadata(Ledger, <<"type">>) =:= <<"topup">>
        andalso kazoo_ledger:metadata(Ledger, <<"trigger">>) =:= <<"automatic">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_top_up(kz_term:api_binary() | kzd_accounts:doc()) ->
                        {'error', error()} |
                        {'ok', integer(), integer()}.
get_top_up(<<_/binary>> = Account) ->
    case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            case kzd_accounts:fetch(Account) of
                {'error', _E}=Error ->
                    lager:error("could not open account ~s: ~p", [Account, _E]),
                    Error;
                {'ok', AccountJObj} -> get_top_up(kz_json:get_value(<<"topup">>, AccountJObj))
            end
    end;
get_top_up('undefined') -> {'error', 'topup_undefined'};
get_top_up(JObj) ->
    case
        {kz_json:get_float_value(<<"amount">>, JObj)
        ,kz_json:get_float_value(<<"threshold">>, JObj)
        }
    of
        {'undefined', _} -> {'error', 'amount_undefined'};
        {_, 'undefined'} -> {'error', 'threshold_undefined'};
        {Amount, Threshold} when Amount > 0
                                 andalso Threshold > 0 ->
            {'ok'
            ,wht_util:dollars_to_units(Amount)
            ,wht_util:dollars_to_units(Threshold)
            };
        {_, _} -> {'error', 'amount_and_threshold_undefined'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_top_up(kz_term:ne_binary(), integer(), integer(), integer()) ->
                          {'ok', kz_json:object()} |
                          {'error', error()}.
maybe_top_up(AccountId, AvailableUnits, ReplinishUnits, ThresholdUnits) ->
    case should_topup(AccountId, AvailableUnits, ThresholdUnits) of
        'true' -> top_up(AccountId, ReplinishUnits);
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec top_up(kz_term:ne_binary(), integer()) -> {'ok', kz_json:object()} | {'error', any()}.
top_up(AccountId, ReplinishUnits) ->
    top_up(AccountId, ReplinishUnits, <<"automatic">>).

-spec top_up(kz_term:ne_binary(), integer(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
top_up(AccountId, ReplinishUnits, Trigger) ->
    case kz_services_bookkeeper:quick_sale(AccountId, ReplinishUnits, wht_util:topup()) of
        {'ok', BookkeeperResults} ->
            update_ledgers(AccountId, ReplinishUnits, Trigger, BookkeeperResults);
        {'error', _} = Error -> Error
    end.

-spec update_ledgers(kz_term:ne_binary(), integer(), kz_term:ne_binary(), kz_json:object()) -> {'ok', kz_json:object()} | {'error', any()}.
update_ledgers(AccountId, ReplinishUnits, Trigger, BookkeeperResults) ->
    Metadata = [{<<"type">>, <<"topup">>}
               ,{<<"trigger">>, Trigger}
               ,{<<"bookkeeper">>, BookkeeperResults}
               ],
    Props = [{<<"amount">>, ReplinishUnits}
            ,{<<"description">>, <<"topup">>}
            ,{<<"metadata">>, kz_json:from_list(Metadata)}
            ],
    ServiceName = <<"kazoo-services">>,
    ServiceId = kz_doc:calculate_document_hash(BookkeeperResults),
    case kz_ledger:credit(AccountId, ServiceName, ServiceId, [], Props) of
        {'error', _} = Error -> Error;
        {'ok', Ledger} ->
            JObj = kz_ledger:public_json(Ledger),
            AvailableUnits = wht_util:current_balance(AccountId),
            {'ok', kz_json:set_value([<<"metadata">>, <<"new_balance">>], AvailableUnits, JObj)}
    end.
