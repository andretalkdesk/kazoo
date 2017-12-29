%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_ledgers).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authorize/2
        ,validate/1, validate/2, validate/3
        ,put/2
        ]).

-include("crossbar.hrl").

-define(AVAILABLE, <<"available">>).
-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).
-define(LEDGER_VIEW, <<"ledgers/listing_by_source">>).
-define(NOTIFY_MSG, "failed to impact reseller ~s ledger : ~p").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ledgers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ledgers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.ledgers">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.ledgers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.ledgers">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ledgers">>, ?MODULE, 'put').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?AVAILABLE) ->
    [?HTTP_GET];
allowed_methods(?CREDIT) ->
    [?HTTP_PUT];
allowed_methods(?DEBIT) ->
    [?HTTP_PUT];
allowed_methods(_LedgerId) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_LedgerId, _LedgerEntryId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /ledgers => []
%%    /ledgers/foo => [<<"foo">>]
%%    /ledgers/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context, Path) ->
    authorize_request(Context, Path, cb_context:req_verb(Context)).

-spec authorize_request(cb_context:context(), path_token(), http_method()) ->
                               boolean() |
                               {'stop', cb_context:context()}.
authorize_request(Context, ?DEBIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, ?CREDIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, _, ?HTTP_PUT) ->
    {'stop', cb_context:add_system_error('forbidden', Context)}.

-spec authorize_create(cb_context:context()) -> boolean() |
                                                {'stop', cb_context:context()}.
authorize_create(Context) ->
    IsAuthenticated = cb_context:is_authenticated(Context),
    IsSuperDuperAdmin = cb_context:is_superduper_admin(Context),
    IsReseller = cb_context:reseller_id(Context) =:= cb_context:auth_account_id(Context),
    case IsAuthenticated
        andalso (IsSuperDuperAdmin
                 orelse IsReseller
                )
    of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /ledgers might load a list of ledgers objects
%% /ledgers/123 might load the ledgers object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_ledgers(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CREDIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?CREDIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, ?DEBIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?DEBIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, ?AVAILABLE) ->
    Available = kz_ledgers:available_ledgers(cb_context:account_id(Context)),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Available}
              ],
    cb_context:setters(Context, Setters);
validate(Context, Id) ->
    ViewOptions = [{'is_chunked', 'true'}
                  ,{'range_keymap', Id}
                  ,{'mapper', fun normalize_view_results/3}
                  ,'include_docs'
                  ],
    crossbar_view:load_modb(Context, ?LEDGER_VIEW, ViewOptions).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Ledger, Id) ->
    validate_ledger_doc(Context, Ledger, Id, cb_context:req_verb(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?CREDIT) ->
    credit_or_debit(Context, ?CREDIT);
put(Context, ?DEBIT) ->
    credit_or_debit(Context, ?DEBIT).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_ledgers(cb_context:context(), http_method()) -> cb_context:context().
validate_ledgers(Context, ?HTTP_GET) ->
    Options = [{'group', 'true'}
              ,{'group_level', 0}
              ,{'mapper', crossbar_view:map_value_fun()}
              ,{'reduce', 'true'}
              ,{'unchunkable', 'true'}
              ],
    Context1 = crossbar_view:load_modb(Context, <<"ledgers/list_by_timestamp">>, Options),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1, summary_to_dollars(kz_json:sum_jobjs(cb_context:doc(Context1))));
        _ ->
            Context1
    end.

-spec summary_to_dollars(kz_json:object()) -> kz_json:object().
summary_to_dollars(LedgersJObj) ->
    kz_json:expand(
      kz_json:from_list(
        [{Path, maybe_convert_units(lists:last(Path), Value)}
         || {Path, Value} <- kz_json:to_proplist(kz_json:flatten(LedgersJObj))
        ])).

-spec maybe_convert_units(kz_term:ne_binary(), kz_transaction:units() | T) -> kz_transaction:dollars() | T when T::any().
maybe_convert_units(<<"amount">>, 'undefined') -> 0;
maybe_convert_units(<<"amount">>, Units) -> wht_util:units_to_dollars(Units);
maybe_convert_units(_, Value) -> Value.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_ledger_doc(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_ledger_doc(Context, Ledger, Id, ?HTTP_GET) ->
    read_ledger_doc(Context, Ledger, Id).

-spec read_ledger_doc(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
read_ledger_doc(Context, Ledger, ?MATCH_MODB_PREFIX(YYYY, MM, SimpleId) = Id) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    Options = ?TYPE_CHECK_OPTION([<<"ledger">>]),
    Ctx = crossbar_doc:load(Id, cb_context:set_account_modb(Context, Year, Month), Options),
    case cb_context:resp_status(Ctx) =:= 'success'
        andalso validate_returned_ledger_doc(Ledger, Ctx)
    of
        'false' ->
            read_ledger_doc(cb_context:set_account_modb(Context, Year, Month), Ledger, SimpleId);
        Ctx1 -> Ctx1
    end;
read_ledger_doc(Context, Ledger, Id) ->
    Options = ?TYPE_CHECK_OPTION([<<"ledger">>]),
    Ctx = crossbar_doc:load(Id, Context, Options),
    case cb_context:resp_status(Ctx) =:= 'success'
        andalso validate_returned_ledger_doc(Ledger, Ctx)
    of
        'false' -> Ctx;
        Ctx1 -> Ctx1
    end.

-spec validate_returned_ledger_doc(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_returned_ledger_doc(Ledger, Context) ->
    JObj = cb_context:doc(Context),
    case kz_doc:type(JObj) =:= <<"ledger">>
        andalso kazoo_ledger:source_service(JObj) =:= Ledger
    of
        'true' -> cb_context:set_resp_data(Context, normalize_view_result(JObj));
        'false' ->
            lager:debug("document type ~s does not match the expected types", [kz_doc:type(JObj)]),
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, kz_doc:id(JObj)}]),  Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec credit_or_debit(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
credit_or_debit(Context, Action) ->
    ReqData = cb_context:req_data(Context),
    AccountId = cb_context:account_id(Context),
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, AccountId}
          ,{fun kz_ledger:set_source_service/2
           ,kz_json:get_ne_binary_value([<<"source">>, <<"service">>], ReqData)
           }
          ,{fun kz_ledger:set_source_id/2
           ,kz_json:get_ne_binary_value([<<"source">>, <<"id">>], ReqData)
           }
          ,{fun kz_ledger:set_description/2
           ,kz_json:get_ne_binary_value(<<"description">>, ReqData)
           }
          ,{fun kz_ledger:set_usage_type/2
           ,kz_json:get_integer_value([<<"usage">>, <<"type">>], ReqData)
           }
          ,{fun kz_ledger:set_usage_quantity/2
           ,kz_json:get_integer_value([<<"usage">>, <<"quantity">>], ReqData)
           }
          ,{fun kz_ledger:set_usage_unit/2
           ,kz_json:get_integer_value([<<"usage">>, <<"unit">>], ReqData)
           }
          ,{fun kz_ledger:set_period_start/2
           ,kz_json:get_integer_value([<<"period">>, <<"start">>], ReqData)
           }
          ,{fun kz_ledger:set_period_end/2
           ,kz_json:get_integer_value([<<"period">>, <<"end">>], ReqData)
           }
          ,{fun kz_ledger:set_metadata/2
           ,kz_json:get_ne_json_value(<<"metadata">>, ReqData)
           }
          ,{fun kz_ledger:set_dollar_amount/2
           ,abs(kz_json:get_integer_value(<<"amount">>, ReqData, 0))
           }
          ]
         ),
    case process_action(Action, kz_ledger:setters(Setters)) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', Ledger} ->
            maybe_impact_reseller(Context, Ledger)
    end.

-spec process_action(kz_term:ne_binary(), kz_ledger:ledger()) ->
                            {'ok', kz_ledger:ledger()} |
                            {'error', any()}.
process_action(?CREDIT, Ledger) -> kz_ledger:credit(Ledger);
process_action(?DEBIT, Ledger) -> kz_ledger:debit(Ledger).

-spec maybe_impact_reseller(cb_context:context(), kz_json:object()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger) ->
    ResellerId = cb_context:reseller_id(Context),
    ImpactReseller = kz_json:is_true(<<"impact_reseller">>, cb_context:req_json(Context))
        andalso ResellerId =/= cb_context:account_id(Context),
    maybe_impact_reseller(Context, Ledger, ImpactReseller, ResellerId).

-spec maybe_impact_reseller(cb_context:context(), kz_json:object(), boolean(), kz_term:api_binary()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger, 'false', _ResellerId) ->
    crossbar_util:response(kz_ledger:public_json(Ledger), Context);
maybe_impact_reseller(Context, Ledger, 'true', 'undefined') ->
    JObj = kz_json:from_list(
             [{kz_ledger:account_id(Ledger)
              ,kz_ledger:public_json(Ledger)
              }
             ]
            ),
    crossbar_util:response(JObj, Context);
maybe_impact_reseller(Context, Ledger, 'true', ResellerId) ->
    case kazoo_ledger:save(kz_doc:delete_revision(Ledger), ResellerId) of
        {'ok', ResellerLedger} ->
            JObj = kz_json:from_list(
                     [{kz_ledger:account_id(Ledger)
                      ,kz_ledger:public_json(Ledger)
                      },
                      {ResellerId
                      ,kz_ledger:public_json(ResellerLedger)
                      }
                     ]
                    ),
            crossbar_util:response(JObj, Context);
        {'error', Error} ->
            Props = kz_json:recursive_to_proplist(Ledger),
            kz_notify:detailed_alert(?NOTIFY_MSG, [ResellerId, Error], Props),
            crossbar_util:response(kz_doc:public_fields(Ledger), Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(cb_context:context(), kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(_Context, JObj, Acc) ->
    [normalize_view_result(kz_json:get_value(<<"doc">>, JObj)) | Acc].

-spec normalize_view_result(kz_json:object()) -> kz_json:object().
normalize_view_result(JObj) ->
    Value = wht_util:units_to_dollars(kzd_ledgers:amount(JObj)),
    Ledger = kzd_ledgers:set_amount(JObj, Value),
    Id = maybe_set_doc_modb_prefix(kz_doc:id(Ledger), kz_doc:created(Ledger)),
    kz_doc:public_fields(kz_doc:set_id(Ledger, Id)).

-spec maybe_set_doc_modb_prefix(kz_term:ne_binary(), kz_term:api_integer()) -> kz_term:ne_binary().
maybe_set_doc_modb_prefix(?MATCH_MODB_PREFIX(_,_,_)=Id, _) -> Id;
maybe_set_doc_modb_prefix(Id, Created) ->
    {Year, Month, _} = kz_term:to_date(Created),
    kazoo_modb_util:modb_id(Year, Month, Id).
