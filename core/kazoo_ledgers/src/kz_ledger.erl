%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledger).

-include("kzl.hrl").

-export([set_account/2]).
-export([account_id/1
        ,set_account_id/2
        ]).
-export([account_name/1
        ,set_account_name/2
        ]).
-export([unit_amount/1
        ,dollar_amount/1
        ,set_unit_amount/2
        ,set_dollar_amount/2
        ]).
-export([description/1
        ,set_description/2
        ]).
-export([source_id/1
        ,set_source_id/2
        ]).
-export([source_service/1
        ,set_source_service/2
        ]).
-export([usage_quantity/1
        ,set_usage_quantity/2
        ]).
-export([usage_type/1
        ,set_usage_type/2
        ]).
-export([usage_unit/1
        ,set_usage_unit/2
        ]).
-export([period_end/1
        ,set_period_end/2
        ]).
-export([period_start/1
        ,set_period_start/2
        ]).
-export([metadata/1
        ,set_metadata/2
        ]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).
-export([to_json/1
        ,from_json/1
        ]).
-export([credit/1
        ,debit/1
        ]).

-record(ledger, {account_id :: kz_term:ne_binary()
                ,account_name :: kz_term:ne_binary()
                ,amount = 0 :: non_neg_integer()
                ,description :: kz_term:api_binary()
                ,source_id :: kz_term:ne_binary()
                ,source_service :: kz_term:ne_binary()
                ,usage_type :: kz_term:ne_binary()
                ,usage_quantity = 0 :: non_neg_integer()
                ,usage_unit :: kz_term:api_binary()
                ,period_start :: kz_term:api_integer()
                ,period_end :: kz_term:api_integer()
                ,metadata = kz_json:new() :: kz_json:object()
                }
       ).

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-opaque ledger() :: #ledger{}.
-type setter_fun() :: {fun((ledger(), Value) -> ledger()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([ledger/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account(ledger(), kz_term:ne_binary()) -> ledger().
set_account(Ledger, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Setters = [{fun set_account_id/2, AccountId}
              ,{fun set_account_name/2, kzd_accounts:fetch_name(AccountId)}
              ],
    setters(Ledger, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(ledger()) -> kz_term:ne_binary().
account_id(#ledger{account_id=AccountId}) ->
    AccountId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(ledger(), kz_term:ne_binary()) -> ledger().
set_account_id(Ledger, AccountId) ->
    Ledger#ledger{account_id=AccountId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_name(ledger()) -> kz_term:ne_binary().
account_name(#ledger{account_name=AccountName}) ->
    AccountName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_name(ledger(), kz_term:ne_binary()) -> ledger().
set_account_name(Ledger, AccountName) ->
    Ledger#ledger{account_name=AccountName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unit_amount(ledger()) -> integer().
unit_amount(#ledger{amount=Amount}) ->
    Amount.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dollar_amount(ledger()) -> integer().
dollar_amount(#ledger{amount=Amount}) ->
    wht_util:units_to_dollars(Amount).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_unit_amount(ledger(), kz_term:ne_binary()) -> ledger().
set_unit_amount(Ledger, Amount) ->
    Ledger#ledger{amount=Amount}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_dollar_amount(ledger(), kz_term:ne_binary()) -> ledger().
set_dollar_amount(Ledger, Amount) ->
    Ledger#ledger{amount=wht_util:dollars_to_units(Amount)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(ledger()) -> kz_term:api_binary().
description(#ledger{description=Description}) ->
    Description.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_description(ledger(), kz_term:ne_binary()) -> ledger().
set_description(Ledger, Description) ->
    Ledger#ledger{description=Description}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_id(ledger()) -> kz_term:ne_binary().
source_id(#ledger{source_id=SourceId}) ->
    SourceId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_id(ledger(), kz_term:ne_binary()) -> ledger().
set_source_id(Ledger, SourceId) ->
    Ledger#ledger{source_id=SourceId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_service(ledger()) -> kz_term:ne_binary().
source_service(#ledger{source_service=SourceService}) ->
    SourceService.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_source_service(ledger(), kz_term:ne_binary()) -> ledger().
set_source_service(Ledger, SourceService) ->
    Ledger#ledger{source_service=SourceService}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_type(ledger()) -> kz_term:ne_binary().
usage_type(#ledger{usage_type=UsageType}) ->
    UsageType.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_type(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_type(Ledger, UsageType) ->
    Ledger#ledger{usage_type=UsageType}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_quantity(ledger()) -> non_neg_integer().
usage_quantity(#ledger{usage_quantity=UsageQuantity}) ->
    UsageQuantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_quantity(ledger(), non_neg_integer()) -> ledger().
set_usage_quantity(Ledger, UsageQuantity) ->
    Ledger#ledger{usage_quantity=UsageQuantity}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_unit(ledger()) -> kz_term:api_binary().
usage_unit(#ledger{usage_unit=UsageUnit}) ->
    UsageUnit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage_unit(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_unit(Ledger, UsageUnit) ->
    Ledger#ledger{usage_unit=UsageUnit}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_start(ledger()) -> non_neg_integer().
period_start(#ledger{period_start=PeriodStart}) ->
    PeriodStart.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_start(ledger(), non_neg_integer()) -> ledger().
set_period_start(Ledger, PeriodStart) ->
    Ledger#ledger{period_start=PeriodStart}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_end(ledger()) -> non_neg_integer().
period_end(#ledger{period_end=PeriodEnd}) ->
    PeriodEnd.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_period_end(ledger(), non_neg_integer()) -> ledger().
set_period_end(Ledger, PeriodEnd) ->
    Ledger#ledger{period_end=PeriodEnd}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(ledger()) -> kz_json:object().
metadata(#ledger{metadata=Metadata}) ->
    Metadata.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_metadata(ledger(), kz_json:object()) -> ledger().
set_metadata(Ledger, Metadata) ->
    Ledger#ledger{metadata=Metadata}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> ledger().
empty() ->
    #ledger{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> ledger().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(ledger(), setter_funs()) -> ledger().
setters(Ledger, Routines) ->
    lists:foldl(fun({Setter, Value}, L) ->
                        Setter(L, Value)
                end
               ,Ledger
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(ledger()) -> kz_json:object().
public_json(Ledger) ->
    kz_doc:public_fields(to_json(Ledger)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(ledger()) -> kzd_ledgers:doc().
to_json(Ledger) ->
    Setters = [{fun kzd_ledgers:set_account_id/2, account_id(Ledger)}
              ,{fun kzd_ledgers:set_source_service/2, source_service(Ledger)}
              ,{fun kzd_ledgers:set_source_id/2, source_id(Ledger)}
              ,{fun kzd_ledgers:set_description/2, description(Ledger)}
              ,{fun kzd_ledgers:set_usage_type/2, usage_type(Ledger)}
              ,{fun kzd_ledgers:set_usage_quantity/2, usage_quantity(Ledger)}
              ,{fun kzd_ledgers:set_usage_unit/2, usage_unit(Ledger)}
              ,{fun kzd_ledgers:set_period_start/2, period_start(Ledger)}
              ,{fun kzd_ledgers:set_period_end/2, period_end(Ledger)}
              ,{fun kzd_ledgers:set_metadata/2, metadata(Ledger)}
              ,{fun kzd_ledgers:set_amount/2, unit_amount(Ledger)}
              ],
    kz_doc:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_json(kzd_ledgers:doc()) -> ledger().
from_json(JObj) ->
    Setters = [{fun set_account_id/2, kzd_ledgers:account_id(JObj)}
              ,{fun set_source_service/2, kzd_ledgers:source_service(JObj)}
              ,{fun set_source_id/2, kzd_ledgers:source_id(JObj)}
              ,{fun set_description/2, kzd_ledgers:description(JObj)}
              ,{fun set_usage_type/2, kzd_ledgers:usage_type(JObj)}
              ,{fun set_usage_quantity/2, kzd_ledgers:usage_quantity(JObj)}
              ,{fun set_usage_unit/2, kzd_ledgers:usage_unit(JObj)}
              ,{fun set_period_start/2, kzd_ledgers:period_start(JObj)}
              ,{fun set_period_end/2, kzd_ledgers:period_end(JObj)}
              ,{fun set_metadata/2, kzd_ledgers:metadata(JObj)}
              ,{fun set_unit_amount/2, kzd_ledgers:amount(JObj)}
              ],
    setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec credit(ledger()) -> {'ok', ledger()} | {'error', any()}.
credit(Ledger) ->
    {'ok', Ledger}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec debit(ledger()) -> {'ok', ledger()} | {'error', any()}.
debit(Ledger) ->
    {'ok', Ledger}.
