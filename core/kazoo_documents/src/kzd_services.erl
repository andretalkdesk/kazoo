%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_services).

-export([new/0]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([account_quantities/1
        ,account_quantities/2
        ,set_account_quantities/2
        ]).
-export([cascade_quantities/1
        ,cascade_quantities/2
        ,set_cascade_quantities/2
        ]).
-export([manual_quantities/1
        ,manual_quantities/2
        ,set_manual_quantities/2
        ]).
-export([overrides/1
        ,overrides/2
        ,set_overrides/2
        ]).
-export([ratedeck_id/1
        ,ratedeck_id/2
        ,set_ratedeck_id/2
        ]).
-export([ratedeck_name/1
        ,ratedeck_name/2
        ,set_ratedeck_name/2
        ]).
-export([plans/1
        ,plans/2
        ,set_plans/2
        ]).
-export([plan_ids/1
        ,plan/2
        ,plan/3
        ,set_plan/3
        ]).
-export([plan_vendor_id/2
        ,plan_vendor_id/3
        ,set_plan_vendor_id/3
        ]).
-export([plan_overrides/2
        ,plan_overrides/3
        ,set_plan_overrides/3
        ]).
-export([reseller_id/1
        ,reseller_id/2
        ,set_reseller_id/2
        ,is_reseller/1
        ,is_reseller/2
        ,set_is_reseller/2
        ]).
-export([tree/1
        ,tree/2
        ,set_tree/2
        ]).
-export([status_good/0
        ,status_delinquent/0
        ,status/1
        ,status/2
        ,set_status/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(QUANTITIES, <<"quantities">>).
-define(ACCOUNT_QUANTITIES, [?QUANTITIES, <<"account">>]).
-define(CASCADE_QUANTITIES, [?QUANTITIES, <<"cascade">>]).
-define(MANUAL_QUANTITIES, [?QUANTITIES, <<"manual">>]).
-define(PLANS, <<"plans">>).
-define(PLAN(PlanId), [?PLANS, PlanId]).
-define(PLAN_VENDOR_ID(PlanId), [?PLANS, PlanId, <<"vendor_id">>]).
-define(PLAN_OVERRIDES(PlanId), [?PLANS, PlanId, <<"overrides">>]).
-define(OVERRIDES, <<"overrides">>).
-define(RATEDECK, <<"ratedeck">>).
-define(RATEDECK_ID, [?RATEDECK, <<"id">>]).
-define(RATEDECK_NAME, [?RATEDECK, <<"name">>]).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(IS_RESELLER, <<"pvt_reseller">>).
-define(TREE, <<"pvt_tree">>).
-define(STATUS, <<"pvt_status">>).
-define(IS_DELETED, <<"pvt_deleted">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

-spec type() -> kz_term:ne_binary().
type() -> <<"service">>.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

-spec account_quantities(doc()) -> kz_json:object().
account_quantities(JObj) ->
    account_quantities(JObj, kz_json:new()).

-spec account_quantities(doc(), Default) -> kz_json:object() | Default.
account_quantities(JObj, Default) ->
    kz_json:get_json_value(?ACCOUNT_QUANTITIES, JObj, Default).

-spec set_account_quantities(doc(), kz_json:object()) -> kz_json:object().
set_account_quantities(JObj, AccountQuantities) ->
    kz_json:set_value(?ACCOUNT_QUANTITIES, AccountQuantities, JObj).

-spec cascade_quantities(doc()) -> kz_json:object().
cascade_quantities(JObj) ->
    cascade_quantities(JObj, kz_json:new()).

-spec cascade_quantities(doc(), Default) -> kz_json:object() | Default.
cascade_quantities(JObj, Default) ->
    kz_json:get_json_value(?CASCADE_QUANTITIES, JObj, Default).

-spec set_cascade_quantities(doc(), kz_json:object()) -> kz_json:object().
set_cascade_quantities(JObj, CascadeQuantities) ->
    kz_json:set_value(?CASCADE_QUANTITIES, CascadeQuantities, JObj).

-spec manual_quantities(doc()) -> kz_json:object().
manual_quantities(JObj) ->
    manual_quantities(JObj, kz_json:new()).

-spec manual_quantities(doc(), Default) -> kz_json:object() | Default.
manual_quantities(JObj, Default) ->
    kz_json:get_json_value(?MANUAL_QUANTITIES, JObj, Default).

-spec set_manual_quantities(doc(), kz_json:object()) -> kz_json:object().
set_manual_quantities(JObj, ManualQuantities) ->
    kz_json:set_value(?MANUAL_QUANTITIES, ManualQuantities, JObj).

-spec overrides(doc()) -> kz_json:object().
overrides(JObj) ->
    overrides(JObj, kz_json:new()).

-spec overrides(doc(), Default) -> kz_json:object() | Default.
overrides(JObj, Default) ->
    kz_json:get_json_value(?OVERRIDES, JObj, Default).

-spec set_overrides(doc(), kz_json:object()) -> doc().
set_overrides(JObj, Overrides) ->
    kz_json:set_value(?OVERRIDES, Overrides, JObj).

-spec ratedeck_id(kz_json:object()) -> kz_term:api_binary().
ratedeck_id(JObj) ->
    ratedeck_id(JObj, 'undefined').

-spec ratedeck_id(doc(), Default) -> kz_term:ne_binary() | Default.
ratedeck_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?RATEDECK_ID, JObj, Default).

-spec set_ratedeck_id(doc(), kz_term:ne_binary()) -> doc().
set_ratedeck_id(JObj, RatedeckId) ->
    kz_json:set_value(?RATEDECK_ID, RatedeckId, JObj).

-spec ratedeck_name(kz_json:object()) -> kz_term:api_binary().
ratedeck_name(JObj) ->
    ratedeck_name(JObj, 'undefined').

-spec ratedeck_name(doc(), Default) -> kz_term:ne_binary() | Default.
ratedeck_name(JObj, Default) ->
    kz_json:get_ne_binary_value(?RATEDECK_NAME, JObj, Default).

-spec set_ratedeck_name(doc(), kz_term:ne_binary()) -> doc().
set_ratedeck_name(JObj, RatedeckName) ->
    kz_json:set_value(?RATEDECK_NAME, RatedeckName, JObj).

-spec plans(doc()) -> kz_json:object().
plans(JObj) ->
    plans(JObj, kz_json:new()).

-spec plans(doc(), Default) -> kz_json:object() | Default.
plans(JObj, Default) ->
    kz_json:get_json_value(?PLANS, JObj, Default).

-spec set_plans(doc(), kz_json:object()) -> doc().
set_plans(JObj, Plans) ->
    kz_json:set_value(?PLANS, Plans, JObj).

-spec plan_ids(doc()) -> kz_term:ne_binaries().
plan_ids(JObj) ->
    kz_json:get_keys(?PLANS, JObj).

-spec plan(doc(), kz_term:ne_binary()) -> kz_json:object().
plan(JObj, PlanId) ->
    plan(JObj, PlanId, kz_json:new()).

-spec plan(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan(JObj, PlanId, Default) ->
    kz_json:get_ne_json_value([?PLANS, PlanId], JObj, Default).

-spec set_plan(doc(), kz_term:ne_binary(), kz_term:api_object()) -> doc().
set_plan(JObj, PlanId, 'undefined') ->
    kz_json:delete_key(?PLAN(PlanId), JObj);
set_plan(JObj, PlanId, Plan) ->
    kz_json:set_value(?PLAN(PlanId), Plan, JObj).

-spec plan_vendor_id(doc(), kz_term:ne_binary()) -> kz_term:api_binary().
plan_vendor_id(JObj, PlanId) ->
    plan_vendor_id(JObj, PlanId, 'undefined').

-spec plan_vendor_id(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | Default.
plan_vendor_id(JObj, PlanId, Default) ->
    kz_json:get_ne_binary_value(?PLAN_VENDOR_ID(PlanId), JObj, Default).

-spec set_plan_vendor_id(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc().
set_plan_vendor_id(JObj, PlanId, VendorId) ->
    kz_json:set_value(?PLAN_VENDOR_ID(PlanId), VendorId, JObj).

-spec plan_overrides(doc(), kz_term:ne_binary()) -> kz_json:object().
plan_overrides(JObj, PlanId) ->
    plan_overrides(JObj, PlanId, kz_json:new()).

-spec plan_overrides(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan_overrides(JObj, PlanId, Default) ->
    kz_json:get_ne_json_value(?PLAN_OVERRIDES(PlanId), JObj, Default).

-spec set_plan_overrides(doc(), kz_term:ne_binary(), kz_json:object()) -> doc().
set_plan_overrides(JObj, PlanId, Overrides) ->
    kz_json:set_value(?PLAN_OVERRIDES(PlanId), Overrides, JObj).

-spec reseller_id(doc()) -> kz_term:api_binary().
reseller_id(JObj) ->
    reseller_id(JObj, 'undefined').

-spec reseller_id(doc(), Default) -> kz_term:ne_binary() | Default.
reseller_id(JObj, Default) ->
    kz_json:get_value(?RESELLER_ID, JObj, Default).

-spec set_reseller_id(doc(), kz_term:api_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value(?RESELLER_ID, ResellerId, JObj).

-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    is_reseller(JObj, 'false').

-spec is_reseller(doc(), Default) -> boolean() | Default.
is_reseller(JObj, Default) ->
    kz_json:is_true(?IS_RESELLER, JObj, Default).

-spec set_is_reseller(doc(), boolean()) -> doc().
set_is_reseller(JObj, IsReseller) ->
    kz_json:set_value(?IS_RESELLER, IsReseller, JObj).

-spec tree(doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    tree(JObj, []).

-spec tree(doc(), Default) -> kz_term:ne_binaries() | Default.
tree(JObj, Default) ->
    kz_json:get_value(?TREE, JObj, Default).

-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?TREE, Tree, JObj).

-spec status_good() -> kz_term:ne_binary().
status_good() -> <<"good_standing">>.

-spec status_delinquent() -> kz_term:ne_binary().
status_delinquent() -> <<"delinquent">>.

-spec status(doc()) -> kz_term:ne_binary().
status(JObj) ->
    status(JObj, status_good()).

-spec status(doc(), Default) -> kz_term:ne_binary() | Default.
status(JObj, Default) ->
    kz_json:get_value(?STATUS, JObj, Default).

-spec set_status(doc(), kz_term:api_binary()) -> doc().
set_status(JObj, Status) ->
    kz_json:set_value(?STATUS, Status, JObj).
