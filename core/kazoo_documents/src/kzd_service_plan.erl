%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_service_plan).

-export([all_items_key/0
        ,merge_overrides/2
        ,new/0
        ]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([bookkeeper_vendor_id/1
        ,bookkeeper_vendor_id/2
        ,set_bookkeeper_vendor_id/2
        ]).
-export([bookkeeper_id/1
        ,bookkeeper_id/2
        ,set_bookkeeper_id/2
        ]).
-export([bookkeeper_type/1
        ,bookkeeper_type/2
        ,set_bookkeeper_type/2
        ]).
-export([applications/1
        ,applications/2
        ,set_applications/2
        ]).
-export([grouping_category/1
        ,grouping_category/2
        ,set_grouping_category/2
        ]).
-export([merge_strategy/1
        ,merge_strategy/2
        ,set_merge_strategy/2
        ]).
-export([merge_priority/1
        ,merge_priority/2
        ,set_merge_priority/2
        ]).
-export([plan/1
        ,plan/2
        ,set_plan/2
        ]).
-export([categories/1
        ,category/2
        ,category/3
        ,category_plan/1
        ,category_plan/2
        ]).
-export([items/2
        ,item/3
        ,item/4
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-type api_doc() :: kz_term:api_object().
-export_type([doc/0
             ,api_doc/0
             ,docs/0
             ]).

-define(ALL, <<"_all">>).
-define(BOOKKEEPER, <<"bookkeeper">>).
-define(BOOKKEEPER_ID, [?BOOKKEEPER, <<"id">>]).
-define(BOOKKEEPER_VENDOR, [?BOOKKEEPER, <<"vendor_id">>]).
-define(BOOKKEEPER_TYPE, [?BOOKKEEPER, <<"type">>]).
-define(APPLICATIONS, <<"applications">>).
-define(CATEGORY, <<"category">>).
-define(MERGE, <<"merge">>).
-define(MERGE_STRATEGY, [?MERGE, <<"strategy">>]).
-define(MERGE_PRIORITY, [?MERGE, <<"priority">>]).
-define(PLAN, <<"plan">>).

-define(DEFAULT_MERGE_PRIORITY, 0).
-define(DEFAULT_MERGE_STRATEGY, <<"simple">>).

-spec all_items_key() -> kz_term:ne_binary().
all_items_key() -> ?ALL.

-spec merge_overrides(doc(), kz_json:object()) -> doc().
merge_overrides(JObj, Overrides) ->
    kz_json:merge(JObj, kz_json:from_list([{?PLAN, Overrides}])).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

-spec type() -> kz_term:ne_binary().
type() -> <<"service_plan">>.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

-spec bookkeeper_vendor_id(doc()) -> kz_term:api_binary().
bookkeeper_vendor_id(JObj) ->
    bookkeeper_vendor_id(JObj, 'undefined').

-spec bookkeeper_vendor_id(doc(), Default) -> kz_term:ne_binary() | Default.
bookkeeper_vendor_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_VENDOR, JObj, Default).

-spec set_bookkeeper_vendor_id(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_vendor_id(JObj, VendorId) ->
    kz_json:set_value(?BOOKKEEPER_VENDOR, VendorId, JObj).

-spec bookkeeper_id(doc()) -> kz_term:ne_binary().
bookkeeper_id(JObj) ->
    bookkeeper_id(JObj, 'undefined').

-spec bookkeeper_id(doc(), Default) -> Default | kz_term:ne_binary().
bookkeeper_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_ID, JObj, Default).

-spec set_bookkeeper_id(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_id(JObj, BookkeeperId) ->
    kz_json:set_value(?BOOKKEEPER_ID, BookkeeperId, JObj).

-spec bookkeeper_type(doc()) -> kz_term:ne_binary().
bookkeeper_type(JObj) ->
    bookkeeper_type(JObj, 'undefined').

-spec bookkeeper_type(doc(), Default) -> Default | kz_term:ne_binary().
bookkeeper_type(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_TYPE, JObj, Default).

-spec set_bookkeeper_type(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_type(JObj, BookkeeperType) ->
    kz_json:set_value(?BOOKKEEPER_TYPE, BookkeeperType, JObj).

-spec applications(doc()) -> kz_json:object().
applications(JObj) ->
    applications(JObj, kz_json:new()).

-spec applications(doc(), Default) -> Default | kz_json:object().
applications(JObj, Default) ->
    kz_json:get_ne_json_value(?APPLICATIONS, JObj, Default).

-spec set_applications(doc(), kz_json:object()) -> doc().
set_applications(JObj, Applications) ->
    kz_json:set_value(?APPLICATIONS, Applications, JObj).

-spec grouping_category(doc()) -> kz_term:api_ne_binary().
grouping_category(JObj) ->
    grouping_category(JObj, 'undefined').

-spec grouping_category(doc(), Default) -> kz_term:ne_binary() | Default.
grouping_category(JObj, Default) ->
    kz_json:get_ne_binary_value(?CATEGORY, JObj, Default).

-spec set_grouping_category(doc(), kz_term:ne_binary()) -> doc().
set_grouping_category(JObj, Category) ->
    kz_json:set_value(?CATEGORY, Category, JObj).

-spec merge_strategy(doc()) -> kz_term:ne_binary().
merge_strategy(JObj) ->
    merge_strategy(JObj, ?DEFAULT_MERGE_STRATEGY).

-spec merge_strategy(doc(), Default) -> kz_term:ne_binary() | Default.
merge_strategy(JObj, Default) ->
    kz_json:get_ne_binary_value(?MERGE_STRATEGY, JObj, Default).

-spec set_merge_strategy(doc(), kz_term:ne_binary()) -> doc().
set_merge_strategy(JObj, Strategy) ->
    kz_json:set_value(?MERGE_STRATEGY, Strategy, JObj).

-spec merge_priority(doc()) -> kz_term:api_integer().
merge_priority(JObj) ->
    merge_priority(JObj, 'undefined').

-spec merge_priority(doc(), Default) -> integer() | Default.
merge_priority(JObj, Default) ->
    kz_json:get_integer_value(?MERGE_PRIORITY, JObj, Default).

-spec set_merge_priority(doc(), integer()) -> doc().
set_merge_priority(JObj, Priority) ->
    kz_json:set_value(?MERGE_PRIORITY, Priority, JObj).

-spec plan(doc()) -> kz_json:object().
plan(JObj) ->
    plan(JObj, kz_json:new()).

-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(JObj, Default) ->
    kz_json:get_json_value(?PLAN, JObj, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(JObj, Plan) ->
    kz_json:set_value(?PLAN, Plan, JObj).

-spec categories(doc()) -> kz_term:ne_binaries().
categories(JObj) ->
    kz_json:get_keys(?PLAN, JObj).

-spec category(doc(), kz_term:ne_binary()) -> kz_term:api_object().
category(JObj, CategoryId) ->
    category(JObj, CategoryId, 'undefined').

-spec category(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
category(JObj, CategoryId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId], JObj, Default).

-spec category_plan(doc()) -> kz_term:api_object().
category_plan(JObj) ->
    category_plan(JObj, 'undefined').

-spec category_plan(doc(), Default) -> kz_json:object() | Default.
category_plan(JObj, Default) ->
    category(JObj, ?ALL, Default).

-spec items(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
items(JObj, Category) ->
    kz_json:get_keys([?PLAN, Category], JObj).

-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
item(JObj, CategoryId, ItemId) ->
    item(JObj, CategoryId, ItemId, 'undefined').

-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
item(JObj, CategoryId, ItemId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], JObj, Default).
