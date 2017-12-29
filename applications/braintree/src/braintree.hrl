-ifndef(BRAINTREE_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(APP_NAME, <<"braintree">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).
-define(BT_DEBUG, kapps_config:get_is_true(?CONFIG_CAT, <<"debug">>, 'false')).

-include("braintree_sdk.hrl").

-define(BRAINTREE_HRL, 'true').
-endif.