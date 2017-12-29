-ifndef(KZL_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_ledgers/include/kazoo_ledger.hrl").

-define(APP_VERSION, <<"4.0.1">>).
-define(APP_NAME, <<"kazoo_ledgers">>).

-define(LIST_BY_SOURCE, <<"ledgers/listing_by_source">>).
-define(TOTAL_BY_SOURCE, <<"ledgers/total_by_source">>).

-define(CONFIG_CAT, <<"ledgers">>).

-define(KZL_HRL, 'true').
-endif.
