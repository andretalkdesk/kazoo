%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_transactions_maintenance).

-export([balance/1
        ,balance/3
        ]).

-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec balance(kz_term:ne_binary()) -> dollars().
balance(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {'ok', Balance} = wht_util:current_balance(AccountId),
    wht_util:units_to_dollars(Balance).

-spec balance(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> dollars().
balance(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {'ok', Balance} = wht_util:previous_balance(AccountId, Year, Month),
    wht_util:units_to_dollars(Balance).
