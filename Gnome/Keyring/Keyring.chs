{- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Keyring
	( getDefaultKeyring
	, setDefaultKeyring
	, listKeyringNames
	, create
	, delete
	, lock
	, lockAll
	, unlock
	, getInfo
	, setInfo
	, changePassword
	, listItemIDs
	) where

import Data.Text.Lazy (Text)
import Gnome.Keyring.Operation.Internal (Operation, operation)
import Gnome.Keyring.Types (Result, CancellationKey (..))

-- Import unqualified for c2hs
import Foreign
import Foreign.C
import Gnome.Keyring.FFI
import Gnome.Keyring.KeyringInfo.Internal

-- get_default_keyring
getDefaultKeyring :: Operation (Maybe Text)
getDefaultKeyring = operation
	get_default_keyring
	get_default_keyring_sync

{# fun get_default_keyring
	{ callbackToPtr `GetNullableStringCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_default_keyring_sync
	{ alloca- `Maybe Text' stealNullableTextPtr*
	} -> `Result' result #}

-- set_default_keyring
setDefaultKeyring :: Text -> Operation ()
setDefaultKeyring k = operation
	(set_default_keyring k)
	(set_default_keyring_sync k)

{# fun set_default_keyring
	{ withText* `Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_default_keyring_sync
	{ withText* `Text'
	} -> `(Result, ())' resultAndTuple #}

-- list_keyring_names
listKeyringNames :: Operation [Text]
listKeyringNames = operation
	list_keyring_names
	list_keyring_names_sync

{# fun list_keyring_names
	{ callbackToPtr `GetTextListCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun list_keyring_names_sync
	{ alloca- `[Text]' stealTextList*
	} -> `Result' result #}

-- create
create :: Text -> Maybe Text -> Operation ()
create k p = operation (c_create k p) (create_sync k p)

{# fun create as c_create
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun create_sync
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- delete
delete :: Text -> Operation ()
delete k = operation (c_delete k) (delete_sync k)

{# fun delete as c_delete
	{ withText* `Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun delete_sync
	{ withText* `Text'
	} -> `(Result, ())' resultAndTuple #}

-- lock
lock :: Maybe Text -> Operation ()
lock k = operation (c_lock k) (lock_sync k)

{# fun lock as c_lock
	{ withNullableText* `Maybe Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun lock_sync
	{ withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- lock_all
lockAll :: Operation ()
lockAll = operation lock_all lock_all_sync

{# fun lock_all
	{ callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun lock_all_sync
	{} -> `(Result, ())' resultAndTuple #}

-- unlock
unlock :: Maybe Text -> Maybe Text -> Operation ()
unlock k p = operation (c_unlock k p) (unlock_sync k p)

{# fun unlock as c_unlock
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unlock_sync
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- get_info
getInfo :: Maybe Text -> Operation KeyringInfo
getInfo k = operation (get_info k) (get_info_sync k)

{# fun get_info
	{ withNullableText* `Maybe Text'
	, callbackToPtr `GetKeyringInfoCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_info_sync
	{ withNullableText* `Maybe Text'
	, alloca- `KeyringInfo' stealKeyringInfoPtr*
	} -> `Result' result #}

-- set_info
setInfo :: Maybe Text -> KeyringInfo -> Operation ()
setInfo k info = operation (set_info k info) (set_info_sync k info)

{# fun set_info
	{ withNullableText* `Maybe Text'
	, withKeyringInfo* `KeyringInfo'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_info_sync
	{ withNullableText* `Maybe Text'
	, withKeyringInfo* `KeyringInfo'
	} -> `(Result, ())' resultAndTuple #}

-- change_password
changePassword :: Text -> Maybe Text -> Maybe Text -> Operation ()
changePassword k op np = operation
	(change_password k op np)
	(change_password_sync k op np)

{# fun change_password
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun change_password_sync
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- list_item_ids
listItemIDs :: Maybe Text -> Operation [Integer]
listItemIDs name = operation
	(list_item_ids name)
	(list_item_ids_sync name)

{# fun list_item_ids
	{ withNullableText* `Maybe Text'
	, callbackToPtr `GetWordListCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun list_item_ids_sync
	{ withNullableText* `Maybe Text'
	, alloca- `[Integer]' stealWordList*
	} -> `Result' result #}
