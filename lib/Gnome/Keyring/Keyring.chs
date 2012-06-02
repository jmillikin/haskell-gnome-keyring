{-# LANGUAGE ForeignFunctionInterface #-}

-- Copyright (C) 2009-2011 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
module Gnome.Keyring.Keyring
	( KeyringName
	, getDefaultKeyring
	, setDefaultKeyring
	, listKeyringNames
	, create
	, delete
	, changePassword
	, listItemIDs
	, lock
	, lockAll
	, unlock
	, KeyringInfo (..)
	, KeyringInfoToken
	, getInfo
	, setInfo
	) where

import           Control.Exception (bracket)

import           Gnome.Keyring.Item (findItems) -- for docs
import           Gnome.Keyring.ItemInfo
import           Gnome.Keyring.KeyringInfo
import           Gnome.Keyring.Internal.FFI
import           Gnome.Keyring.Internal.Operation
import           Gnome.Keyring.Internal.Types

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

-- | Get the default keyring name. If no default keyring exists, then
-- 'Nothing' will be returned.
getDefaultKeyring :: Operation (Maybe KeyringName)
getDefaultKeyring = maybeStringOperation
	get_default_keyring
	get_default_keyring_sync

{# fun get_default_keyring
	{ id `GetStringCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_default_keyring_sync
	{ alloca- `Maybe String' stealNullableUtf8Ptr*
	} -> `Result' result #}

stealNullableUtf8Ptr :: Ptr CString -> IO (Maybe String)
stealNullableUtf8Ptr ptr = bracket (peek ptr) free peekNullableUtf8

-- | Change the default keyring.
setDefaultKeyring :: KeyringName -> Operation ()
setDefaultKeyring k = voidOperation
	(set_default_keyring k)
	(set_default_keyring_sync k)

{# fun set_default_keyring
	{ withUtf8* `String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_default_keyring_sync
	{ withUtf8* `String'
	} -> `(Result, ())' resultAndTuple #}

-- | Get a list of keyring names. If no keyrings exist, an empty list
-- will be returned.
listKeyringNames :: Operation [KeyringName]
listKeyringNames = stringListOperation
	list_keyring_names
	list_keyring_names_sync

{# fun list_keyring_names
	{ id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun list_keyring_names_sync
	{ alloca- `[String]' stealUtf8List*
	} -> `Result' result #}

stealUtf8List :: Ptr (Ptr ()) -> IO [String]
stealUtf8List ptr = bracket (peek ptr)
	{# call gnome_keyring_string_list_free #}
	(mapGList peekUtf8)

-- | Create a new keyring with the specified name. In most cases, 'Nothing'
-- will be passed as the password, which will prompt the user to enter a
-- password of their choice.
create :: KeyringName -> Maybe String -> Operation ()
create k p = voidOperation (c_create k p) (create_sync k p)

{# fun create as c_create
	{ withUtf8* `String'
	, withNullableUtf8* `Maybe String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun create_sync
	{ withUtf8* `String'
	, withNullableUtf8* `Maybe String'
	} -> `(Result, ())' resultAndTuple #}

-- | Delete a keyring. Once a keyring is deleted, there is no mechanism for
-- recovery of its contents.
delete :: KeyringName -> Operation ()
delete k = voidOperation (c_delete k) (delete_sync k)

{# fun delete as c_delete
	{ withUtf8* `String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun delete_sync
	{ withUtf8* `String'
	} -> `(Result, ())' resultAndTuple #}

-- | Lock a keyring, so that its contents may not be accessed without first
-- supplying a password.
--
-- Most keyring operations involving items require that the keyring first be
-- unlocked. One exception is 'findItems' and related computations.
lock :: Maybe KeyringName -> Operation ()
lock k = voidOperation (c_lock k) (lock_sync k)

{# fun lock as c_lock
	{ withNullableUtf8* `Maybe String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun lock_sync
	{ withNullableUtf8* `Maybe String'
	} -> `(Result, ())' resultAndTuple #}

-- | Lock all the keyrings, so that their contents may not be accessed
-- without first unlocking them with a password.
lockAll :: Operation ()
lockAll = voidOperation lock_all lock_all_sync

{# fun lock_all
	{ id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun lock_all_sync
	{} -> `(Result, ())' resultAndTuple #}

-- | Unlock a keyring, so that its contents may be accessed. In most cases,
-- 'Nothing' will be specified as the password, which will prompt the user
-- to enter the correct password.
--
-- Most keyring operations involving items require that the keyring first be
-- unlocked. One exception is 'findItems' and related computations.
unlock :: Maybe KeyringName -> Maybe String -> Operation ()
unlock k p = voidOperation (c_unlock k p) (unlock_sync k p)

{# fun unlock as c_unlock
	{ withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unlock_sync
	{ withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about the keyring.
getInfo :: Maybe KeyringName -> Operation KeyringInfo
getInfo k = keyringInfoOperation (get_info k) (get_info_sync k)

{# fun get_info
	{ withNullableUtf8* `Maybe String'
	, id `GetKeyringInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_info_sync
	{ withNullableUtf8* `Maybe String'
	, alloca- `KeyringInfo' stealKeyringInfoPtr*
	} -> `Result' result #}

-- | Set flags and info for the keyring. The only fields in the
-- 'KeyringInfo' which are used are 'keyringLockOnIdle' and
-- 'keyringLockTimeout'.
setInfo :: Maybe KeyringName -> KeyringInfo -> Operation ()
setInfo k info = voidOperation
	(set_info k info)
	(set_info_sync k info)

{# fun set_info
	{ withNullableUtf8* `Maybe String'
	, withKeyringInfo* `KeyringInfo'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_info_sync
	{ withNullableUtf8* `Maybe String'
	, withKeyringInfo* `KeyringInfo'
	} -> `(Result, ())' resultAndTuple #}

-- | Change the password for a keyring. In most cases, 'Nothing' would
-- be specified for both the original and new passwords to allow the user
-- to type both.
changePassword :: KeyringName
               -> Maybe String -- ^ Old password
               -> Maybe String -- ^ New password
               -> Operation ()
changePassword k op np = voidOperation
	(change_password k op np)
	(change_password_sync k op np)

{# fun change_password
	{ withUtf8* `String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun change_password_sync
	{ withUtf8* `String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	} -> `(Result, ())' resultAndTuple #}

-- | Get a list of all the IDs for items in the keyring. All items which are
-- not flagged as 'ItemApplicationSecret' are included in the list. This
-- includes items that the calling application may not (yet) have access to.
listItemIDs :: Maybe KeyringName -> Operation [ItemID]
listItemIDs name = itemIDListOperation
	(list_item_ids name)
	(list_item_ids_sync name)

{# fun list_item_ids
	{ withNullableUtf8* `Maybe String'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun list_item_ids_sync
	{ withNullableUtf8* `Maybe String'
	, alloca- `[ItemID]' stealItemIDList*
	} -> `Result' result #}
