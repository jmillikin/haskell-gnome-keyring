-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
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
-- 
-- |
-- Maintainer  : John Millikin <jmillikin@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (FFI)
-- 
-- GNOME Keyring manages multiple keyrings. Each keyring can store one or
-- more items, containing secrets.
-- 
-- One of the keyrings is the default keyring, which can in many cases be
-- used by specifying 'Nothing' for a keyring names.
-- 
-- Each keyring can be in a locked or unlocked state. A password must be
-- specified, either by the user or the calling application, to unlock the
-- keyring.

{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Keyring
	( KeyringName
	, getDefaultKeyring
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

import Control.Exception (bracket)
import Data.Text.Lazy (Text)
import Gnome.Keyring.Item (findItems) -- for docs
import Gnome.Keyring.ItemInfo
import Gnome.Keyring.Internal.FFI
import Gnome.Keyring.Internal.KeyringInfo
import Gnome.Keyring.Internal.Operation
import Gnome.Keyring.Internal.Types

-- | Get the default keyring name. If no default keyring exists, then
-- 'Nothing' will be returned.
-- 
getDefaultKeyring :: Operation (Maybe KeyringName)
getDefaultKeyring = maybeTextOperation
	get_default_keyring
	get_default_keyring_sync

{# fun get_default_keyring
	{ id `GetStringCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe get_default_keyring_sync
	{ alloca- `Maybe Text' stealNullableTextPtr*
	} -> `Result' result #}

stealNullableTextPtr :: Ptr CString -> IO (Maybe Text)
stealNullableTextPtr ptr = bracket (peek ptr) free peekNullableText

-- | Change the default keyring.
-- 
setDefaultKeyring :: KeyringName -> Operation ()
setDefaultKeyring k = voidOperation
	(set_default_keyring k)
	(set_default_keyring_sync k)

{# fun set_default_keyring
	{ withText* `Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe set_default_keyring_sync
	{ withText* `Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Get a list of keyring names. If no keyrings exist, an empty list
-- will be returned.
-- 
listKeyringNames :: Operation [KeyringName]
listKeyringNames = textListOperation
	list_keyring_names
	list_keyring_names_sync

{# fun list_keyring_names
	{ id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe list_keyring_names_sync
	{ alloca- `[Text]' stealTextList*
	} -> `Result' result #}

stealTextList :: Ptr (Ptr ()) -> IO [Text]
stealTextList ptr = bracket (peek ptr)
	{# call unsafe gnome_keyring_string_list_free #}
	(mapGList peekText)

-- | Create a new keyring with the specified name. In most cases, 'Nothing'
-- will be passed as the password, which will prompt the user to enter a
-- password of their choice.
-- 
create :: KeyringName -> Maybe Text -> Operation ()
create k p = voidOperation (c_create k p) (create_sync k p)

{# fun create as c_create
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe create_sync
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Delete a keyring. Once a keyring is deleted, there is no mechanism for
-- recovery of its contents.
-- 
delete :: KeyringName -> Operation ()
delete k = voidOperation (c_delete k) (delete_sync k)

{# fun delete as c_delete
	{ withText* `Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe delete_sync
	{ withText* `Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Lock a keyring, so that its contents may not be accessed without first
-- supplying a password.
-- 
-- Most keyring operations involving items require that the keyring first be
-- unlocked. One exception is 'findItems' and related computations.
-- 
lock :: Maybe KeyringName -> Operation ()
lock k = voidOperation (c_lock k) (lock_sync k)

{# fun lock as c_lock
	{ withNullableText* `Maybe Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe lock_sync
	{ withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Lock all the keyrings, so that their contents may not be accessed
-- without first unlocking them with a password.
-- 
lockAll :: Operation ()
lockAll = voidOperation lock_all lock_all_sync

{# fun lock_all
	{ id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe lock_all_sync
	{} -> `(Result, ())' resultAndTuple #}

-- | Unlock a keyring, so that its contents may be accessed. In most cases,
-- 'Nothing' will be specified as the password, which will prompt the user
-- to enter the correct password.
-- 
-- Most keyring operations involving items require that the keyring first be
-- unlocked. One exception is 'findItems' and related computations.
-- 
unlock :: Maybe KeyringName -> Maybe Text -> Operation ()
unlock k p = voidOperation (c_unlock k p) (unlock_sync k p)

{# fun unlock as c_unlock
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe unlock_sync
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about the keyring.
-- 
getInfo :: Maybe KeyringName -> Operation KeyringInfo
getInfo k = keyringInfoOperation (get_info k) (get_info_sync k)

{# fun get_info
	{ withNullableText* `Maybe Text'
	, id `GetKeyringInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe get_info_sync
	{ withNullableText* `Maybe Text'
	, alloca- `KeyringInfo' stealKeyringInfoPtr*
	} -> `Result' result #}

-- | Set flags and info for the keyring. The only fields in the
-- 'KeyringInfo' which are used are 'keyringLockOnIdle' and
-- 'keyringLockTimeout'.
-- 
setInfo :: Maybe KeyringName -> KeyringInfo -> Operation ()
setInfo k info = voidOperation
	(set_info k info)
	(set_info_sync k info)

{# fun set_info
	{ withNullableText* `Maybe Text'
	, withKeyringInfo* `KeyringInfo'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe set_info_sync
	{ withNullableText* `Maybe Text'
	, withKeyringInfo* `KeyringInfo'
	} -> `(Result, ())' resultAndTuple #}

-- | Change the password for a keyring. In most cases, 'Nothing' would
-- be specified for both the original and new passwords to allow the user
-- to type both.
-- 
changePassword :: KeyringName
               -> Maybe Text -- ^ Old password
               -> Maybe Text -- ^ New password
               -> Operation ()
changePassword k op np = voidOperation
	(change_password k op np)
	(change_password_sync k op np)

{# fun change_password
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe change_password_sync
	{ withText* `Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	} -> `(Result, ())' resultAndTuple #}

-- | Get a list of all the IDs for items in the keyring. All items which are
-- not flagged as 'ItemApplicationSecret' are included in the list. This
-- includes items that the calling application may not (yet) have access to.
listItemIDs :: Maybe KeyringName -> Operation [ItemID]
listItemIDs name = itemIDListOperation
	(list_item_ids name)
	(list_item_ids_sync name)

{# fun list_item_ids
	{ withNullableText* `Maybe Text'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe list_item_ids_sync
	{ withNullableText* `Maybe Text'
	, alloca- `[ItemID]' stealItemIDList*
	} -> `Result' result #}
