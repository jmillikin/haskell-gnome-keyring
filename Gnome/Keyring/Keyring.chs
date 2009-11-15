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

module Gnome.Keyring.Keyring where
import Data.Text.Lazy (Text)
import Gnome.Keyring.Operation (Operation, operation)
import Gnome.Keyring.Types (Result, CancellationKey (..))

-- Import unqualified for c2hs
import Foreign
import Foreign.C
import Gnome.Keyring.Bindings

-- get_default_keyring
getDefaultKeyring :: Operation Text
getDefaultKeyring = operation
	get_default_keyring
	get_default_keyring_sync

{# fun get_default_keyring
	{ callbackToPtr `GetStringCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_default_keyring_sync
	{ alloca- `Text' peekTextPtr*
	} -> `Result' result #}

-- set_default_keyring
setDefaultKeyring :: Text -> Operation ()
setDefaultKeyring name = operation
	(set_default_keyring name)
	(set_default_keyring_sync name)

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
	{ alloca- `[Text]' peekTextList*
	} -> `Result' result #}

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
	, alloca- `[Integer]' peekWordList*
	} -> `Result' result #}
