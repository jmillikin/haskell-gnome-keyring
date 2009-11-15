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
{-# LANGUAGE MultiParamTypeClasses #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Item
	( ItemInfoFlag (..)
	, ItemType (..)
	, ItemID
	, itemCreate
	, itemDelete
	) where

import Data.Text.Lazy (Text)
import Gnome.Keyring.Operation.Internal (Operation, operation)
import Gnome.Keyring.Types (Result, CancellationKey (..))
import Gnome.Keyring.Attribute.Internal (Attribute, withAttributeList)

import Foreign
import Foreign.C
import Gnome.Keyring.FFI

data ItemInfoFlag
	= ItemInfoBasics
	| ItemInfoSecret
	deriving (Show, Eq)

data ItemType
	= ItemGenericSecret
	| ItemNetworkPassword
	| ItemNote
	| ItemChainedKeyringPassword
	| ItemEncryptionKeyPassword
	| ItemPublicKeyStorage
	deriving (Show, Eq)

newtype ItemID = ItemID Word32
	deriving (Show, Eq, Ord)

{# enum GnomeKeyringItemType as RawType {} deriving (Show) #}

fromItemType :: ItemType -> CInt
fromItemType = fromIntegral . fromEnum . toRaw where
	toRaw ItemGenericSecret = ITEM_GENERIC_SECRET
	toRaw ItemNetworkPassword = ITEM_NETWORK_PASSWORD
	toRaw ItemNote = ITEM_NOTE
	toRaw ItemChainedKeyringPassword = ITEM_CHAINED_KEYRING_PASSWORD
	toRaw ItemEncryptionKeyPassword = ITEM_ENCRYPTION_KEY_PASSWORD
	toRaw ItemPublicKeyStorage = ITEM_PK_STORAGE

peekItemID :: (Storable a, Integral a) => Ptr a -> IO ItemID
peekItemID = fmap (ItemID . fromIntegral) . peek

cItemID :: Integral a => ItemID -> a
cItemID (ItemID x) = fromIntegral x

-- wrap a GetWordCallback
data GetItemIDCallback = GetItemIDCallback GetWordCallback
instance Callback GetItemIDCallback ItemID where
	callbackToPtr (GetItemIDCallback x) = callbackToPtr x
	freeCallback (GetItemIDCallback x) = freeCallback x
	buildCallback onSuccess = let
		onSuccess' = onSuccess . ItemID
		in fmap GetItemIDCallback . buildCallback onSuccess'

-- item_create
itemCreate :: Maybe Text -> ItemType -> Text -> [Attribute] -> Text -> Bool
           -> Operation ItemID
itemCreate k t dn as s u = operation
	(item_create k t dn as s u)
	(item_create_sync k t dn as s u)

{# fun item_create
	{ withNullableText* `Maybe Text'
	, fromItemType `ItemType'
	, withText* `Text'
	, withAttributeList* `[Attribute]'
	, withText* `Text'
	, fromBool `Bool'
	, callbackToPtr `GetItemIDCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_create_sync
	{ withNullableText* `Maybe Text'
	, fromItemType `ItemType'
	, withText* `Text'
	, withAttributeList* `[Attribute]'
	, withText* `Text'
	, fromBool `Bool'
	, alloca- `ItemID' peekItemID*
	} -> `Result' result #}

-- item_delete
itemDelete :: Maybe Text -> ItemID -> Operation ()
itemDelete k item = operation (item_delete k item) (item_delete_sync k item)

{# fun item_delete
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_delete_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	} -> `(Result, ())' resultAndTuple #}
