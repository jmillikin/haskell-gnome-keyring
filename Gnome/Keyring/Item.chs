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
{-# LANGUAGE FlexibleInstances #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Item
	( ItemInfoFlag (..)
	, ItemID (..)
	, itemCreate
	, itemDelete
	, itemGetInfo
	, itemGetInfoFull
	, itemSetInfo
	, itemGetAttributes
	, itemSetAttributes
	, itemGetACL
	, itemSetACL
	, itemGrantAccessRights
	) where

import Data.Set (Set)
import Data.Text.Lazy (Text)
import Gnome.Keyring.Operation.Internal
import Gnome.Keyring.Types
import Gnome.Keyring.AccessControl.Internal
import Gnome.Keyring.Attribute.Internal
import Gnome.Keyring.ItemInfo.Internal

import Foreign
import Foreign.C
import Gnome.Keyring.FFI

data ItemInfoFlag
	= ItemInfoBasics
	| ItemInfoSecret
	deriving (Show, Eq)

peekItemID :: (Storable a, Integral a) => Ptr a -> IO ItemID
peekItemID = fmap (ItemID . fromIntegral) . peek

cItemID :: Integral a => ItemID -> a
cItemID (ItemID x) = fromIntegral x

cItemInfoFlags :: Bits a => [ItemInfoFlag] -> a
cItemInfoFlags = foldr (.|.) 0 . map flagValue where
	flagValue ItemInfoBasics = 0
	flagValue ItemInfoSecret = 1

-- wrap a GetWordCallback
data GetItemIDCallback = GetItemIDCallback GetWordCallback
instance Callback GetItemIDCallback ItemID where
	callbackToPtr (GetItemIDCallback x) = callbackToPtr x
	freeCallback (GetItemIDCallback x) = freeCallback x
	buildCallback onSuccess = let
		onSuccess' = onSuccess . ItemID
		in fmap GetItemIDCallback . buildCallback onSuccess'

-- GnomeKeyringOperationGetItemInfoCallback
data GetItemInfoCallback = GetItemInfoCallback GetItemInfoCallbackPtr
instance Callback GetItemInfoCallback ItemInfo where
	callbackToPtr (GetItemInfoCallback x) = castFunPtr x
	freeCallback  (GetItemInfoCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetItemInfoCallback $ \cres ptr _ ->
			case result cres of
				RESULT_OK -> peekItemInfo ptr >>= onSuccess
				x -> onError $ resultToError x
		return $ GetItemInfoCallback funptr

type RawGetItemInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetItemInfoCallback as GetItemInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetItemInfoCallback :: RawGetItemInfoCallback -> IO GetItemInfoCallbackPtr

-- GnomeKeyringOperationGetAttributesCallback
data GetAttributesCallback = GetAttributesCallback GetAttributesCallbackPtr
instance Callback GetAttributesCallback [Attribute] where
	callbackToPtr (GetAttributesCallback x) = castFunPtr x
	freeCallback  (GetAttributesCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetAttributesCallback $ \cres ptr _ ->
			case result cres of
				RESULT_OK -> peekAttributeList ptr >>= onSuccess
				x -> onError $ resultToError x
		return $ GetAttributesCallback funptr

type RawGetAttributesCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetAttributesCallback as GetAttributesCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetAttributesCallback :: RawGetAttributesCallback -> IO GetAttributesCallbackPtr

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

{# fun unsafe item_create_sync
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

{# fun unsafe item_delete_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	} -> `(Result, ())' resultAndTuple #}

-- item_get_info
itemGetInfo :: Maybe Text -> ItemID -> Operation ItemInfo
itemGetInfo k item = operation
	(item_get_info k item)
	(item_get_info_sync k item)

{# fun item_get_info
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, callbackToPtr `GetItemInfoCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- item_get_info_full
itemGetInfoFull :: Maybe Text -> ItemID -> [ItemInfoFlag] -> Operation ItemInfo
itemGetInfoFull k item flags = operation
	(item_get_info_full k item flags)
	(item_get_info_full_sync k item flags)

{# fun item_get_info_full
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, cItemInfoFlags `[ItemInfoFlag]'
	, callbackToPtr `GetItemInfoCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_info_full_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, cItemInfoFlags `[ItemInfoFlag]'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- item_set_info
itemSetInfo :: Maybe Text -> ItemID -> ItemInfo -> Operation ()
itemSetInfo k item info = operation
	(item_set_info k item info)
	(item_set_info_sync k item info)

{# fun item_set_info
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	} -> `(Result, ())' resultAndTuple #}

-- item_get_attributes
itemGetAttributes :: Maybe Text -> ItemID -> Operation [Attribute]
itemGetAttributes k item = operation
	(item_get_attributes k item)
	(item_get_attributes_sync k item)

{# fun item_get_attributes
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, callbackToPtr `GetAttributesCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_attributes_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[Attribute]' stealAttributeList*
	} -> `Result' result #}

-- item_set_attributes
itemSetAttributes :: Maybe Text -> ItemID -> [Attribute] -> Operation ()
itemSetAttributes k item as = operation
	(item_set_attributes k item as)
	(item_set_attributes_sync k item as)

{# fun item_set_attributes
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_attributes_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	} -> `(Result, ())' resultAndTuple #}

-- item_get_acl
itemGetACL :: Maybe Text -> ItemID -> Operation [AccessControl]
itemGetACL k item = operation
	(item_get_acl k item)
	(item_get_acl_sync k item)

{# fun item_get_acl
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, callbackToPtr `GetACLCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[AccessControl]' stealACL*
	} -> `Result' result #}

-- item_set_acl
itemSetACL :: Maybe Text -> ItemID -> [AccessControl] -> Operation ()
itemSetACL k item acl = operation
	(item_set_acl k item acl)
	(item_set_acl_sync k item acl)

{# fun item_set_acl
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	} -> `(Result, ())' resultAndTuple #}

-- item_grant_access_rights
itemGrantAccessRights :: Maybe Text -> Text -> Text -> ItemID
                      -> Set AccessType -> Operation ()
itemGrantAccessRights k d p item r = operation
	(item_grant_access_rights k d p item r)
	(item_grant_access_rights_sync k d p item r)

{# fun item_grant_access_rights
	{ withNullableText* `Maybe Text'
	, withText* `Text'
	, withText* `Text'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	, callbackToPtr `DoneCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_grant_access_rights_sync
	{ withNullableText* `Maybe Text'
	, withText* `Text'
	, withText* `Text'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	} -> `(Result, ())' resultAndTuple #}
