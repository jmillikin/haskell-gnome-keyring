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
-- A keyring contains multiple items. Each item has a secret, attributes and
-- access information associated with it.
-- 
-- An item is identified by an 'ItemID' unique to the keyring in which it
-- exists. An item's name is for displaying to the user. Each item has a
-- single secret, which is Unicode text. This secret is stored in
-- non-pageable memory in the server, and encrypted on disk. All of this
-- information is exposed via 'ItemInfo' values.
-- 
-- Note that the underlying C library stores secrets in non-pageable memory,
-- but the Haskell bindings currently do not.
--
-- Attributes allow various other pieces of information to be associated
-- with an item. These can also be used to search for relevant items.
--
-- Each item has an access control list, which specifies which applications
-- may read, write or delete an item. The read access applies only to
-- reading the secret. All applications can read other parts of the item.
-- ACLs are accessed and changed through 'AccessControl' values.
-- 
{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Item
	( ItemID (..)
	, ItemInfoFlag (..)
	, ItemType (..)
	, ItemInfo (..)
	
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

import Data.Set (Set, toList)
import Data.Text.Lazy (Text)
import Gnome.Keyring.Operation.Internal
import Gnome.Keyring.Types
import Gnome.Keyring.AccessControl.Internal
import Gnome.Keyring.Attribute.Internal
import Gnome.Keyring.ItemInfo

import Foreign
import Foreign.C
import Gnome.Keyring.FFI

data ItemInfoFlag
	= ItemInfoBasics
	| ItemInfoSecret
	deriving (Show, Eq)

cItemID :: Integral a => ItemID -> a
cItemID (ItemID x) = fromIntegral x

cItemInfoFlags :: Bits a => Set ItemInfoFlag -> a
cItemInfoFlags = foldr (.|.) 0 . map flagValue . toList where
	flagValue ItemInfoBasics = 0
	flagValue ItemInfoSecret = 1

-- | Create a new item in a keyring.
-- 
-- The user may have been prompted to unlock necessary keyrings. If 'Nothing'
-- is specified as the keyring and no default keyring exists, the user will
-- be prompted to create a new keyring.
-- 
-- If an existing item should be updated, the user may be prompted for access
-- to the existing item.
-- 
-- Whether a new item is created or not, the ID of the item will be returned.
-- 
itemCreate :: Maybe KeyringName
           -> ItemType
           -> Text -- ^ Display name
           -> [Attribute]
           -> Text -- ^ The secret
           -> Bool -- ^ Update an existing item, if one exists.
           -> Operation ItemID
itemCreate k t dn as s u = itemIDOperation
	(item_create k t dn as s u)
	(item_create_sync k t dn as s u)

{# fun item_create
	{ withNullableText* `Maybe Text'
	, fromItemType `ItemType'
	, withText* `Text'
	, withAttributeList* `[Attribute]'
	, withText* `Text'
	, fromBool `Bool'
	, id `GetIntCallbackPtr'
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

-- | Delete an item in a keyring.
-- 
-- The user may be prompted if the calling application doesn't have
-- necessary access to delete the item.
-- 
itemDelete :: Maybe KeyringName -> ItemID -> Operation ()
itemDelete k item = voidOperation
	(item_delete k item)
	(item_delete_sync k item)

{# fun item_delete
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_delete_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about an item and its secret.
-- 
-- The user may be prompted if the calling application doesn't have
-- necessary access to read the item with its secret.
-- 
itemGetInfo :: Maybe KeyringName -> ItemID -> Operation ItemInfo
itemGetInfo k item = itemInfoOperation
	(item_get_info k item)
	(item_get_info_sync k item)

{# fun item_get_info
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, id `GetItemInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- | Get information about an item, optionally retrieving its secret.
-- 
-- If the flags include 'ItemInfoSecret', then the user may be prompted if
-- the calling application doesn't have necessary access to read the item
-- with its secret.
-- 
itemGetInfoFull :: Maybe KeyringName -> ItemID -> Set ItemInfoFlag
                -> Operation ItemInfo
itemGetInfoFull k item flags = itemInfoOperation
	(item_get_info_full k item flags)
	(item_get_info_full_sync k item flags)

{# fun item_get_info_full
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, cItemInfoFlags `Set ItemInfoFlag'
	, id `GetItemInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_info_full_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, cItemInfoFlags `Set ItemInfoFlag'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- | Set information on an item, like its display name, secret, etc.
-- 
-- Only the fields in the info info which are non-'Nothing' or non-zero
-- will be set on the item.
-- 
itemSetInfo :: Maybe KeyringName -> ItemID -> ItemInfo -> Operation ()
itemSetInfo k item info = voidOperation
	(item_set_info k item info)
	(item_set_info_sync k item info)

{# fun item_set_info
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	} -> `(Result, ())' resultAndTuple #}

-- | Get all the attributes for an item.
-- 
itemGetAttributes :: Maybe KeyringName -> ItemID -> Operation [Attribute]
itemGetAttributes k item = attributeListOperation
	(item_get_attributes k item)
	(item_get_attributes_sync k item)

{# fun item_get_attributes
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, id `GetAttributesCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_attributes_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[Attribute]' stealAttributeList*
	} -> `Result' result #}

-- | Set all the attributes for an item. These will replace any previous
-- attributes set on the item.
-- 
itemSetAttributes :: Maybe KeyringName -> ItemID -> [Attribute] -> Operation ()
itemSetAttributes k item as = voidOperation
	(item_set_attributes k item as)
	(item_set_attributes_sync k item as)

{# fun item_set_attributes
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_attributes_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	} -> `(Result, ())' resultAndTuple #}

-- | Get the access control list for an item.
-- 
itemGetACL :: Maybe KeyringName -> ItemID -> Operation [AccessControl]
itemGetACL k item = accessControlListOperation
	(item_get_acl k item)
	(item_get_acl_sync k item)

{# fun item_get_acl
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_get_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[AccessControl]' stealACL*
	} -> `Result' result #}

-- | Set the full access control list on an item. This replaces any previous
-- ACL set on the item.
-- 
itemSetACL :: Maybe KeyringName -> ItemID -> [AccessControl] -> Operation ()
itemSetACL k item acl = voidOperation
	(item_set_acl k item acl)
	(item_set_acl_sync k item acl)

{# fun item_set_acl
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe item_set_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	} -> `(Result, ())' resultAndTuple #}

-- | Will grant the application access rights to the item, provided callee
-- has write access to said item.
-- 
-- This is similar to performing 'itemGetACL' and 'itemSetACL' with
-- appropriate parameters.
-- 
itemGrantAccessRights :: Maybe KeyringName
                      -> Text -- ^ Display name
                      -> Text -- ^ Application executable path
                      -> ItemID
                      -> Set AccessType
                      -> Operation ()
itemGrantAccessRights k d p item r = voidOperation
	(item_grant_access_rights k d p item r)
	(item_grant_access_rights_sync k d p item r)

{# fun item_grant_access_rights
	{ withNullableText* `Maybe Text'
	, withText* `Text'
	, withText* `Text'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	, id `DoneCallbackPtr'
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
