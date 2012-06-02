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
module Gnome.Keyring.Item
	(
	  ItemID (..)
	, ItemType (..)
	, itemCreate
	, itemDelete
	
	, Attribute (..)
	, attributeName
	, itemGetAttributes
	, itemSetAttributes
	
	, ItemInfo
	, itemType
	, itemSecret
	, itemDisplayName
	, itemModified
	, itemCreated
	
	, getItemInfo
	, setItemInfo
	
	, AccessControl (..)
	, AccessType (..)
	, itemGetACL
	, itemSetACL
	, itemGrantAccessRights
	
	, FoundItem
	, foundItemKeyring
	, foundItemID
	, foundItemAttributes
	, foundItemSecret
	, findItems
	) where

import           Control.Exception (bracket)
import           Data.Set (Set, toList, fromList)

import           Gnome.Keyring.ItemInfo
import           Gnome.Keyring.Internal.FFI
import           Gnome.Keyring.Internal.Operation
import           Gnome.Keyring.Internal.Types

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

cItemID :: Integral a => ItemID -> a
cItemID (ItemID x) = fromIntegral x

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
itemCreate :: Keyring
           -> ItemType
           -> String -- ^ Display name
           -> [Attribute]
           -> String -- ^ The secret
           -> Bool -- ^ Update an existing item, if one exists.
           -> Operation ItemID
itemCreate k t dn as s u = itemIDOperation
	(item_create k t dn as s u)
	(item_create_sync k t dn as s u)

{# fun item_create
	{ withKeyringName* `Keyring'
	, fromItemType `ItemType'
	, withUtf8* `String'
	, withAttributeList* `[Attribute]'
	, withUtf8* `String'
	, fromBool `Bool'
	, id `GetIntCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_create_sync
	{ withKeyringName* `Keyring'
	, fromItemType `ItemType'
	, withUtf8* `String'
	, withAttributeList* `[Attribute]'
	, withUtf8* `String'
	, fromBool `Bool'
	, alloca- `ItemID' peekItemID*
	} -> `Result' result #}

-- | Delete an item in a keyring.
--
-- The user may be prompted if the calling application doesn't have
-- necessary access to delete the item.
itemDelete :: Keyring -> ItemID -> Operation ()
itemDelete k item = voidOperation
	(item_delete k item)
	(item_delete_sync k item)

{# fun item_delete
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_delete_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about an item and its secret.
--
-- The user may be prompted if the calling application doesn't have
-- necessary access to read the item with its secret.
getItemInfo :: Keyring
            -> Bool -- ^ Whether to read the secret.
            -> ItemID
            -> Operation ItemInfo
getItemInfo k includeSecret item = itemInfoOperation
	(item_get_info_full k item includeSecret)
	(item_get_info_full_sync k item includeSecret)

{# fun item_get_info_full
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, cItemInfoFlags `Bool'
	, id `GetItemInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_get_info_full_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, cItemInfoFlags `Bool'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

cItemInfoFlags :: Integral a => Bool -> a
cItemInfoFlags includeSecret = if includeSecret then 1 else 0

-- | Set information on an item, like its display name, secret, etc.
--
-- Only the fields in the info info which are non-'Nothing' or non-zero
-- will be set on the item.
setItemInfo :: Keyring -> ItemID -> ItemInfo -> Operation ()
setItemInfo k item info = voidOperation
	(item_set_info k item info)
	(item_set_info_sync k item info)

{# fun item_set_info
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_set_info_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	} -> `(Result, ())' resultAndTuple #}

{# enum GnomeKeyringAttributeType as AttributeType {} #}

data Attribute
	= TextAttribute String String
	| WordAttribute String Word32
	deriving (Show, Eq)

attributeName :: Attribute -> String
attributeName (TextAttribute n _) = n
attributeName (WordAttribute n _) = n

withAttributeList :: [Attribute] -> (Ptr () -> IO a) -> IO a
withAttributeList attrs io = bracket newList freeAttributeList buildList where
	newList = {# call g_array_new #} 0 0 {# sizeof GnomeKeyringAttribute #}
	buildList list = mapM_ (append list) attrs >> io list
	append list (TextAttribute n x) = appendString list n x
	append list (WordAttribute n x) = appendUInt32 list n x

{# fun attribute_list_append_string as appendString
	{ id `Ptr ()'
	, withUtf8* `String'
	, withUtf8* `String'
	} -> `()' id #}

{# fun attribute_list_append_uint32 as appendUInt32
	{ id `Ptr ()'
	, withUtf8* `String'
	, fromIntegral `Word32'
	} -> `()' id #}

peekAttribute :: Ptr () -> IO Attribute
peekAttribute attr = do
	name <- peekUtf8 =<< {# get GnomeKeyringAttribute->name #} attr
	cType <- {# get GnomeKeyringAttribute->type #} attr
	case toEnum . fromIntegral $ cType of
		ATTRIBUTE_TYPE_STRING -> do
			value <- peekUtf8 =<< {# get GnomeKeyringAttribute.value.string #} attr
			return (TextAttribute name value)
		ATTRIBUTE_TYPE_UINT32 -> do
			cValue <- {# get GnomeKeyringAttribute.value.integer #} attr
			return $ WordAttribute name $ fromIntegral cValue

peekAttributeList :: Ptr () -> IO [Attribute]
peekAttributeList = mapGArray peekAttribute {# sizeof GnomeKeyringAttribute #}

stealAttributeList :: Ptr (Ptr ()) -> IO [Attribute]
stealAttributeList ptr = bracket (peek ptr) freeAttributeList peekAttributeList

freeAttributeList :: Ptr () -> IO ()
freeAttributeList = {# call attribute_list_free #}

type GetAttributesCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetAttributesCallback as GetAttributesCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetAttributesCallback :: GetAttributesCallback -> IO GetAttributesCallbackPtr

attributeListOperation :: OperationImpl GetAttributesCallback [Attribute]
attributeListOperation = operationImpl $ \checkResult ->
	wrapGetAttributesCallback $ \cres array _ ->
	checkResult cres $ peekAttributeList array

-- | Get all the attributes for an item.
itemGetAttributes :: Keyring -> ItemID -> Operation [Attribute]
itemGetAttributes k item = attributeListOperation
	(item_get_attributes k item)
	(item_get_attributes_sync k item)

{# fun item_get_attributes
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, id `GetAttributesCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_get_attributes_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, alloca- `[Attribute]' stealAttributeList*
	} -> `Result' result #}

-- | Set all the attributes for an item. These will replace any previous
-- attributes set on the item.
itemSetAttributes :: Keyring -> ItemID -> [Attribute] -> Operation ()
itemSetAttributes k item as = voidOperation
	(item_set_attributes k item as)
	(item_set_attributes_sync k item as)

{# fun item_set_attributes
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_set_attributes_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withAttributeList* `[Attribute]'
	} -> `(Result, ())' resultAndTuple #}

{# enum GnomeKeyringAccessType as RawAccessType {} deriving (Show) #}

data AccessType
	= AccessRead
	| AccessWrite
	| AccessRemove
	deriving (Show, Eq, Ord)

data AccessControl = AccessControl
	{ accessControlName :: Maybe String
	, accessControlPath :: Maybe String
	, accessControlType :: Set AccessType
	}
	deriving (Show, Eq)

peekAccessControl :: Ptr () -> IO AccessControl
peekAccessControl ac = do
	name <- stealNullableUtf8 =<< {# call item_ac_get_display_name #} ac
	path <- stealNullableUtf8 =<< {# call item_ac_get_path_name #} ac
	cType <- {# call item_ac_get_access_type #} ac
	return $ AccessControl name path $ peekAccessType cType

stealACL :: Ptr (Ptr ()) -> IO [AccessControl]
stealACL ptr = bracket (peek ptr) freeACL (mapGList peekAccessControl)

withACL :: [AccessControl] -> (Ptr () -> IO a) -> IO a
withACL acl = bracket (buildACL acl) freeACL

buildACL :: [AccessControl] -> IO (Ptr ())
buildACL acs = bracket
	{# call application_ref_new #}
	{# call application_ref_free #} $ \appRef ->
	buildACL' appRef acs nullPtr

buildACL' :: Ptr () -> [AccessControl] -> Ptr () -> IO (Ptr ())
buildACL'      _       [] list = return list
buildACL' appRef (ac:acs) list = buildAC appRef ac
	>>= {# call g_list_append #} list
	>>= buildACL' appRef acs

buildAC :: Ptr () -> AccessControl -> IO (Ptr ())
buildAC appRef ac = do
	let cAllowed = cAccessTypes $ accessControlType ac
	ptr <- {# call access_control_new #} appRef cAllowed
	withNullableUtf8 (accessControlName ac) $ {# call item_ac_set_display_name #} ptr
	withNullableUtf8 (accessControlPath ac) $ {# call item_ac_set_path_name #} ptr
	return ptr

freeACL :: Ptr () -> IO ()
freeACL = {# call acl_free #}

cAccessTypes :: Bits a => Set AccessType -> a
cAccessTypes = foldr (.|.) 0 . map (fromIntegral . fromEnum . fromAccessType) . toList where

peekAccessType :: Integral a => a -> Set AccessType
peekAccessType cint = fromList $ concat
	[ [AccessRead   | int .&. fromEnum ACCESS_READ   > 0]
	, [AccessWrite  | int .&. fromEnum ACCESS_WRITE  > 0]
	, [AccessRemove | int .&. fromEnum ACCESS_REMOVE > 0]
	]
	where int = fromIntegral cint

fromAccessType :: AccessType -> RawAccessType
fromAccessType AccessRead   = ACCESS_READ
fromAccessType AccessWrite  = ACCESS_WRITE
fromAccessType AccessRemove = ACCESS_REMOVE

accessControlListOperation :: OperationImpl GetListCallback [AccessControl]
accessControlListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ mapGList peekAccessControl list

-- | Get the access control list for an item.
itemGetACL :: Keyring -> ItemID -> Operation [AccessControl]
itemGetACL k item = accessControlListOperation
	(item_get_acl k item)
	(item_get_acl_sync k item)

{# fun item_get_acl
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_get_acl_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, alloca- `[AccessControl]' stealACL*
	} -> `Result' result #}

-- | Set the full access control list on an item. This replaces any previous
-- ACL set on the item.
itemSetACL :: Keyring -> ItemID -> [AccessControl] -> Operation ()
itemSetACL k item acl = voidOperation
	(item_set_acl k item acl)
	(item_set_acl_sync k item acl)

{# fun item_set_acl
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_set_acl_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	} -> `(Result, ())' resultAndTuple #}

-- | Will grant the application access rights to the item, provided callee
-- has write access to said item.
--
-- This is similar to performing 'itemGetACL' and 'itemSetACL' with
-- appropriate parameters.
itemGrantAccessRights :: Keyring
                      -> String -- ^ Display name
                      -> String -- ^ Application executable path
                      -> ItemID
                      -> Set AccessType
                      -> Operation ()
itemGrantAccessRights k d p item r = voidOperation
	(item_grant_access_rights k d p item r)
	(item_grant_access_rights_sync k d p item r)

{# fun item_grant_access_rights
	{ withKeyringName* `Keyring'
	, withUtf8* `String'
	, withUtf8* `String'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_grant_access_rights_sync
	{ withKeyringName* `Keyring'
	, withUtf8* `String'
	, withUtf8* `String'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	} -> `(Result, ())' resultAndTuple #}

data FoundItem = FoundItem
	{ foundItemKeyring    :: Keyring
	, foundItemID         :: ItemID
	, foundItemAttributes :: [Attribute]
	, foundItemSecret     :: String
	}
	deriving (Show, Eq)

peekFound :: Ptr () -> IO FoundItem
peekFound f = do
	keyringName <- peekUtf8 =<< {# get GnomeKeyringFound->keyring #} f
	itemID <- {# get GnomeKeyringFound->item_id #} f
	attrs <- peekAttributeList =<< {# get GnomeKeyringFound->attributes #} f
	secret <- peekUtf8 =<< {# get GnomeKeyringFound->secret #} f
	let itemID' = ItemID $ fromIntegral itemID
	return (FoundItem (keyring keyringName) itemID' attrs secret)

stealFoundList :: Ptr (Ptr ()) -> IO [FoundItem]
stealFoundList ptr = bracket (peek ptr)
	{# call found_list_free #}
	(mapGList peekFound)

foundItemsOperation :: OperationImpl GetListCallback [FoundItem]
foundItemsOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ (mapGList peekFound) list

-- | Searches through all keyrings for items that match the attributes. The
-- matches are for exact equality.
--
-- The user may be prompted to unlock necessary keyrings, and will be
-- prompted for access to the items if needed.
findItems :: ItemType -> [Attribute] -> Operation [FoundItem]
findItems t as = foundItemsOperation
	(find_items t as)
	(find_items_sync t as)

{# fun find_items
	{ fromItemType `ItemType'
	, withAttributeList* `[Attribute]'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun find_items_sync
	{ fromItemType `ItemType'
	, withAttributeList* `[Attribute]'
	, alloca- `[FoundItem]' stealFoundList*
	} -> `Result' result #}
