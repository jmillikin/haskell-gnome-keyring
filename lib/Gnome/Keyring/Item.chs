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
	
	, ItemInfoFlag (..)
	, ItemInfo (..)
	, itemGetInfo
	, itemGetInfoFull
	, itemSetInfo
	
	, AccessControl (..)
	, AccessType (..)
	, itemGetACL
	, itemSetACL
	, itemGrantAccessRights
	
	, FoundItem (..)
	, findItems
	) where

import           Control.Exception (bracket)
import           Data.Set (Set, toList, fromList)
import           Data.Text (Text)

import           Gnome.Keyring.ItemInfo
import           Gnome.Keyring.Internal.FFI
import           Gnome.Keyring.Internal.Operation
import           Gnome.Keyring.Internal.Types

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

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

{# fun item_create_sync
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

{# fun item_delete_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about an item and its secret.
--
-- The user may be prompted if the calling application doesn't have
-- necessary access to read the item with its secret.
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

{# fun item_get_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- | Get information about an item, optionally retrieving its secret.
--
-- If the flags include 'ItemInfoSecret', then the user may be prompted if
-- the calling application doesn't have necessary access to read the item
-- with its secret.
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

{# fun item_get_info_full_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, cItemInfoFlags `Set ItemInfoFlag'
	, alloca- `ItemInfo' stealItemInfo*
	} -> `Result' result #}

-- | Set information on an item, like its display name, secret, etc.
--
-- Only the fields in the info info which are non-'Nothing' or non-zero
-- will be set on the item.
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

{# fun item_set_info_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withItemInfo* `ItemInfo'
	} -> `(Result, ())' resultAndTuple #}

{# enum GnomeKeyringAttributeType as AttributeType {} #}

data Attribute
	= TextAttribute Text Text
	| WordAttribute Text Word32
	deriving (Show, Eq)

attributeName :: Attribute -> Text
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
	, withText* `Text'
	, withText* `Text'
	} -> `()' id #}

{# fun attribute_list_append_uint32 as appendUInt32
	{ id `Ptr ()'
	, withText* `Text'
	, fromIntegral `Word32'
	} -> `()' id #}

peekAttribute :: Ptr () -> IO Attribute
peekAttribute attr = do
	name <- peekText =<< {# get GnomeKeyringAttribute->name #} attr
	cType <- {# get GnomeKeyringAttribute->type #} attr
	case toEnum . fromIntegral $ cType of
		ATTRIBUTE_TYPE_STRING -> do
			value <- peekText =<< {# get GnomeKeyringAttribute.value.string #} attr
			return $ TextAttribute name value
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

{# fun item_get_attributes_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[Attribute]' stealAttributeList*
	} -> `Result' result #}

-- | Set all the attributes for an item. These will replace any previous
-- attributes set on the item.
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

{# fun item_set_attributes_sync
	{ withNullableText* `Maybe Text'
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
	{ accessControlName :: Maybe Text
	, accessControlPath :: Maybe Text
	, accessControlType :: Set AccessType
	}
	deriving (Show, Eq)

peekAccessControl :: Ptr () -> IO AccessControl
peekAccessControl ac = do
	name <- stealNullableText =<< {# call item_ac_get_display_name #} ac
	path <- stealNullableText =<< {# call item_ac_get_path_name #} ac
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
	withNullableText (accessControlName ac) $ {# call item_ac_set_display_name #} ptr
	withNullableText (accessControlPath ac) $ {# call item_ac_set_path_name #} ptr
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

{# fun item_get_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, alloca- `[AccessControl]' stealACL*
	} -> `Result' result #}

-- | Set the full access control list on an item. This replaces any previous
-- ACL set on the item.
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

{# fun item_set_acl_sync
	{ withNullableText* `Maybe Text'
	, cItemID `ItemID'
	, withACL* `[AccessControl]'
	} -> `(Result, ())' resultAndTuple #}

-- | Will grant the application access rights to the item, provided callee
-- has write access to said item.
--
-- This is similar to performing 'itemGetACL' and 'itemSetACL' with
-- appropriate parameters.
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

{# fun item_grant_access_rights_sync
	{ withNullableText* `Maybe Text'
	, withText* `Text'
	, withText* `Text'
	, cItemID `ItemID'
	, cAccessTypes `Set AccessType'
	} -> `(Result, ())' resultAndTuple #}

data FoundItem = FoundItem
	{ foundItemKeyring    :: KeyringName
	, foundItemID         :: ItemID
	, foundItemAttributes :: [Attribute]
	, foundItemSecret     :: Text
	}
	deriving (Show, Eq)

peekFound :: Ptr () -> IO FoundItem
peekFound f = do
	keyring <- peekText =<< {# get GnomeKeyringFound->keyring #} f
	itemID <- {# get GnomeKeyringFound->item_id #} f
	attrs <- peekAttributeList =<< {# get GnomeKeyringFound->attributes #} f
	secret <- peekText =<< {# get GnomeKeyringFound->secret #} f
	let itemID' = ItemID $ fromIntegral itemID
	return $ FoundItem keyring itemID' attrs secret

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
