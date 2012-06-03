{-# LANGUAGE DeriveDataTypeable #-}
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

-- |
-- Maintainer  : John Millikin <jmillikin@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (FFI)
--
-- The GNOME Keyring is a service for securely storing per-user secret
-- information, such as passwords and encryption keys. This library is
-- a binding to the @libgnome-keyring@ C library.
--
-- Documentation for the original library is available at
-- <http://library.gnome.org/devel/gnome-keyring/stable/>
module Gnome.Keyring
	( available
	
	-- * Items
	-- $item-doc
	, ItemID
	, ItemType(..)
	, createItem
	, deleteItem
	, listItemIDs
	
	-- ** Item info
	, Item
	, getItem
	, setItem
	, itemType
	, itemSecret
	, itemDisplayName
	, itemModified
	, itemCreated
	
	-- ** Item attributes
	, Attribute (..)
	, attributeName
	, getItemAttributes
	, setItemAttributes
	
	-- ** Access control
	, Access (..)
	, AccessType (..)
	, getItemAccess
	, setItemAccess
	, grantItemAccess
	
	-- ** Searching for items
	, FoundItem
	, foundItemKeyring
	, foundItemID
	, foundItemAttributes
	, foundItemSecret
	, findItems
	
	-- * Keyrings
	, Keyring
	, defaultKeyring
	, sessionKeyring
	, keyring
	
	-- ** Basic operations
	, getDefaultKeyring
	, setDefaultKeyring
	, listKeyringNames
	, createKeyring
	, deleteKeyring
	, changeKeyringPassword
	
	-- ** Locking and unlocking keyrings
	, lockKeyring
	, unlockKeyring
	, lockAll
	
	-- ** Keyring information
	, KeyringInfo
	, keyringLockOnIdle
	, keyringLockTimeout
	, keyringModified
	, keyringCreated
	, keyringIsLocked
	, getKeyringInfo
	, setKeyringInfo
	
	-- * Network passwords
	, NetworkPassword
	, networkPasswordKeyring
	, networkPasswordSecret
	, networkPasswordItemID
	, networkPasswordNetwork
	
	, Network
	, network
	, networkProtocol
	, networkServer
	, networkObject
	, networkAuthType
	, networkPort
	, networkUser
	, networkDomain
	
	, findNetworkPassword
	, setNetworkPassword
	
	-- * Operations
	, Operation
	, CancellationKey
	, KeyringError
	, keyringErrorMessage
	, async
	, async'
	, sync
	, sync_
	, cancel
	) where

import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad (join)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Typeable (Typeable)
import           Foreign hiding (unsafePerformIO)
import           Foreign.C
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as ByteString
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

-- | Check whether the client can communicate with a GNOME Keyring server.
{# fun is_available as available
	{} -> `Bool' toBool #}

-- $item-doc
-- A keyring contains multiple items. Each item has a secret, attributes and
-- access information associated with it.
--
-- An item is identified by an 'ItemID' unique to the keyring in which it
-- exists. An item's name is for displaying to the user. Each item has a
-- single secret, which is Unicode text. This secret is stored in
-- non-pageable memory in the server, and encrypted on disk. All of this
-- information is exposed via 'Item' values.
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
-- ACLs are accessed and changed through 'Access' values.

newtype ItemID = ItemID CUInt
	deriving (Show, Eq, Ord)

data ItemType
	= ItemGenericSecret
	| ItemNetworkPassword
	| ItemNote
	| ItemChainedKeyringPassword
	| ItemEncryptionKeyPassword
	| ItemPublicKeyStorage
	| ItemApplicationSecret
	deriving (Show, Eq)

data Item = Item
	{
	
	-- | Get or set the item's type.
	  itemType :: ItemType
	
	-- | Get or set the item's secret.
	, itemSecret :: Maybe String
	
	-- | Get or set the item's display name.
	, itemDisplayName :: Maybe String
	
	, itemMTime :: UTCTime
	, itemCTime :: UTCTime
	}
	deriving (Show, Eq)

-- | Get when the item was last modified.
itemModified :: Item -> UTCTime
itemModified = itemMTime

-- | Get when the item was created.
itemCreated :: Item -> UTCTime
itemCreated = itemCTime

fromItemType :: ItemType -> CInt
fromItemType ItemGenericSecret = 0
fromItemType ItemNetworkPassword = 1
fromItemType ItemNote = 2
fromItemType ItemChainedKeyringPassword = 3
fromItemType ItemEncryptionKeyPassword = 4
fromItemType ItemPublicKeyStorage = 0x100
fromItemType ItemApplicationSecret = 0x01000000

toItemType :: CInt -> ItemType
toItemType 0 = ItemGenericSecret
toItemType 1 = ItemNetworkPassword
toItemType 2 = ItemNote
toItemType 3 = ItemChainedKeyringPassword
toItemType 4 = ItemEncryptionKeyPassword
toItemType 0x100 = ItemPublicKeyStorage
toItemType 0x01000000 = ItemApplicationSecret
toItemType _ = ItemGenericSecret

peekItemInfo :: Ptr () -> IO Item
peekItemInfo info = do
	cType <- {# call item_info_get_type #} info
	secret <- stealNullableUtf8 =<< {# call item_info_get_secret #} info
	name <- stealNullableUtf8 =<< {# call item_info_get_display_name #} info
	mtime <- cToUTC `fmap` {# call item_info_get_mtime #} info
	ctime <- cToUTC `fmap` {# call item_info_get_ctime #} info
	return (Item (toItemType cType) secret name mtime ctime)

stealItemInfo :: Ptr (Ptr ()) -> IO Item
stealItemInfo ptr = bracket (peek ptr) freeItemInfo peekItemInfo

freeItemInfo :: Ptr () -> IO ()
freeItemInfo = {# call item_info_free #}

foreign import ccall "gnome-keyring.h &gnome_keyring_item_info_free"
	finalizeItemInfo :: FunPtr (Ptr a -> IO ())

withItemInfo :: Item -> (Ptr () -> IO a) -> IO a
withItemInfo info io = do
	fptr <- newForeignPtr finalizeItemInfo =<< {# call item_info_new #}
	withForeignPtr fptr $ \ptr -> do
	{# call item_info_set_type #} ptr (fromItemType (itemType info))
	withNullableUtf8 (itemSecret info) ({# call item_info_set_secret #} ptr)
	withNullableUtf8 (itemDisplayName info) ({# call item_info_set_display_name #} ptr)
	io ptr

type GetItemInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetItemInfoCallback as GetItemInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetItemInfoCallback :: GetItemInfoCallback -> IO GetItemInfoCallbackPtr

itemIDOperation :: OperationImpl GetIntCallback ItemID
itemIDOperation = operationImpl $ \checkResult ->
	wrapGetIntCallback $ \cres cint _ ->
	checkResult cres (return (ItemID cint))

itemInfoOperation :: OperationImpl GetItemInfoCallback Item
itemInfoOperation = operationImpl $ \checkResult ->
	wrapGetItemInfoCallback $ \cres ptr _ ->
	checkResult cres (peekItemInfo ptr)

peekItemID :: Ptr CUInt -> IO ItemID
peekItemID ptr = fmap ItemID (peek ptr)

cItemID :: ItemID -> CUInt
cItemID (ItemID x) = x

-- | Create a new item in a keyring.
--
-- The user may have been prompted to unlock necessary keyrings. If
-- 'defaultKeyring' is specified as the keyring and no default keyring exists,
-- the user will be prompted to create a new keyring.
--
-- If an existing item should be updated, the user may be prompted for access
-- to the existing item.
--
-- Whether a new item is created or not, the ID of the item will be returned.
createItem :: Keyring
           -> ItemType
           -> String -- ^ Display name
           -> [Attribute]
           -> String -- ^ The secret
           -> Bool -- ^ Update an existing item, if one exists.
           -> Operation ItemID
createItem k t dn as s u = itemIDOperation
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
	} -> `Result' Result #}

-- | Delete an item in a keyring.
--
-- The user may be prompted if the calling application doesn't have
-- necessary access to delete the item.
deleteItem :: Keyring -> ItemID -> Operation ()
deleteItem k item = voidOperation
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

-- | Get a list of all the IDs for items in the keyring. All items which are
-- not flagged as 'ItemApplicationSecret' are included in the list. This
-- includes items that the calling application may not (yet) have access to.
listItemIDs :: Keyring -> Operation [ItemID]
listItemIDs name = itemIDListOperation
	(list_item_ids name)
	(list_item_ids_sync name)

{# fun list_item_ids
	{ withKeyringName* `Keyring'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun list_item_ids_sync
	{ withKeyringName* `Keyring'
	, alloca- `[ItemID]' stealItemIDList*
	} -> `Result' Result #}

itemIDListOperation :: OperationImpl GetListCallback [ItemID]
itemIDListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres ptr _ ->
	checkResult cres (peekItemIDList ptr)

peekItemIDList :: Ptr () -> IO [ItemID]
peekItemIDList = mapGList (return . ItemID . fromIntegral . ptrToWordPtr)

stealItemIDList :: Ptr (Ptr ()) -> IO [ItemID]
stealItemIDList ptr = bracket (peek ptr) freeList peekItemIDList where
	freeList = {# call g_list_free #}

-- | Get information about an item and its secret.
--
-- The user may be prompted if the calling application doesn't have
-- necessary access to read the item with its secret.
getItem :: Keyring
        -> Bool -- ^ Whether to read the secret.
        -> ItemID
        -> Operation Item
getItem k includeSecret item = itemInfoOperation
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
	, alloca- `Item' stealItemInfo*
	} -> `Result' Result #}

cItemInfoFlags :: Integral a => Bool -> a
cItemInfoFlags includeSecret = if includeSecret then 1 else 0

-- | Set information on an item, like its display name, secret, etc.
--
-- Only the fields in the Item which are not 'Nothing' or non-zero
-- will be set on the item.
setItem :: Keyring -> ItemID -> Item -> Operation ()
setItem k item info = voidOperation
	(item_set_info k item info)
	(item_set_info_sync k item info)

{# fun item_set_info
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withItemInfo* `Item'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_set_info_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withItemInfo* `Item'
	} -> `(Result, ())' resultAndTuple #}

{# enum GnomeKeyringAttributeType as AttributeType {} #}

-- | Attributes allow various other pieces of information to be associated
-- with an item. These can also be used to search for relevant items. Use
-- 'getItemAttributes' or 'setItemAttributes' to manipulate attributes in
-- the keyring.
--
-- Each attribute is either Unicode text, or an unsigned 32-bit integer.
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

appendUInt32 :: Ptr () -> String -> Word32 -> IO ()
appendUInt32 list name val = withUtf8 name (\name_ptr -> c_append_uint32 list name_ptr val)

foreign import ccall unsafe "gnome_keyring_attribute_list_append_uint32"
	c_append_uint32 :: Ptr () -> CString -> Word32 -> IO ()

peekAttribute :: Ptr () -> IO Attribute
peekAttribute attr = do
	name <- peekUtf8 =<< {# get GnomeKeyringAttribute->name #} attr
	cType <- {# get GnomeKeyringAttribute->type #} attr
	case cType of
		0 -> do
			value <- peekUtf8 =<< {# get GnomeKeyringAttribute.value.string #} attr
			return (TextAttribute name value)
		1 -> do
			cValue <- {# get GnomeKeyringAttribute.value.integer #} attr
			return (WordAttribute name (fromIntegral cValue))
		_ -> undefined

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
	checkResult cres (peekAttributeList array)

-- | Get all the attributes for an item.
getItemAttributes :: Keyring -> ItemID -> Operation [Attribute]
getItemAttributes k item = attributeListOperation
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
	} -> `Result' Result #}

-- | Set all the attributes for an item. These will replace any existing
-- attributes.
setItemAttributes :: Keyring -> ItemID -> [Attribute] -> Operation ()
setItemAttributes k item as = voidOperation
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

data AccessType
	= AccessRead
	| AccessWrite
	| AccessRemove
	deriving (Show, Eq, Ord)

-- | Each item has an access control list, which specifies which applications
-- may read, write or delete an item. The read access applies only to reading
-- the secret. All applications can read other parts of the item. ACLs are
-- accessed and changed with 'getItemAccess' and 'setItemAccess'.
data Access = Access
	{ accessName :: Maybe String
	, accessPath :: Maybe String
	, accessType :: [AccessType]
	}
	deriving (Show, Eq)

peekAccessControl :: Ptr () -> IO Access
peekAccessControl ac = do
	name <- stealNullableUtf8 =<< {# call item_ac_get_display_name #} ac
	path <- stealNullableUtf8 =<< {# call item_ac_get_path_name #} ac
	cType <- {# call item_ac_get_access_type #} ac
	return (Access name path (peekAccessType cType))

stealACL :: Ptr (Ptr ()) -> IO [Access]
stealACL ptr = bracket (peek ptr) freeACL (mapGList peekAccessControl)

withACL :: [Access] -> (Ptr () -> IO a) -> IO a
withACL acl = bracket (buildACL acl) freeACL

buildACL :: [Access] -> IO (Ptr ())
buildACL acs = bracket
	{# call application_ref_new #}
	{# call application_ref_free #} $ \appRef ->
	buildACL' appRef acs nullPtr

buildACL' :: Ptr () -> [Access] -> Ptr () -> IO (Ptr ())
buildACL'      _       [] list = return list
buildACL' appRef (ac:acs) list = buildAC appRef ac
	>>= {# call g_list_append #} list
	>>= buildACL' appRef acs

buildAC :: Ptr () -> Access-> IO (Ptr ())
buildAC appRef ac = do
	let cAllowed = cAccessTypes (accessType ac)
	ptr <- {# call access_control_new #} appRef cAllowed
	withNullableUtf8 (accessName ac) ({# call item_ac_set_display_name #} ptr)
	withNullableUtf8 (accessPath ac) ({# call item_ac_set_path_name #} ptr)
	return ptr

freeACL :: Ptr () -> IO ()
freeACL = {# call acl_free #}

cAccessTypes :: [AccessType] -> CInt
cAccessTypes = foldr (.|.) 0 . map fromAccessType where
	fromAccessType :: AccessType -> CInt
	fromAccessType AccessRead   = 1
	fromAccessType AccessWrite  = 2
	fromAccessType AccessRemove = 4

peekAccessType :: CInt -> [AccessType]
peekAccessType cint = concat
	[ [AccessRead   | (cint .&. 1) > 0]
	, [AccessWrite  | (cint .&. 2) > 0]
	, [AccessRemove | (cint .&. 4) > 0]
	]

accessControlListOperation :: OperationImpl GetListCallback [Access]
accessControlListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres (mapGList peekAccessControl list)

-- | Get the access control list for an item.
getItemAccess :: Keyring -> ItemID -> Operation [Access]
getItemAccess k item = accessControlListOperation
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
	, alloca- `[Access]' stealACL*
	} -> `Result' Result #}

-- | Set the full access control list on an item. This replaces any previous
-- ACL set on the item.
setItemAccess :: Keyring -> ItemID -> [Access] -> Operation ()
setItemAccess k item acl = voidOperation
	(item_set_acl k item acl)
	(item_set_acl_sync k item acl)

{# fun item_set_acl
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withACL* `[Access]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_set_acl_sync
	{ withKeyringName* `Keyring'
	, cItemID `ItemID'
	, withACL* `[Access]'
	} -> `(Result, ())' resultAndTuple #}

-- | Will grant the application access rights to the item, provided callee
-- has write access to said item.
--
-- This is similar to performing 'getItemAccess' and 'setItemAccess' with
-- appropriate parameters.
grantItemAccess :: Keyring
                -> String -- ^ Display name
                -> String -- ^ Application executable path
                -> ItemID
                -> [AccessType]
                -> Operation ()
grantItemAccess k d p item r = voidOperation
	(item_grant_access_rights k d p item r)
	(item_grant_access_rights_sync k d p item r)

{# fun item_grant_access_rights
	{ withKeyringName* `Keyring'
	, withUtf8* `String'
	, withUtf8* `String'
	, cItemID `ItemID'
	, cAccessTypes `[AccessType]'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun item_grant_access_rights_sync
	{ withKeyringName* `Keyring'
	, withUtf8* `String'
	, withUtf8* `String'
	, cItemID `ItemID'
	, cAccessTypes `[AccessType]'
	} -> `(Result, ())' resultAndTuple #}

data FoundItem = FoundItem
	{ foundItemKeyring_ :: Keyring
	, foundItemID_ :: ItemID
	, foundItemAttributes_ :: [Attribute]
	, foundItemSecret_ :: String
	}
	deriving (Eq)

instance Show FoundItem where
	showsPrec d x = showParen (d > 10) $
		s "FoundItem " .
		s " {foundItemKeyring = " . shows (foundItemKeyring_ x) .
		s ", foundItemID = " . shows (foundItemID_ x) .
		s ", foundItemAttributes_= " . shows (foundItemAttributes_ x) .
		s ", foundItemSecret = " . shows (foundItemSecret_ x) .
		s "}"
		where s = showString

-- | Get which keyring the item was found in.
foundItemKeyring :: FoundItem -> Keyring
foundItemKeyring = foundItemKeyring_

-- | Get the found item's ID.
foundItemID :: FoundItem -> ItemID
foundItemID = foundItemID_

-- | Get the found item's attributes.
foundItemAttributes :: FoundItem -> [Attribute]
foundItemAttributes = foundItemAttributes_

-- | Get the found item's secret.
foundItemSecret :: FoundItem -> String
foundItemSecret = foundItemSecret_

peekFound :: Ptr () -> IO FoundItem
peekFound f = do
	keyringName <- peekUtf8 =<< {# get GnomeKeyringFound->keyring #} f
	itemID <- {# get GnomeKeyringFound->item_id #} f
	attrs <- peekAttributeList =<< {# get GnomeKeyringFound->attributes #} f
	secret <- peekUtf8 =<< {# get GnomeKeyringFound->secret #} f
	return (FoundItem (keyring keyringName) (ItemID itemID) attrs secret)

stealFoundList :: Ptr (Ptr ()) -> IO [FoundItem]
stealFoundList ptr = bracket (peek ptr)
	{# call found_list_free #}
	(mapGList peekFound)

foundItemsOperation :: OperationImpl GetListCallback [FoundItem]
foundItemsOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ -> if cres == 9
		then checkResult 0 (return [])
		else checkResult cres ((mapGList peekFound) list)

-- | Searches through all keyrings for items that match the attributes. The
-- matches are for exact equality.
--
-- The user may be prompted to unlock necessary keyrings, and will be
-- prompted for access to the items if needed.
--
-- Returns an empty list if no items were found.
findItems :: ItemType -> [Attribute] -> Operation [FoundItem]
findItems t as = foundItemsOperation
	(find_items t as)
	(do
		(rc, lst) <- find_items_sync t as
		return $ if rc == Result 9
			then (Result 0, [])
			else (rc, lst))

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
	} -> `Result' Result #}

-- | GNOME Keyring manages multiple keyrings. Each keyring can store one or
-- more items, containing secrets.
--
-- Each keyring can be in a locked or unlocked state. A password must be
-- specified, either by the user or the calling application, to unlock the
-- keyring.
data Keyring
	= DefaultKeyring
	| NamedKeyring String
	deriving (Eq, Show)

defaultKeyring :: Keyring
defaultKeyring = DefaultKeyring

sessionKeyring :: Keyring
sessionKeyring = keyring "session"

keyring :: String -> Keyring
keyring = NamedKeyring

-- | Get the name of the default keyring. If no keyring is the default,
-- returns @Nothing@.
getDefaultKeyring :: Operation (Maybe String)
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
	} -> `Result' Result #}

stealNullableUtf8Ptr :: Ptr CString -> IO (Maybe String)
stealNullableUtf8Ptr ptr = bracket (peek ptr) free peekNullableUtf8

-- | Change the default keyring.
setDefaultKeyring :: String -> Operation ()
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
listKeyringNames :: Operation [String]
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
	} -> `Result' Result #}

stealUtf8List :: Ptr (Ptr ()) -> IO [String]
stealUtf8List ptr = bracket (peek ptr)
	{# call gnome_keyring_string_list_free #}
	(mapGList peekUtf8)

-- | Create a new keyring with the specified name. In most cases, @Nothing@
-- will be passed as the password, which will prompt the user to enter a
-- password of their choice.
createKeyring :: String -- ^ Keyring name
             -> Maybe String -- ^ Keyring password, or @Nothing@ to prompt
                             -- the user.
             -> Operation ()
createKeyring k p = voidOperation (c_create k p) (create_sync k p)

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
deleteKeyring :: String -> Operation ()
deleteKeyring k = voidOperation (c_delete k) (delete_sync k)

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
lockKeyring :: Keyring -> Operation ()
lockKeyring k = voidOperation (c_lock k) (lock_sync k)

{# fun lock as c_lock
	{ withKeyringName* `Keyring'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun lock_sync
	{ withKeyringName* `Keyring'
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
unlockKeyring :: Keyring -> Maybe String -> Operation ()
unlockKeyring k p = voidOperation (c_unlock k p) (unlock_sync k p)

{# fun unlock as c_unlock
	{ withKeyringName* `Keyring '
	, withNullableUtf8* `Maybe String'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unlock_sync
	{ withKeyringName* `Keyring '
	, withNullableUtf8* `Maybe String'
	} -> `(Result, ())' resultAndTuple #}

-- | Get information about the keyring.
getKeyringInfo :: Keyring -> Operation KeyringInfo
getKeyringInfo k = keyringInfoOperation (get_info k) (get_info_sync k)

{# fun get_info
	{ withKeyringName* `Keyring'
	, id `GetKeyringInfoCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun get_info_sync
	{ withKeyringName* `Keyring'
	, alloca- `KeyringInfo' stealKeyringInfoPtr*
	} -> `Result' Result #}

-- | Set flags and info for the keyring. The only fields in the
-- 'KeyringInfo' which may be modified are 'keyringLockOnIdle' and
-- 'keyringLockTimeout'.
setKeyringInfo :: Keyring -> KeyringInfo -> Operation ()
setKeyringInfo k info = voidOperation
	(set_info k info)
	(set_info_sync k info)

{# fun set_info
	{ withKeyringName* `Keyring'
	, withKeyringInfo* `KeyringInfo'
	, id `DoneCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_info_sync
	{ withKeyringName* `Keyring'
	, withKeyringInfo* `KeyringInfo'
	} -> `(Result, ())' resultAndTuple #}

-- | Change the password for a keyring. In most cases, @Nothing@ would
-- be specified for both the original and new passwords to allow the user
-- to type both.
changeKeyringPassword
	:: String -- ^ Keyring name
	-> Maybe String -- ^ Old password, or @Nothing@ to prompt the user.
	-> Maybe String -- ^ New password, or @Nothing@ to prompt the user.
	-> Operation ()
changeKeyringPassword k op np = voidOperation
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

data KeyringInfo = KeyringInfo
	{
	
	-- | Get or set whether the keyring should be locked when idle.
	  keyringLockOnIdle :: Bool
	
	-- | Get or set the keyring lock timeout.
	, keyringLockTimeout :: Word32
	, keyringMTime :: UTCTime
	, keyringCTime :: UTCTime
	, keyringIsLocked_ :: Bool
	, keyringInfoToken :: ForeignPtr ()
	}

-- | Get when the keyring was last modified.
keyringModified :: KeyringInfo -> UTCTime
keyringModified = keyringMTime

-- | Get when the keyring was created.
keyringCreated :: KeyringInfo -> UTCTime
keyringCreated = keyringCTime

-- | Get whether the keyring is locked.
keyringIsLocked :: KeyringInfo -> Bool
keyringIsLocked = keyringIsLocked_

-- The extra pointer shouldn't be printed out when showing a KeyringInfo,
-- so deriving(Show) can't be used. This instance acts like the
-- auto-generated instance, minus the pointer.
instance Show KeyringInfo where
	showsPrec d info = showParen (d > 10) $
		s "KeyringInfo" .
		s " {keyringLockOnIdle = " . shows (keyringLockOnIdle info) .
		s ", keyringLockTimeout = " . shows (keyringLockTimeout info) .
		s ", keyringMTime = " . shows (keyringMTime info) .
		s ", keyringCTime = " . shows (keyringCTime info) .
		s ", keyringIsLocked = " . shows (keyringIsLocked info) .
		s "}"
		where s = showString

-- GnomeKeyringOperationGetKeyringInfoCallback
type GetKeyringInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetKeyringInfoCallback
	as GetKeyringInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetKeyringInfoCallback :: GetKeyringInfoCallback
	                           -> IO GetKeyringInfoCallbackPtr

keyringInfoOperation :: OperationImpl GetKeyringInfoCallback KeyringInfo
keyringInfoOperation = operationImpl $ \checkResult ->
	wrapGetKeyringInfoCallback $ \cres ptr _ ->
	checkResult cres (peekKeyringInfo ptr)

copyInfo :: Ptr () -> IO (ForeignPtr ())
copyInfo = (newForeignPtr finalizeKeyringInfo =<<) . {# call info_copy as c_copy #}

peekKeyringInfo :: Ptr () -> IO KeyringInfo
peekKeyringInfo ptr = do
	lockOnIdle <- toBool `fmap` {# call info_get_lock_on_idle #} ptr
	timeout <- fromIntegral `fmap` {# call info_get_lock_timeout #} ptr
	mtime <- cToUTC `fmap` {# call info_get_mtime #} ptr
	ctime <- cToUTC `fmap` {# call info_get_ctime #} ptr
	isLocked <- toBool `fmap` {# call info_get_is_locked #} ptr
	copy <- copyInfo ptr
	return (KeyringInfo lockOnIdle timeout mtime ctime isLocked copy)

stealKeyringInfoPtr :: Ptr (Ptr ()) -> IO KeyringInfo
stealKeyringInfoPtr ptr = do
	infoPtr <- newForeignPtr finalizeKeyringInfo =<< peek ptr
	withForeignPtr infoPtr peekKeyringInfo

withKeyringInfo :: KeyringInfo -> (Ptr () -> IO a) -> IO a
withKeyringInfo info io = do
	let infoPtr = keyringInfoToken info
	copy <- withForeignPtr infoPtr copyInfo
	withForeignPtr copy $ \ptr -> do
	{# call info_set_lock_on_idle #} ptr (fromBool (keyringLockOnIdle info))
	{# call info_set_lock_timeout #} ptr (fromIntegral (keyringLockTimeout info))
	io ptr

foreign import ccall "gnome-keyring.h &gnome_keyring_info_free"
	finalizeKeyringInfo :: FunPtr (Ptr a -> IO ())

-- | Networks passwords are a simple way of saving passwords associated with
-- a certain user, server, protocol, and other fields.
data NetworkPassword = NetworkPassword
	{ networkPasswordKeyring_ :: Keyring
	, networkPasswordItemID_ :: ItemID
	, networkPasswordNetwork_ :: Network
	, networkPasswordSecret_ :: String
	}
	deriving (Eq)

-- | Get which keyring the password is stored in.
networkPasswordKeyring :: NetworkPassword -> Keyring
networkPasswordKeyring = networkPasswordKeyring_

-- | Get the ID of the network password's keyring item.
networkPasswordItemID :: NetworkPassword -> ItemID
networkPasswordItemID = networkPasswordItemID_

-- | Get the network location metadata associated with the network password.
networkPasswordNetwork :: NetworkPassword -> Network
networkPasswordNetwork = networkPasswordNetwork_

-- | Get the network password's secret value.
networkPasswordSecret :: NetworkPassword -> String
networkPasswordSecret = networkPasswordSecret_

instance Show NetworkPassword where
	showsPrec d x = showParen (d > 10) $ 
		s "NetworkPassword" .
		s " {networkPasswordKeyring = " . shows (networkPasswordKeyring_ x) .
		s ", networkPasswordItemID = " . shows (networkPasswordItemID_ x) .
		s ", networkPasswordNetwork = " . shows (networkPasswordNetwork_ x) .
		s ", networkPasswordSecret = " . shows (networkPasswordSecret_ x) .
		s "}"
		where s = showString

-- | A set of predicates to store with a 'NetworkPassword', used to find the
-- password later.
data Network = Network
	{
	-- | Get or set the network protocol.
	  networkProtocol :: Maybe String
	
	-- | Get or set the network server name.
	, networkServer :: Maybe String
	
	-- | Get or set the network object.
	, networkObject :: Maybe String
	
	-- | Get or set the type of authentication.
	, networkAuthType :: Maybe String
	
	-- | Get or set the network port. A port of 0 is considered blank.
	, networkPort :: Word32
	
	-- | Get or set the network user name.
	, networkUser :: Maybe String
	
	-- | Get or set the network domain name.
	, networkDomain :: Maybe String
	}
	deriving (Show, Eq)

-- | A 'Network' with no set fields.
network :: Network
network = Network
	{ networkProtocol = Nothing
	, networkServer = Nothing
	, networkObject = Nothing
	, networkAuthType = Nothing
	, networkPort = 0
	, networkUser = Nothing
	, networkDomain = Nothing
	}

-- | Find a previously stored 'NetworkPassword'. Searches all keyrings.
--
-- The user may be prompted to unlock necessary keyrings, and will be
-- prompted for access to the items if needed.
--
-- Network passwords are items with the 'ItemType' 'ItemNetworkPassword'.
--
-- Returns an empty list if no items were found.
findNetworkPassword :: Network -> Operation [NetworkPassword]
findNetworkPassword net = let
	p1 = networkUser     net
	p2 = networkDomain   net
	p3 = networkServer   net
	p4 = networkObject   net
	p5 = networkProtocol net
	p6 = networkAuthType net
	p7 = networkPort     net
	in passwordListOperation
		(find_network_password p1 p2 p3 p4 p5 p6 p7)
		(do
			(rc, lst) <- find_network_password_sync p1 p2 p3 p4 p5 p6 p7
			return $ if rc == Result 9
				then (Result 0, [])
				else (rc, lst))

passwordListOperation :: OperationImpl GetListCallback [NetworkPassword]
passwordListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ -> if cres == 9
		then checkResult 0 (return [])
		else checkResult cres (mapGList peekPassword list)

{# fun find_network_password
	{ withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, fromIntegral `Word32'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun find_network_password_sync
	{ withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, fromIntegral `Word32'
	, alloca- `[NetworkPassword]' stealPasswordList*
	} -> `Result' Result #}

-- | Store a network password.
--
-- If an item already exists for with this network info (ie: user, server,
-- etc.) then it will be updated.
--
-- Whether a new item is created or not, the item's ID will be returned.
--
-- Network passwords are items with the 'ItemType' 'ItemNetworkPassword'.
setNetworkPassword :: Keyring
                   -> Network
                   -> String
                   -> Operation ItemID
setNetworkPassword k net secret = let
	p1 = networkUser     net
	p2 = networkDomain   net
	p3 = networkServer   net
	p4 = networkObject   net
	p5 = networkProtocol net
	p6 = networkAuthType net
	p7 = networkPort     net
	in itemIDOperation
		(set_network_password k p1 p2 p3 p4 p5 p6 p7 secret)
		(set_network_password_sync k p1 p2 p3 p4 p5 p6 p7 secret)

{# fun set_network_password
	{ withKeyringName* `Keyring'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, fromIntegral `Word32'
	, withUtf8* `String'
	, id `GetIntCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_network_password_sync
	{ withKeyringName* `Keyring'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, withNullableUtf8* `Maybe String'
	, fromIntegral `Word32'
	, withUtf8* `String'
	, alloca- `ItemID' peekItemID*
	} -> `Result' Result #}

peekPassword :: Ptr () -> IO NetworkPassword
peekPassword pwd = do
	-- Password location
	protocol <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->protocol #} pwd
	server <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->server #} pwd
	object <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->object #} pwd
	authType <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->authtype #} pwd
	port <- fromIntegral `fmap` {# get GnomeKeyringNetworkPasswordData->port #} pwd
	user <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->user #} pwd
	domain <- peekNullableUtf8 =<< {# get GnomeKeyringNetworkPasswordData->domain #} pwd
	let net = Network
		{ networkProtocol = protocol
		, networkServer   = server
		, networkObject   = object
		, networkAuthType = authType
		, networkPort     = port
		, networkUser     = user
		, networkDomain   = domain
		}
	
	-- Keyring, item, and secret
	keyringName <- peekUtf8 =<< {# get GnomeKeyringNetworkPasswordData->keyring #} pwd
	itemID <- ItemID `fmap` {# get GnomeKeyringNetworkPasswordData->item_id #} pwd
	password <- peekUtf8 =<< {# get GnomeKeyringNetworkPasswordData->password #} pwd
	return (NetworkPassword (keyring keyringName) itemID net password)

stealPasswordList :: Ptr (Ptr ()) -> IO [NetworkPassword]
stealPasswordList ptr = bracket (peek ptr)
	{# call network_password_list_free #}
	(mapGList peekPassword)

data Operation a = Operation
	{ async    :: (KeyringError -> IO ()) -> (a -> IO ()) -> IO CancellationKey
	, syncImpl :: IO (Result, a)
	}

-- Synchronous operation public API
sync :: Operation a -> IO (Either KeyringError a)
sync op = do
	(res, x) <- syncImpl op
	return $ case res of
		Result 0 -> Right x
		_        -> Left (resultToError res)

sync_ :: Operation a -> IO a
sync_ op = do
	res <- sync op
	case res of
		Right x -> return x
		Left err -> throwIO (KeyringException err)

-- Helper for async operations which return nothing useful
async' :: Operation a -> (KeyringError -> IO ()) -> IO ()  -> IO CancellationKey
async' op onError onSuccess = async op onError (const onSuccess)

-- Implementation details of async operations
type OperationImpl a b = (FunPtr a -> Ptr () -> DestroyNotifyPtr -> IO CancellationKey) -> IO (Result, b) -> Operation b
operationImpl :: ((CInt -> IO a -> IO ()) -> IO (FunPtr b)) -> OperationImpl b a
operationImpl impl asyncIO = Operation $ \onError onSuccess -> do
	
	callback <- impl $ \cres io -> case cres of
		0 -> io >>= onSuccess
		x        -> onError (resultToError (Result x))
	
	destroy <- wrapDestroyNotify $ \ptr -> do
		let stable = castPtrToStablePtr ptr
		_ <- join (deRefStablePtr stable)
		freeStablePtr stable
	
	stable <- newStablePtr $ do
		freeHaskellFunPtr callback
		freeHaskellFunPtr destroy
	
	asyncIO callback (castStablePtrToPtr stable) destroy

-- Available basic operation types

voidOperation :: OperationImpl DoneCallback ()
voidOperation = operationImpl $ \checkResult ->
	wrapDoneCallback $ \cres _ ->
	checkResult cres (return ())

maybeStringOperation :: OperationImpl GetStringCallback (Maybe String)
maybeStringOperation = operationImpl $ \checkResult ->
	wrapGetStringCallback $ \cres cstr _ ->
	checkResult cres (peekNullableUtf8 cstr)

stringListOperation :: OperationImpl GetListCallback [String]
stringListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres (mapGList peekUtf8 list)

cToUTC :: Integral a => a -> UTCTime
cToUTC = posixSecondsToUTCTime . fromIntegral

peekText :: CString -> IO Text
peekText cstr
	| cstr == nullPtr = error "Gnome.Keyring.FFI.peekText nullPtr"
	| otherwise       = do
		bytes <- ByteString.packCString cstr
		return (decodeUtf8 bytes)

withUtf8 :: String -> (CString -> IO a) -> IO a
withUtf8 = ByteString.useAsCString . encodeUtf8  . Data.Text.pack

peekUtf8 :: CString -> IO String
peekUtf8 cstr = fmap Data.Text.unpack (peekText cstr)

withNullableUtf8 :: Maybe String -> (CString -> IO a) -> IO a
withNullableUtf8 = maybeWith withUtf8

peekNullableUtf8 :: CString -> IO (Maybe String)
peekNullableUtf8= maybePeek peekUtf8

stealNullableUtf8 :: CString -> IO (Maybe String)
stealNullableUtf8 cstr = bracket (return cstr) free peekNullableUtf8

withKeyringName :: Keyring -> (CString -> IO a) -> IO a
withKeyringName k = withNullableUtf8 name where
	name = case k of
		DefaultKeyring -> Nothing
		NamedKeyring s -> Just s

-- Convert GList to []
mapGList :: (Ptr a -> IO b) -> Ptr () -> IO [b]
mapGList f list
	| list == nullPtr = return []
	| otherwise = do
		item <- {# get GList->data #} list
		next <- {# get GList->next #} list
		items <- mapGList f next
		item' <- f (castPtr item)
		return (item' : items)

-- Convert GArray to []
mapGArray :: (Ptr a -> IO b) -> Int -> Ptr () -> IO [b]
mapGArray f size array = do
	len <- {# get GArray->len #} array
	start <- {# get GArray->data #} array
	mapGArray' f size (fromIntegral len) (castPtr start)

mapGArray' :: (Ptr a -> IO b) -> Int -> Integer -> Ptr () -> IO [b]
mapGArray' _    _ 0   _ = return []
mapGArray' f size n ptr = do
	attr <- f (castPtr ptr)
	attrs <- mapGArray' f size (n - 1) (plusPtr ptr size)
	return (attr : attrs)

resultToError :: Result -> KeyringError
resultToError (Result 7) = KeyringError "Canceled"
resultToError (Result x) = unsafePerformIO $ do
	ptr <- {# call gnome_keyring_result_to_message #} x
	msg <- peekUtf8 ptr
	return (KeyringError msg)

--------------

-- GDestroyNotify
type DestroyNotify = Ptr () -> IO ()
{# pointer GDestroyNotify as DestroyNotifyPtr #}
foreign import ccall "wrapper"
	wrapDestroyNotify :: DestroyNotify -> IO DestroyNotifyPtr

-- GnomeKeyringOperationDoneCallback
type DoneCallback = CInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationDoneCallback as DoneCallbackPtr #}
foreign import ccall "wrapper"
	wrapDoneCallback :: DoneCallback -> IO DoneCallbackPtr

-- GnomeKeyringOperationGetStringCallback
type GetStringCallback = CInt -> CString -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetStringCallback as GetStringCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetStringCallback :: GetStringCallback -> IO GetStringCallbackPtr

-- GnomeKeyringOperationGetIntCallback
type GetIntCallback = CInt -> CUInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetIntCallback as GetIntCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetIntCallback :: GetIntCallback -> IO GetIntCallbackPtr

-- GnomeKeyringOperationGetListCallback
type GetListCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetListCallback as GetListCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetListCallback :: GetListCallback -> IO GetListCallbackPtr

unpackKey :: CancellationKey -> Ptr ()
unpackKey (CancellationKey x) = x

-- | Cancel an asynchronous request. The request will return
-- 'ErrorCancelled'.
{# fun cancel_request as cancel
	{ unpackKey `CancellationKey'
	} -> `()' id #}

newtype CancellationKey = CancellationKey (Ptr ())

newtype KeyringError = KeyringError String
	deriving (Eq, Show)

keyringErrorMessage :: KeyringError -> String
keyringErrorMessage (KeyringError msg) = msg

newtype KeyringException = KeyringException KeyringError
	deriving (Show, Eq, Typeable)

instance Exception KeyringException

newtype Result = Result CInt
	deriving (Eq)

resultAndTuple :: CInt -> (Result, ())
resultAndTuple x = (Result x, ())
