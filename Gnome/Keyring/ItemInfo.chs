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
{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.ItemInfo
	( ItemInfo (..)
	, ItemType (..)
	, ItemID (..)
	
	, GetItemInfoCallbackPtr
	, itemIDOperation
	, itemIDListOperation
	, stealItemIDList
	, itemInfoOperation
	, fromItemType
	, withItemInfo
	, stealItemInfo
	, peekItemID
	) where
import Data.Text.Lazy (Text)
import Control.Exception (bracket)
import Gnome.Keyring.Internal.FFI
import Gnome.Keyring.Internal.Operation

newtype ItemID = ItemID Word32
	deriving (Show, Eq, Ord)

peekItemID :: (Storable a, Integral a) => Ptr a -> IO ItemID
peekItemID = fmap (ItemID . fromIntegral) . peek

{# enum GnomeKeyringItemType as RawType {} deriving (Show) #}

data ItemType
	= ItemGenericSecret
	| ItemNetworkPassword
	| ItemNote
	| ItemChainedKeyringPassword
	| ItemEncryptionKeyPassword
	| ItemPublicKeyStorage
	deriving (Show, Eq)

fromItemType :: ItemType -> CInt
fromItemType = fromIntegral . fromEnum . toRaw where
	toRaw ItemGenericSecret = ITEM_GENERIC_SECRET
	toRaw ItemNetworkPassword = ITEM_NETWORK_PASSWORD
	toRaw ItemNote = ITEM_NOTE
	toRaw ItemChainedKeyringPassword = ITEM_CHAINED_KEYRING_PASSWORD
	toRaw ItemEncryptionKeyPassword = ITEM_ENCRYPTION_KEY_PASSWORD
	toRaw ItemPublicKeyStorage = ITEM_PK_STORAGE

toItemType :: RawType -> ItemType
toItemType ITEM_GENERIC_SECRET = ItemGenericSecret
toItemType ITEM_NETWORK_PASSWORD = ItemNetworkPassword
toItemType ITEM_NOTE = ItemNote
toItemType ITEM_CHAINED_KEYRING_PASSWORD = ItemChainedKeyringPassword
toItemType ITEM_ENCRYPTION_KEY_PASSWORD = ItemEncryptionKeyPassword
toItemType ITEM_PK_STORAGE = ItemPublicKeyStorage
toItemType x = error $ "Unknown item type: " ++ show x

-- | Note: setting mtime and ctime will not affect the keyring
data ItemInfo = ItemInfo
	{ itemType        :: ItemType
	, itemSecret      :: Maybe Text
	, itemDisplayName :: Maybe Text
	, itemMTime       :: Integer -- TODO: TimeOfDay
	, itemCTime       :: Integer -- TODO: TimeOfDay
	}
	deriving (Show, Eq)

peekItemInfo :: Ptr () -> IO ItemInfo
peekItemInfo info = do
	cType <- {# call unsafe item_info_get_type #} info
	secret <- stealNullableText =<< {# call unsafe item_info_get_secret #} info
	name <- stealNullableText =<< {# call unsafe item_info_get_display_name #} info
	mtime <- toInteger `fmap` {# call unsafe item_info_get_mtime #} info
	ctime <- toInteger `fmap` {# call unsafe item_info_get_ctime #} info
	let type' = toItemType . toEnum . fromIntegral $ cType
	return $ ItemInfo type' secret name mtime ctime

stealItemInfo :: Ptr (Ptr ()) -> IO ItemInfo
stealItemInfo ptr = bracket (peek ptr) freeItemInfo peekItemInfo

freeItemInfo :: Ptr () -> IO ()
freeItemInfo = {# call unsafe item_info_free #}

foreign import ccall "gnome-keyring.h &gnome_keyring_item_info_free"
	finalizeItemInfo :: FunPtr (Ptr a -> IO ())

withItemInfo :: ItemInfo -> (Ptr () -> IO a) -> IO a
withItemInfo info io = do
	fptr <- newForeignPtr finalizeItemInfo =<< {# call unsafe item_info_new #}
	withForeignPtr fptr $ \ptr -> do
	{# call unsafe item_info_set_type #} ptr . fromItemType . itemType $ info
	withNullableText (itemSecret info) $ {# call unsafe item_info_set_secret #} ptr
	withNullableText (itemDisplayName info) $ {# call unsafe item_info_set_display_name #} ptr
	io ptr

itemIDListOperation :: OperationImpl GetListCallback [ItemID]
itemIDListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres ptr _ ->
	checkResult cres $ peekItemIDList ptr

peekItemIDList :: Ptr () -> IO [ItemID]
peekItemIDList = mapGList $ return . ItemID . fromIntegral . ptrToWordPtr

stealItemIDList :: Ptr (Ptr ()) -> IO [ItemID]
stealItemIDList ptr = bracket (peek ptr) freeList peekItemIDList where
	freeList = {# call unsafe g_list_free #}

type GetItemInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetItemInfoCallback as GetItemInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetItemInfoCallback :: GetItemInfoCallback -> IO GetItemInfoCallbackPtr

itemIDOperation :: OperationImpl GetIntCallback ItemID
itemIDOperation = operationImpl $ \checkResult ->
	wrapGetIntCallback $ \cres cint _ ->
	checkResult cres $ return . ItemID . fromIntegral $ cint

itemInfoOperation :: OperationImpl GetItemInfoCallback ItemInfo
itemInfoOperation = operationImpl $ \checkResult ->
	wrapGetItemInfoCallback $ \cres ptr _ ->
	checkResult cres $ peekItemInfo ptr
