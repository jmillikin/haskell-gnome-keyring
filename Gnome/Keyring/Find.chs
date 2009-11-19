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
-- A find operation searches through all keyrings for items that match the
-- given attributes. The user may be prompted to unlock necessary keyrings,
-- and will be prompted for access to the items if needed.
-- 
{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Find
	( Found (..)
	, findItems
	) where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)
import Foreign
import Foreign.C
import Gnome.Keyring.ItemInfo.Internal
import Gnome.Keyring.Attribute.Internal
import Gnome.Keyring.Operation.Internal
import Gnome.Keyring.FFI
import Gnome.Keyring.Types

data Found = Found
	{ foundKeyring    :: KeyringName
	, foundItemID     :: ItemID
	, foundAttributes :: [Attribute]
	, foundSecret     :: Text
	}
	deriving (Show, Eq)

peekFound :: Ptr () -> IO Found
peekFound f = do
	keyring <- peekText =<< {# get GnomeKeyringFound->keyring #} f
	cItemID <- {# get GnomeKeyringFound->item_id #} f
	attrs <- peekAttributeList =<< {# get GnomeKeyringFound->attributes #} f
	secret <- peekText =<< {# get GnomeKeyringFound->secret #} f
	return $ Found keyring (ItemID (fromIntegral cItemID)) attrs secret

stealFoundList :: Ptr (Ptr ()) -> IO [Found]
stealFoundList ptr = bracket (peek ptr)
	{# call unsafe found_list_free #}
	(mapGList peekFound)

foundItemsOperation :: OperationImpl GetListCallback [Found]
foundItemsOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ (mapGList peekFound) list

-- | Searches through all keyrings for items that match the attributes. The
-- matches are for exact equality.
-- 
-- The user may be prompted to unlock necessary keyrings, and will be
-- prompted for access to the items if needed.
-- 
findItems :: ItemType -> [Attribute] -> Operation [Found]
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

{# fun unsafe find_items_sync
	{ fromItemType `ItemType'
	, withAttributeList* `[Attribute]'
	, alloca- `[Found]' stealFoundList*
	} -> `Result' result #}
