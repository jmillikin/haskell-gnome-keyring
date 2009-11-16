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
	{ foundKeyring    :: Text
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
	{# call unsafe gnome_keyring_found_list_free #}
	(mapGList peekFound)

data GetFoundListCallback = GetFoundListCallback GetListCallbackPtr
instance Callback GetFoundListCallback [Found] where
	callbackToPtr (GetFoundListCallback x) = castFunPtr x
	freeCallback  (GetFoundListCallback x) = freeHaskellFunPtr x
	buildCallback = mkListCallback GetFoundListCallback
		peekFound

-- find_items
findItems :: ItemType -> [Attribute] -> Operation [Found]
findItems t as = operation
	(find_items t as)
	(find_items_sync t as)

{# fun find_items
	{ fromItemType `ItemType'
	, withAttributeList* `[Attribute]'
	, callbackToPtr `GetFoundListCallback'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun unsafe find_items_sync
	{ fromItemType `ItemType'
	, withAttributeList* `[Attribute]'
	, alloca- `[Found]' stealFoundList*
	} -> `Result' result #}
