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
-- Portability : non-portable (Typeclass extensions & FFI)
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
import Gnome.Keyring.Item.Internal
import Gnome.Keyring.ItemInfo.Internal
import Gnome.Keyring.AccessControl.Internal -- for docs
