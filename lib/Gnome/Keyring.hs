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
	, ItemType (..)
	, itemCreate
	, itemDelete
	, itemGetInfo
	, ItemInfoFlag (..)
	, itemGetInfoFull
	, itemSetInfo
	
	-- ** Item info
	, ItemInfo
	, itemType
	, itemSecret
	, itemDisplayName
	, itemModified
	, itemCreated
	
	-- ** Item attributes
	-- $attribute-doc
	, Attribute (..)
	, attributeName
	, itemGetAttributes
	, itemSetAttributes
	
	-- ** Access control
	-- $access-control-doc
	, AccessControl (..)
	, AccessType (..)
	, itemGetACL
	, itemSetACL
	, itemGrantAccessRights
	
	-- ** Searching for items
	, FoundItem (..)
	, findItems
	
	-- * Keyrings
	-- $keyring-doc
	, KeyringName
	
	-- ** Basic operations
	, getDefaultKeyring
	, setDefaultKeyring
	, listKeyringNames
	, create
	, delete
	, changePassword
	, listItemIDs
	
	-- ** Locking and unlocking
	, lock
	, lockAll
	, unlock
	
	-- ** Keyring information
	, KeyringInfo (..)
	, KeyringInfoToken
	, getInfo
	, setInfo
	
	-- * Network passwords
	-- $network-password-doc
	, NetworkPassword (..)
	, NetworkPasswordLocation (..)
	, findNetworkPassword
	, setNetworkPassword
	
	-- * Operations
	, Operation
	, CancellationKey
	, Error (..)
	, async
	, async'
	, sync
	, sync_
	, cancel
	) where

import           Gnome.Keyring.Item
import           Gnome.Keyring.Keyring
import           Gnome.Keyring.NetworkPassword
import           Gnome.Keyring.Operation
import           Gnome.Keyring.Misc

-- $item-doc
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

-- $attribute-doc
-- Attributes allow various other pieces of information to be associated
-- with an item. These can also be used to search for relevant items. Use
-- 'itemGetAttributes' or 'itemSetAttributes' to manipulate attributes in
-- the keyring.
--
-- Each attribute is either Unicode text, or an unsigned 32-bit integer.

-- $access-control-doc
-- Each item has an access control list, which specifies which applications
-- may read, write or delete an item. The read access applies only to reading
-- the secret. All applications can read other parts of the item. ACLs are
-- accessed and changed with 'itemGetACL' and 'itemSetACL'.

-- $keyring-doc
-- GNOME Keyring manages multiple keyrings. Each keyring can store one or
-- more items, containing secrets.
--
-- One of the keyrings is the default keyring, which can in many cases be
-- used by specifying 'Nothing' for a keyring names.
--
-- Each keyring can be in a locked or unlocked state. A password must be
-- specified, either by the user or the calling application, to unlock the
-- keyring.

-- $network-password-doc
-- Networks passwords are a simple way of saving passwords associated with
-- a certain user, server, protocol, and other fields.
