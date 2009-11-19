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
-- Each item has an access control list, which specifies which applications
-- may read, write or delete an item. The read access applies only to reading
-- the secret. All applications can read other parts of the item. ACLs are
-- accessed and changed with 'itemGetACL' and 'itemSetACL'.
-- 
module Gnome.Keyring.AccessControl
	( AccessControl (..)
	, AccessType (..)
	) where
import Gnome.Keyring.AccessControl.Internal
import Gnome.Keyring.Item -- for docs
