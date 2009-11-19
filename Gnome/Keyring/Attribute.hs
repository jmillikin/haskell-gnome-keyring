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
-- Attributes allow various other pieces of information to be associated
-- with an item. These can also be used to search for relevant items. Use
-- 'itemGetAttributes' or 'itemSetAttributes' to manipulate attributes in
-- the keyring.
-- 
-- Each attribute is either Unicode text, or an unsigned 32-bit integer.
-- 
module Gnome.Keyring.Attribute
	( Attribute (..)
	, attributeName
	) where
import Gnome.Keyring.Attribute.Internal
import Gnome.Keyring.Item -- for docs
