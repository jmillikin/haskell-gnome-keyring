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
	, module Gnome.Keyring.Item
	, module Gnome.Keyring.Keyring
	, module Gnome.Keyring.NetworkPassword
	, module Gnome.Keyring.Operation
	) where

import           Gnome.Keyring.Item
import           Gnome.Keyring.Keyring
import           Gnome.Keyring.NetworkPassword
import           Gnome.Keyring.Operation
import           Gnome.Keyring.Misc
