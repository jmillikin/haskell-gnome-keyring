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
-- Use 'getInfo' or 'setInfo' to modify information about particular
-- keyrings.
-- 
module Gnome.Keyring.KeyringInfo
	( KeyringInfo
	, keyringLockOnIdle
	, keyringLockTimeout
	, keyringMTime
	, keyringCTime
	, keyringIsLocked
	, keyringSetLockOnIdle
	, keyringSetLockTimeout
	) where
import Gnome.Keyring.Internal.KeyringInfo
import Gnome.Keyring.Keyring -- for docs
