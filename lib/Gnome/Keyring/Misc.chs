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
module Gnome.Keyring.Misc
	( available
	, cancel
	) where

import           Gnome.Keyring.Internal.FFI
import           Gnome.Keyring.Internal.Types

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

-- | Check whether the client can communicate with a GNOME Keyring server.
{# fun is_available as available
	{} -> `Bool' toBool #}

unpackKey :: CancellationKey -> Ptr ()
unpackKey (CancellationKey x) = x

-- | Cancel an asynchronous request. The request will return
-- 'ErrorCancelled'.
{# fun cancel_request as cancel
	{ unpackKey `CancellationKey'
	} -> `()' id #}
