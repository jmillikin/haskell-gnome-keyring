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
{-# LANGUAGE DeriveDataTypeable #-}
#include <gnome-keyring.h>
module Gnome.Keyring.Internal.Types
	( KeyringName
	, CancellationKey (..)
	, Error (..)
	, Result (..)
	, resultToError
	) where
import Data.Text.Lazy (Text)
import Foreign (Ptr)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

type KeyringName = Text

newtype CancellationKey = CancellationKey (Ptr ())

data Error
	= ErrorDenied
	| ErrorNoKeyringDaemon
	| ErrorAlreadyUnlocked
	| ErrorNoSuchKeyring
	| ErrorBadArguments
	| ErrorIOError
	| ErrorCancelled
	| ErrorKeyringAlreadyExists
	| ErrorNoMatch
	deriving (Show, Eq, Typeable)

instance Exception Error

{# enum GnomeKeyringResult as Result {}
	with prefix = "gnome_keyring_"
	deriving (Show) #}

resultToError :: Result -> Error
resultToError RESULT_DENIED = ErrorDenied
resultToError RESULT_NO_KEYRING_DAEMON = ErrorNoKeyringDaemon
resultToError RESULT_ALREADY_UNLOCKED = ErrorAlreadyUnlocked
resultToError RESULT_NO_SUCH_KEYRING = ErrorNoSuchKeyring
resultToError RESULT_BAD_ARGUMENTS = ErrorBadArguments
resultToError RESULT_IO_ERROR = ErrorIOError
resultToError RESULT_CANCELLED = ErrorCancelled
resultToError RESULT_KEYRING_ALREADY_EXISTS = ErrorKeyringAlreadyExists
resultToError RESULT_NO_MATCH = ErrorNoMatch
resultToError x = error $ "Not an error: " ++ show x
