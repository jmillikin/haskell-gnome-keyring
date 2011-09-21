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
module Gnome.Keyring.KeyringInfo
	( KeyringInfo (..)
	, KeyringInfoToken
	
	, GetKeyringInfoCallbackPtr
	, keyringInfoOperation
	, withKeyringInfo
	, stealKeyringInfoPtr
	) where

import           Data.Time (UTCTime)

import           Gnome.Keyring.Internal.FFI
import           Gnome.Keyring.Internal.Operation

#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_info_" #}

-- Our keyring info populates/is populated by the native info structure.
-- Clients can't create them directly, because GKR doesn't allow it.
newtype KeyringInfoToken = KeyringInfoToken (ForeignPtr ())

data KeyringInfo = KeyringInfo
	{ keyringLockOnIdle  :: Bool
	, keyringLockTimeout :: Word32
	, keyringMTime       :: UTCTime
	, keyringCTime       :: UTCTime
	, keyringIsLocked    :: Bool
	, keyringInfoToken   :: KeyringInfoToken
	}

-- The extra pointer shouldn't be printed out when showing a KeyringInfo,
-- so deriving(Show) can't be used. This instance acts like the
-- auto-generated instance, minus the pointer.
instance Show KeyringInfo where
	showsPrec d info = showParen (d > 10) $
		s "KeyringInfo" .
		s " {keyringLockOnIdle = " . shows (keyringLockOnIdle info) .
		s ", keyringLockTimeout = " . shows (keyringLockTimeout info) .
		s ", keyringMTime = " . shows (keyringMTime info) .
		s ", keyringCTime = " . shows (keyringCTime info) .
		s ", keyringIsLocked = " . shows (keyringIsLocked info) .
		s "}"
		where s = showString

-- GnomeKeyringOperationGetKeyringInfoCallback
type GetKeyringInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetKeyringInfoCallback
	as GetKeyringInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetKeyringInfoCallback :: GetKeyringInfoCallback
	                           -> IO GetKeyringInfoCallbackPtr

keyringInfoOperation :: OperationImpl GetKeyringInfoCallback KeyringInfo
keyringInfoOperation = operationImpl $ \checkResult ->
	wrapGetKeyringInfoCallback $ \cres ptr _ ->
	checkResult cres $ peekKeyringInfo ptr

copyInfo :: Ptr () -> IO (ForeignPtr ())
copyInfo = (newForeignPtr finalizeKeyringInfo =<<) . {# call copy as c_copy #}

peekKeyringInfo :: Ptr () -> IO KeyringInfo
peekKeyringInfo ptr = do
	lockOnIdle <- toBool `fmap` {# call get_lock_on_idle #} ptr
	timeout <- fromIntegral `fmap` {# call get_lock_timeout #} ptr
	mtime <- cToUTC `fmap` {# call get_mtime #} ptr
	ctime <- cToUTC `fmap` {# call get_ctime #} ptr
	isLocked <- toBool `fmap` {# call get_is_locked #} ptr
	copy <- copyInfo ptr
	let token = KeyringInfoToken copy
	return $ KeyringInfo lockOnIdle timeout mtime ctime isLocked token

stealKeyringInfoPtr :: Ptr (Ptr ()) -> IO KeyringInfo
stealKeyringInfoPtr ptr = do
	infoPtr <- newForeignPtr finalizeKeyringInfo =<< peek ptr
	withForeignPtr infoPtr peekKeyringInfo

withKeyringInfo :: KeyringInfo -> (Ptr () -> IO a) -> IO a
withKeyringInfo info io = do
	let (KeyringInfoToken infoPtr) = keyringInfoToken info
	copy <- withForeignPtr infoPtr copyInfo
	withForeignPtr copy $ \ptr -> do
	{# call set_lock_on_idle #} ptr . fromBool . keyringLockOnIdle $ info
	{# call set_lock_timeout #} ptr . fromIntegral . keyringLockTimeout $ info
	io ptr

foreign import ccall "gnome-keyring.h &gnome_keyring_info_free"
	finalizeKeyringInfo :: FunPtr (Ptr a -> IO ())
