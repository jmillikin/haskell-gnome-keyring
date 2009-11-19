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
{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_info_" #}

module Gnome.Keyring.Internal.KeyringInfo
	( KeyringInfo (..)
	, keyringSetLockOnIdle
	, keyringSetLockTimeout
	
	, GetKeyringInfoCallbackPtr
	, keyringInfoOperation
	, withKeyringInfo
	, stealKeyringInfoPtr
	) where
import Foreign
import Foreign.C
import Gnome.Keyring.Internal.Operation

-- Our keyring info populates/is populated by the native info structure.
-- Clients can't create them directly, because GKR doesn't allow it.
data KeyringInfo = KeyringInfo
	{ keyringLockOnIdle  :: Bool
	, keyringLockTimeout :: Word32
	, keyringMTime       :: Integer -- TODO: TimeOfDay
	, keyringCTime       :: Integer -- TODO: TimeOfDay
	, keyringIsLocked    :: Bool
	, keyringInfoPtr     :: ForeignPtr ()
	}

-- | Set whether or not to lock a keyring after a certain amount of idle
-- time.
-- 
keyringSetLockOnIdle  :: KeyringInfo -> Bool -> KeyringInfo
keyringSetLockOnIdle info x = info {keyringLockOnIdle = x}

-- | Set the idle timeout, in seconds, after which to lock the keyring.
-- 
keyringSetLockTimeout :: KeyringInfo -> Word32 -> KeyringInfo
keyringSetLockTimeout info x = info {keyringLockTimeout = x}

-- GnomeKeyringOperationGetKeyringInfoCallback
type GetKeyringInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetKeyringInfoCallback
	as GetKeyringInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetKeyringInfoCallback :: GetKeyringInfoCallback
	                           -> IO GetKeyringInfoCallbackPtr

keyringInfoOperation :: OperationImpl GetKeyringInfoCallback KeyringInfo
keyringInfoOperation = operationImpl $ \checkResult ->
	wrapGetKeyringInfoCallback $ \cres ptr _ -> do
	checkResult cres $ peekKeyringInfo ptr

copyInfo :: Ptr () -> IO (ForeignPtr ())
copyInfo = (newForeignPtr finalizeKeyringInfo =<<) . {# call unsafe copy as c_copy #}

peekKeyringInfo :: Ptr () -> IO KeyringInfo
peekKeyringInfo ptr = do
	lockOnIdle <- toBool `fmap` {# call unsafe get_lock_on_idle #} ptr
	timeout <- fromIntegral `fmap` {# call unsafe get_lock_timeout #} ptr
	mtime <- toInteger `fmap` {# call unsafe get_mtime #} ptr
	ctime <- toInteger `fmap` {# call unsafe get_ctime #} ptr
	isLocked <- toBool `fmap` {# call unsafe get_is_locked #} ptr
	copy <- copyInfo ptr
	return $ KeyringInfo lockOnIdle timeout mtime ctime isLocked copy

stealKeyringInfoPtr :: Ptr (Ptr ()) -> IO KeyringInfo
stealKeyringInfoPtr ptr = do
	infoPtr <- newForeignPtr finalizeKeyringInfo =<< peek ptr
	withForeignPtr infoPtr peekKeyringInfo

withKeyringInfo :: KeyringInfo -> (Ptr () -> IO a) -> IO a
withKeyringInfo info io = do
	copy <- withForeignPtr (keyringInfoPtr info) copyInfo
	withForeignPtr copy $ \ptr -> do
	{# call unsafe set_lock_on_idle #} ptr . fromBool . keyringLockOnIdle $ info
	{# call unsafe set_lock_timeout #} ptr . fromIntegral . keyringLockTimeout $ info
	io ptr

foreign import ccall "gnome-keyring.h &gnome_keyring_info_free"
	finalizeKeyringInfo :: FunPtr (Ptr a -> IO ())
