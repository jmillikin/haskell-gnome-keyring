{- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_info_" #}

module Gnome.Keyring.KeyringInfo where

-- Import unqualified for c2hs
import Foreign
import Foreign.C
import Gnome.Keyring.Bindings
import Gnome.Keyring.Types

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

keyringSetLockOnIdle  :: KeyringInfo -> Bool -> KeyringInfo
keyringSetLockOnIdle info x = info {keyringLockOnIdle = x}

keyringSetLockTimeout :: KeyringInfo -> Word32 -> KeyringInfo
keyringSetLockTimeout info x = info {keyringLockTimeout = x}

-- GnomeKeyringOperationGetKeyringInfoCallback
data GetKeyringInfoCallback = GetKeyringInfoCallback GetKeyringInfoCallbackPtr
instance Callback GetKeyringInfoCallback KeyringInfo where
	callbackToPtr (GetKeyringInfoCallback x) = castFunPtr x
	freeCallback  (GetKeyringInfoCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetKeyringInfoCallback $ \cres ptr _ -> do
			case result cres of
				RESULT_OK -> peekKeyringInfo ptr >>= onSuccess
				x -> onError $ resultToError x
		return $ GetKeyringInfoCallback funptr

type RawGetKeyringInfoCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetKeyringInfoCallback
	as GetKeyringInfoCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetKeyringInfoCallback :: RawGetKeyringInfoCallback
	                           -> IO GetKeyringInfoCallbackPtr

copyInfo :: Ptr () -> IO (ForeignPtr ())
copyInfo = (newForeignPtr finalizeKeyringInfo =<<) . {# call copy as c_copy #}

peekKeyringInfo :: Ptr () -> IO KeyringInfo
peekKeyringInfo ptr = do
	lockOnIdle <- toBool `fmap` {# call get_lock_on_idle #} ptr
	timeout <- fromIntegral `fmap` {# call get_lock_timeout #} ptr
	mtime <- toInteger `fmap` {# call get_mtime #} ptr
	ctime <- toInteger `fmap` {# call get_ctime #} ptr
	isLocked <- toBool `fmap` {# call get_is_locked #} ptr
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
	{# call set_lock_on_idle #} ptr . fromBool . keyringLockOnIdle $ info
	{# call set_lock_timeout #} ptr . fromIntegral . keyringLockTimeout $ info
	io ptr

foreign import ccall "gnome-keyring.h &gnome_keyring_info_free"
	finalizeKeyringInfo :: FunPtr (Ptr a -> IO ())
