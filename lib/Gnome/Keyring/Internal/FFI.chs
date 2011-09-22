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
module Gnome.Keyring.Internal.FFI
	(
	-- * Callbacks
	  DestroyNotify
	, DestroyNotifyPtr
	, wrapDestroyNotify
	
	, DoneCallback
	, DoneCallbackPtr
	, wrapDoneCallback
	
	, GetStringCallback
	, GetStringCallbackPtr
	, wrapGetStringCallback
	
	, GetIntCallback
	, GetIntCallbackPtr
	, wrapGetIntCallback
	
	, GetListCallback
	, GetListCallbackPtr
	, wrapGetListCallback
	
	-- * Marshaling helpers
	, cToUTC
	, withText
	, peekText
	, withNullableText
	, peekNullableText
	, stealNullableText
	, mapGList
	, mapGArray
	
	-- * Re-export, since any clients of this module will need Foreign
	-- and Foreign.C anyway.
	, module Foreign
	, module Foreign.C
	) where

import           Control.Exception (bracket)
import qualified Data.ByteString as ByteString
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- Import unqualified for c2hs
import           Foreign
import           Foreign.C

#include <gnome-keyring.h>

cToUTC :: Integral a => a -> UTCTime
cToUTC = posixSecondsToUTCTime . fromIntegral

withText :: Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . encodeUtf8

peekText :: CString -> IO Text
peekText cstr
	| cstr == nullPtr = error "Gnome.Keyring.FFI.peekText nullPtr"
	| otherwise       = do
		bytes <- ByteString.packCString cstr
		return (decodeUtf8 bytes)

withNullableText :: Maybe Text -> (CString -> IO a) -> IO a
withNullableText = maybeWith withText

peekNullableText :: CString -> IO (Maybe Text)
peekNullableText = maybePeek peekText

stealNullableText :: CString -> IO (Maybe Text)
stealNullableText cstr = bracket (return cstr) free peekNullableText

-- Convert GList to []
mapGList :: (Ptr a -> IO b) -> Ptr () -> IO [b]
mapGList f list
	| list == nullPtr = return []
	| otherwise = do
		item <- {# get GList->data #} list
		next <- {# get GList->next #} list
		items <- mapGList f next
		item' <- f $ castPtr item
		return $ item' : items

-- Convert GArray to []
mapGArray :: (Ptr a -> IO b) -> Int -> Ptr () -> IO [b]
mapGArray f size array = do
	len <- {# get GArray->len #} array
	start <- {# get GArray->data #} array
	mapGArray' f size (fromIntegral len) (castPtr start)

mapGArray' :: (Ptr a -> IO b) -> Int -> Integer -> Ptr () -> IO [b]
mapGArray' _    _ 0   _ = return []
mapGArray' f size n ptr = do
	attr <- f $ castPtr ptr
	attrs <- mapGArray' f size (n - 1) (plusPtr ptr size)
	return $ attr : attrs

--------------

-- GDestroyNotify
type DestroyNotify = Ptr () -> IO ()
{# pointer GDestroyNotify as DestroyNotifyPtr #}
foreign import ccall "wrapper"
	wrapDestroyNotify :: DestroyNotify -> IO DestroyNotifyPtr

-- GnomeKeyringOperationDoneCallback
type DoneCallback = CInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationDoneCallback as DoneCallbackPtr #}
foreign import ccall "wrapper"
	wrapDoneCallback :: DoneCallback -> IO DoneCallbackPtr

-- GnomeKeyringOperationGetStringCallback
type GetStringCallback = CInt -> CString -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetStringCallback as GetStringCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetStringCallback :: GetStringCallback -> IO GetStringCallbackPtr

-- GnomeKeyringOperationGetIntCallback
type GetIntCallback = CInt -> CUInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetIntCallback as GetIntCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetIntCallback :: GetIntCallback -> IO GetIntCallbackPtr

-- GnomeKeyringOperationGetListCallback
type GetListCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetListCallback as GetListCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetListCallback :: GetListCallback -> IO GetListCallbackPtr
