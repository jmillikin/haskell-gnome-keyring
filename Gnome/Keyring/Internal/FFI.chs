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
	, result
	, resultAndTuple
	, withText
	, peekText
	, withNullableText
	, peekNullableText
	, stealNullableText
	, stealNullableTextPtr
	, stealTextList
	, convertStringList
	, mapGList
	) where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Gnome.Keyring.Internal.Types as T

-- Import unqualified for c2hs
import Foreign
import Foreign.C

result :: CInt -> T.Result
result = toEnum . fromIntegral

resultAndTuple :: CInt -> (T.Result, ())
resultAndTuple x = (result x, ())

withText :: Text -> (CString -> IO a) -> IO a
withText = BS.useAsCString . BS.concat . BSL.toChunks . encodeUtf8

peekText :: CString -> IO Text
peekText cstr
	| cstr == nullPtr = error $ "Gnome.Keyring.FFI.peekText nullPtr"
	| otherwise       = do
		bytes <- BS.packCString cstr
		return . decodeUtf8 . BSL.fromChunks $ [bytes]

withNullableText :: Maybe Text -> (CString -> IO a) -> IO a
withNullableText = maybeWith withText

peekNullableText :: CString -> IO (Maybe Text)
peekNullableText = maybePeek peekText

stealNullableText :: CString -> IO (Maybe Text)
stealNullableText cstr = bracket (return cstr) free peekNullableText

stealPeek :: (Ptr a -> IO b) -> Ptr (Ptr a) -> IO b
stealPeek io ptr = bracket (peek ptr) free io

stealNullableTextPtr :: Ptr CString -> IO (Maybe Text)
stealNullableTextPtr = stealPeek peekNullableText

-- Convert GList to []
mapGList :: (Ptr () -> IO a) -> Ptr () -> IO [a]
mapGList f list
	| list == nullPtr = return []
	| otherwise = do
		item <- {# get GList->data #} list
		next <- {# get GList->next #} list
		items <- mapGList f next
		item' <- f item
		return $ item' : items

convertStringList :: Ptr () -> IO [Text]
convertStringList = mapGList (peekText . castPtr)

stealTextList :: Ptr (Ptr ()) -> IO [Text]
stealTextList ptr = do
	list <- peek ptr
	items <- convertStringList list
	{# call unsafe gnome_keyring_string_list_free #} list
	return items

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
