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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
#include <gnome-keyring.h>
module Gnome.Keyring.FFI where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Gnome.Keyring.Types as T

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

stealText :: CString -> IO Text
stealText cstr = bracket (return cstr) free peekText

stealNullableText :: CString -> IO (Maybe Text)
stealNullableText cstr = bracket (return cstr) free peekNullableText

stealPeek :: (Ptr a -> IO b) -> Ptr (Ptr a) -> IO b
stealPeek io ptr = bracket (peek ptr) free io

stealTextPtr :: Ptr CString -> IO Text
stealTextPtr = stealPeek peekText

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

stealWordList :: Ptr (Ptr ()) -> IO [Integer]
stealWordList ptr = do
	list <- peek ptr
	items <- mapGList (return . toInteger . ptrToWordPtr) list
	{# call unsafe g_list_free #} list
	return items

--------------

type DestroyNotify = Ptr () -> IO ()
{# pointer GDestroyNotify as DestroyNotifyPtr #}
foreign import ccall "wrapper"
	wrapDestroyNotify :: DestroyNotify -> IO DestroyNotifyPtr

--------------

class Callback a b | a -> b where
	callbackToPtr :: a -> FunPtr z
	freeCallback :: a -> IO ()
	buildCallback :: (b -> IO ()) -> (T.Error -> IO ()) -> IO a

-- GnomeKeyringOperationDoneCallback
data DoneCallback = DoneCallback DoneCallbackPtr
instance Callback DoneCallback () where
	callbackToPtr (DoneCallback x) = castFunPtr x
	freeCallback  (DoneCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapDoneCallback $ \cres _ ->
			case result cres of
				T.RESULT_OK -> onSuccess ()
				x -> onError $ T.resultToError x
		return $ DoneCallback funptr

type RawDoneCallback = CInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationDoneCallback as DoneCallbackPtr #}
foreign import ccall "wrapper"
	wrapDoneCallback :: RawDoneCallback -> IO DoneCallbackPtr

-- GnomeKeyringOperationGetStringCallback
data GetStringCallback = GetStringCallback GetStringCallbackPtr
instance Callback GetStringCallback Text where
	callbackToPtr (GetStringCallback x) = castFunPtr x
	freeCallback  (GetStringCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetStringCallback $ \cres cstr _ ->
			case result cres of
				T.RESULT_OK -> do
					str <- peekCString cstr
					onSuccess (Text.pack str)
				x -> onError $ T.resultToError x
		return $ GetStringCallback funptr

data GetNullableStringCallback = GetNullableStringCallback GetStringCallbackPtr
instance Callback GetNullableStringCallback (Maybe Text) where
	callbackToPtr (GetNullableStringCallback x) = castFunPtr x
	freeCallback  (GetNullableStringCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetStringCallback $ \cres cstr _ ->
			case result cres of
				T.RESULT_OK -> peekNullableText cstr >>= onSuccess
				x -> onError $ T.resultToError x
		return $ GetNullableStringCallback funptr

type RawGetStringCallback = CInt -> CString -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetStringCallback as GetStringCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetStringCallback :: RawGetStringCallback -> IO GetStringCallbackPtr

-- GnomeKeyringOperationGetIntCallback
data GetWordCallback = GetWordCallback GetIntCallbackPtr
instance Callback GetWordCallback Word32 where
	callbackToPtr (GetWordCallback x) = castFunPtr x
	freeCallback  (GetWordCallback x) = freeHaskellFunPtr x
	buildCallback onSuccess onError = do
		funptr <- wrapGetIntCallback $ \cres cint _ ->
			case result cres of
				T.RESULT_OK -> onSuccess $ fromIntegral cint
				x -> onError $ T.resultToError x
		return $ GetWordCallback funptr

type RawGetIntCallback = CInt -> CUInt -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetIntCallback as GetIntCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetIntCallback :: RawGetIntCallback -> IO GetIntCallbackPtr

-- GnomeKeyringOperationGetListCallback
mkListCallback :: (GetListCallbackPtr -> a)
               -> (Ptr () -> IO b)
               -> ([b] -> IO ())
               -> (T.Error -> IO ())
               -> IO a
mkListCallback mkCallback f onSuccess onError = do
	funptr <- wrapGetListCallback $ \cres list _ ->
		case result cres of
			T.RESULT_OK -> do
				items <- mapGList f list
				onSuccess items
			x -> onError $ T.resultToError x
	return $ mkCallback funptr

data GetTextListCallback = GetTextListCallback GetListCallbackPtr
instance Callback GetTextListCallback [Text] where
	callbackToPtr (GetTextListCallback x) = castFunPtr x
	freeCallback  (GetTextListCallback x) = freeHaskellFunPtr x
	buildCallback = mkListCallback GetTextListCallback
		(peekText . castPtr)

data GetWordListCallback = GetWordListCallback GetListCallbackPtr
instance Callback GetWordListCallback [Integer] where
	callbackToPtr (GetWordListCallback x) = castFunPtr x
	freeCallback  (GetWordListCallback x) = freeHaskellFunPtr x
	buildCallback = mkListCallback GetWordListCallback
		(return . toInteger . ptrToWordPtr)

type RawGetListCallback = CInt -> Ptr () -> Ptr () -> IO ()
{# pointer GnomeKeyringOperationGetListCallback as GetListCallbackPtr #}
foreign import ccall "wrapper"
	wrapGetListCallback :: RawGetListCallback -> IO GetListCallbackPtr
