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
module Gnome.Keyring.Internal.Operation
	(
	-- * Public API
	  Operation
	, async
	, async'
	, sync
	
	-- * Implementation helpers for within G.KR
	, OperationImpl
	, operationImpl
	, voidOperation
	, maybeTextOperation
	, textListOperation
	) where
import Control.Exception (throwIO)
import Control.Monad (join)
import Data.Text.Lazy (Text)
import Gnome.Keyring.Internal.FFI
import Gnome.Keyring.Internal.Types

data Operation a = Operation
	{ async    :: (Error -> IO ()) -> (a -> IO ()) -> IO CancellationKey
	, syncImpl :: IO (Result, a)
	}

-- Synchronous operation public API
sync :: Operation a -> IO a
sync op = do
	(res, x) <- syncImpl op
	case res of
		RESULT_OK -> return x
		_           -> throwIO $ resultToError res

-- Helper for async operations which return nothing useful
async' :: Operation a -> (Error -> IO ()) -> IO ()  -> IO CancellationKey
async' op onError onSuccess = async op onError (const onSuccess)

-- Implementation details of async operations
type OperationImpl a b = (FunPtr a -> Ptr () -> DestroyNotifyPtr -> IO CancellationKey) -> IO (Result, b) -> Operation b
operationImpl :: ((CInt -> IO a -> IO ()) -> IO (FunPtr b)) -> OperationImpl b a
operationImpl impl asyncIO = Operation $ \onError onSuccess -> do
	
	callback <- impl $ \cres io -> case result cres of
		RESULT_OK -> io >>= onSuccess
		x -> onError $ resultToError x
	
	destroy <- wrapDestroyNotify $ \ptr -> do
		let stable = castPtrToStablePtr ptr
		void . join $ deRefStablePtr stable
		freeStablePtr stable
	
	stable <- newStablePtr $ do
		freeHaskellFunPtr callback
		freeHaskellFunPtr destroy
	
	asyncIO callback (castStablePtrToPtr stable) destroy

-- Available basic operation types

voidOperation :: OperationImpl DoneCallback ()
voidOperation = operationImpl $ \checkResult ->
	wrapDoneCallback $ \cres _ ->
	checkResult cres $ return ()

maybeTextOperation :: OperationImpl GetStringCallback (Maybe Text)
maybeTextOperation = operationImpl $ \checkResult ->
	wrapGetStringCallback $ \cres cstr _ ->
	checkResult cres $ peekNullableText cstr

textListOperation :: OperationImpl GetListCallback [Text]
textListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ mapGList peekText list
