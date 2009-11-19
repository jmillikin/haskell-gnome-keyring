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
module Gnome.Keyring.Operation.Internal
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
import Data.Text.Lazy (Text)
import Foreign
import Foreign.C
import qualified Gnome.Keyring.Types as T
import qualified Gnome.Keyring.FFI as B
import Control.Exception (throwIO)

data Operation a = Operation
	{ async    :: (T.Error -> IO ()) -> (a -> IO ()) -> IO T.CancellationKey
	, syncImpl :: IO (T.Result, a)
	}

-- Synchronous operation public API
sync :: Operation a -> IO a
sync op = do
	(res, x) <- syncImpl op
	case res of
		T.RESULT_OK -> return x
		_           -> throwIO $ T.resultToError res

-- Helper for async operations which return nothing useful
async' :: Operation a -> (T.Error -> IO ()) -> IO ()  -> IO T.CancellationKey
async' op onError onSuccess = async op onError (const onSuccess)

-- Implementation details of async operations
type OperationImpl a b = (FunPtr a -> Ptr () -> B.DestroyNotifyPtr -> IO T.CancellationKey) -> IO (T.Result, b) -> Operation b
operationImpl :: ((CInt -> IO a -> IO ()) -> IO (FunPtr b)) -> OperationImpl b a
operationImpl impl asyncIO = Operation $ \onError onSuccess -> do
	
	callback <- impl $ \cres io -> case B.result cres of
		T.RESULT_OK -> io >>= onSuccess
		x -> onError $ T.resultToError x
	
	destroy <- B.wrapDestroyNotify $ \ptr -> do
		let stable = castPtrToStablePtr ptr
		freeCallbacks <- deRefStablePtr stable
		freeCallbacks
		freeStablePtr stable
	
	stable <- newStablePtr $ do
		freeHaskellFunPtr callback
		freeHaskellFunPtr destroy
	
	asyncIO callback (castStablePtrToPtr stable) destroy

-- Available basic operation types

voidOperation :: OperationImpl B.DoneCallback ()
voidOperation = operationImpl $ \checkResult ->
	B.wrapDoneCallback $ \cres _ ->
	checkResult cres $ return ()

maybeTextOperation :: OperationImpl B.GetStringCallback (Maybe Text)
maybeTextOperation = operationImpl $ \checkResult ->
	B.wrapGetStringCallback $ \cres cstr _ ->
	checkResult cres $ B.peekNullableText cstr

textListOperation :: OperationImpl B.GetListCallback [Text]
textListOperation = operationImpl $ \checkResult ->
	B.wrapGetListCallback $ \cres list _ ->
	checkResult cres $ B.convertStringList list
