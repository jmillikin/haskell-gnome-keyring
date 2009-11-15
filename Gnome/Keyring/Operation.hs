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

module Gnome.Keyring.Operation where
import Foreign (Ptr)
import qualified Foreign as F
import qualified Gnome.Keyring.Types as T
import qualified Gnome.Keyring.Bindings as B
import Control.Exception (throwIO)

data Operation a = Operation
	{ async  :: (T.Error -> IO ()) -> (a -> IO ()) -> IO T.CancellationKey
	, async' :: (T.Error -> IO ()) ->       IO ()  -> IO T.CancellationKey
	, sync   :: IO a
	}

operation :: B.Callback a b => (a -> Ptr () -> B.DestroyNotifyPtr -> IO T.CancellationKey)
          -> IO (T.Result, b) -> Operation b
operation asyncIO syncIO = Operation
	{ async  = runAsync asyncIO
	, async' = runNullAsync asyncIO
	, sync   = runSync syncIO
	}

runNullAsync :: B.Callback a b => (a -> Ptr () -> B.DestroyNotifyPtr -> IO T.CancellationKey)
             -> (T.Error -> IO ()) -> IO () -> IO T.CancellationKey
runNullAsync io onError = runAsync io onError . const

runSync :: IO (T.Result, a) -> IO a
runSync io = do
	(res, x) <- io
	checkResult res
	return x

checkResult :: T.Result -> IO ()
checkResult T.RESULT_OK = return ()
checkResult x           = throwIO $ T.resultToError x

----------------------------------

-- async support

data Context = Context
	{ contextFreeCallback :: IO ()
	, contextFreeDestroy  :: IO ()
	}

destroyNotify :: IO B.DestroyNotifyPtr
destroyNotify = B.wrapDestroyNotify $ \ptr -> do
	let stable = F.castPtrToStablePtr ptr
	ctx <- F.deRefStablePtr stable
	contextFreeCallback ctx
	contextFreeDestroy ctx
	F.freeStablePtr stable

runAsync :: B.Callback a b => (a -> Ptr () -> B.DestroyNotifyPtr -> IO T.CancellationKey)
         -> (T.Error -> IO ()) -> (b -> IO ()) -> IO T.CancellationKey
runAsync io onError onSuccess = do
	destroy <- destroyNotify
	callback <- B.buildCallback onSuccess onError
	let context = Context
		(B.freeCallback callback)
		(F.freeHaskellFunPtr destroy)
	stable <- F.newStablePtr context
	io callback (F.castStablePtrToPtr stable) destroy
