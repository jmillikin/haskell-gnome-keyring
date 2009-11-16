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
{-# LANGUAGE FlexibleInstances #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.AccessControl.Internal where
import Control.Exception (bracket)
import Data.Set (Set, toList, fromList)
import Data.Text.Lazy (Text)
import Foreign
import Foreign.C
import Gnome.Keyring.FFI hiding (g_list_free)

{# enum GnomeKeyringAccessType as RawAccessType {} deriving (Show) #}

data AccessType
	= AccessRead
	| AccessWrite
	| AccessRemove
	deriving (Show, Eq, Ord)

data AccessControl = AccessControl
	{ accessControlName :: Text
	, accessControlPath :: Text
	, accessControlType :: Set AccessType
	}
	deriving (Show, Eq)

peekAccessControl :: Ptr () -> IO AccessControl
peekAccessControl ac = do
	name <- stealText =<< {# call item_ac_get_display_name #} ac
	path <- stealText =<< {# call item_ac_get_path_name #} ac
	cType <- {# call item_ac_get_access_type #} ac
	return $ AccessControl name path $ peekAccessType cType

stealACL :: Ptr (Ptr ()) -> IO [AccessControl]
stealACL ptr = bracket (peek ptr) freeACL (mapGList peekAccessControl)

freeACL :: Ptr () -> IO ()
freeACL = {# call acl_free #}

cAccessTypes :: Bits a => Set AccessType -> a
cAccessTypes = foldr (.|.) 0 . map (fromIntegral . fromEnum . fromAccessType) . toList where

peekAccessType :: Integral a => a -> Set AccessType
peekAccessType cint = fromList $ concat
	[ [AccessRead   | int .&. fromEnum ACCESS_READ   > 0]
	, [AccessWrite  | int .&. fromEnum ACCESS_WRITE  > 0]
	, [AccessRemove | int .&. fromEnum ACCESS_REMOVE > 0]
	]
	where int = fromIntegral cint

fromAccessType :: AccessType -> RawAccessType
fromAccessType AccessRead   = ACCESS_READ
fromAccessType AccessWrite  = ACCESS_WRITE
fromAccessType AccessRemove = ACCESS_REMOVE

toAccessType :: RawAccessType -> AccessType
toAccessType ACCESS_READ   = AccessRead
toAccessType ACCESS_WRITE  = AccessWrite
toAccessType ACCESS_REMOVE = AccessRemove

data GetACLCallback = GetACLCallback GetListCallbackPtr
instance Callback GetACLCallback [AccessControl] where
	callbackToPtr (GetACLCallback x) = castFunPtr x
	freeCallback  (GetACLCallback x) = freeHaskellFunPtr x
	buildCallback = mkListCallback GetACLCallback
		peekAccessControl
