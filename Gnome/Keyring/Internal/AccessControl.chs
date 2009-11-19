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
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Internal.AccessControl
	( AccessControl (..)
	, AccessType (..)
	, accessControlListOperation
	, withACL
	, stealACL
	, cAccessTypes
	) where
import Control.Exception (bracket)
import Data.Set (Set, toList, fromList)
import Data.Text.Lazy (Text)
import Foreign
import Foreign.C
import Gnome.Keyring.Internal.FFI
import Gnome.Keyring.Internal.Operation

{# enum GnomeKeyringAccessType as RawAccessType {} deriving (Show) #}

data AccessType
	= AccessRead
	| AccessWrite
	| AccessRemove
	deriving (Show, Eq, Ord)

data AccessControl = AccessControl
	{ accessControlName :: Maybe Text
	, accessControlPath :: Maybe Text
	, accessControlType :: Set AccessType
	}
	deriving (Show, Eq)

peekAccessControl :: Ptr () -> IO AccessControl
peekAccessControl ac = do
	name <- stealNullableText =<< {# call unsafe item_ac_get_display_name #} ac
	path <- stealNullableText =<< {# call unsafe item_ac_get_path_name #} ac
	cType <- {# call unsafe item_ac_get_access_type #} ac
	return $ AccessControl name path $ peekAccessType cType

stealACL :: Ptr (Ptr ()) -> IO [AccessControl]
stealACL ptr = bracket (peek ptr) freeACL (mapGList peekAccessControl)

withACL :: [AccessControl] -> (Ptr () -> IO a) -> IO a
withACL acl = bracket (buildACL acl) freeACL

buildACL :: [AccessControl] -> IO (Ptr ())
buildACL acs = bracket
	{# call unsafe application_ref_new #}
	{# call unsafe application_ref_free #} $ \appRef ->
	buildACL' appRef acs nullPtr

buildACL' :: Ptr () -> [AccessControl] -> Ptr () -> IO (Ptr ())
buildACL'      _       [] list = return list
buildACL' appRef (ac:acs) list = buildAC appRef ac
	>>= {# call unsafe g_list_append #} list
	>>= buildACL' appRef acs

buildAC :: Ptr () -> AccessControl -> IO (Ptr ())
buildAC appRef ac = do
	let cAllowed = cAccessTypes $ accessControlType ac
	ptr <- {# call unsafe access_control_new #} appRef cAllowed
	withNullableText (accessControlName ac) $ {# call unsafe item_ac_set_display_name #} ptr
	withNullableText (accessControlPath ac) $ {# call unsafe item_ac_set_path_name #} ptr
	return ptr

freeACL :: Ptr () -> IO ()
freeACL = {# call unsafe acl_free #}

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

accessControlListOperation :: OperationImpl GetListCallback [AccessControl]
accessControlListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ mapGList peekAccessControl list
