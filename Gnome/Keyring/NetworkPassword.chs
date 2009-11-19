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
-- |
-- Maintainer  : John Millikin <jmillikin@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (FFI)
-- 
-- Networks passwords are a simple way of saving passwords associated with
-- a certain user, server, protocol, and other fields.

{-# LANGUAGE ForeignFunctionInterface #-}
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.NetworkPassword
	( NetworkPassword (..)
	, NetworkPasswordLocation (..)
	, findNetworkPassword
	, setNetworkPassword
	) where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)
import Gnome.Keyring.ItemInfo
import Gnome.Keyring.Operation.Internal
import Gnome.Keyring.Types
import Foreign
import Foreign.C
import Gnome.Keyring.FFI

data NetworkPassword = NetworkPassword
	{ networkPasswordKeyring  :: KeyringName
	, networkPasswordItemID   :: ItemID
	, networkPasswordLocation :: NetworkPasswordLocation
	, networkPassword         :: Text
	}
	deriving (Show, Eq)

data NetworkPasswordLocation = NetworkPasswordLocation
	{ locationProtocol :: Maybe Text
	, locationServer   :: Maybe Text
	, locationObject   :: Maybe Text
	, locationAuthType :: Maybe Text
	, locationPort     :: Word32
	, locationUser     :: Maybe Text
	, locationDomain   :: Maybe Text
	}
	deriving (Show, Eq)

-- | Find a previously stored 'NetworkPassword'. Searches all keyrings.
-- 
-- The user may be prompted to unlock necessary keyrings, and will be
-- prompted for access to the items if needed.
-- 
-- Network passwords are items with the 'ItemType' 'ItemNetworkPassword'.
-- 
findNetworkPassword :: NetworkPasswordLocation -> Operation [NetworkPassword]
findNetworkPassword loc = let
	p1 = locationUser     loc
	p2 = locationDomain   loc
	p3 = locationServer   loc
	p4 = locationObject   loc
	p5 = locationProtocol loc
	p6 = locationAuthType loc
	p7 = locationPort     loc
	in passwordListOperation
		(find_network_password p1 p2 p3 p4 p5 p6 p7)
		(find_network_password_sync p1 p2 p3 p4 p5 p6 p7)

{# fun unsafe find_network_password
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, fromIntegral `Word32'
	, id `GetListCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun find_network_password_sync
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, fromIntegral `Word32'
	, alloca- `[NetworkPassword]' stealPasswordList*
	} -> `Result' result #}

-- | Store a network password.
-- 
-- If an item already exists for with this network info (ie: user, server,
-- etc.) then it will be updated.
-- 
-- Whether a new item is created or not, the item's ID will be returned.
-- 
-- Network passwords are items with the 'ItemType' 'ItemNetworkPassword'.
-- 
setNetworkPassword :: Maybe KeyringName -> NetworkPasswordLocation ->
                      Text ->
                      Operation ItemID
setNetworkPassword k loc secret = let
	p1 = locationUser     loc
	p2 = locationDomain   loc
	p3 = locationServer   loc
	p4 = locationObject   loc
	p5 = locationProtocol loc
	p6 = locationAuthType loc
	p7 = locationPort     loc
	in itemIDOperation
		(set_network_password k p1 p2 p3 p4 p5 p6 p7 secret)
		(set_network_password_sync k p1 p2 p3 p4 p5 p6 p7 secret)

{# fun unsafe set_network_password
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, fromIntegral `Word32'
	, withText* `Text'
	, id `GetIntCallbackPtr'
	, id `Ptr ()'
	, id `DestroyNotifyPtr'
	} -> `CancellationKey' CancellationKey #}

{# fun set_network_password_sync
	{ withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, withNullableText* `Maybe Text'
	, fromIntegral `Word32'
	, withText* `Text'
	, alloca- `ItemID' peekItemID*
	} -> `Result' result #}

peekPassword :: Ptr () -> IO NetworkPassword
peekPassword pwd = do
	-- Password location
	protocol <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->protocol #} pwd
	server <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->server #} pwd
	object <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->object #} pwd
	authType <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->authtype #} pwd
	port <- fromIntegral `fmap` {# get GnomeKeyringNetworkPasswordData->port #} pwd
	user <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->user #} pwd
	domain <- peekNullableText =<< {# get GnomeKeyringNetworkPasswordData->domain #} pwd
	let loc = NetworkPasswordLocation
		{ locationProtocol = protocol
		, locationServer   = server
		, locationObject   = object
		, locationAuthType = authType
		, locationPort     = port
		, locationUser     = user
		, locationDomain   = domain
		}
	
	-- Keyring, item, and secret
	keyring <- peekText =<< {# get GnomeKeyringNetworkPasswordData->keyring #} pwd
	itemID <- (ItemID . fromIntegral) `fmap` {# get GnomeKeyringNetworkPasswordData->item_id #} pwd
	password <- peekText =<< {# get GnomeKeyringNetworkPasswordData->password #} pwd
	return $ NetworkPassword keyring itemID loc password

stealPasswordList :: Ptr (Ptr ()) -> IO [NetworkPassword]
stealPasswordList ptr = bracket (peek ptr)
	{# call unsafe network_password_list_free #}
	(mapGList peekPassword)

passwordListOperation :: OperationImpl GetListCallback [NetworkPassword]
passwordListOperation = operationImpl $ \checkResult ->
	wrapGetListCallback $ \cres list _ ->
	checkResult cres $ mapGList peekPassword list
