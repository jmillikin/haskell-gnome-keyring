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

module Gnome.Keyring.Attribute.Internal where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)

import Foreign
import Foreign.C
import Gnome.Keyring.FFI

{# enum GnomeKeyringAttributeType as AttributeType {} #}

data Attribute
	= TextAttribute Text Text
	| WordAttribute Text Word32
	deriving (Show, Eq)

attributeName :: Attribute -> Text
attributeName (TextAttribute n _) = n
attributeName (WordAttribute n _) = n

withAttributeList :: [Attribute] -> (Ptr () -> IO a) -> IO a
withAttributeList attrs io = bracket newList freeList buildList where
	newList = {# call unsafe g_array_new #} 0 0 {# sizeof GnomeKeyringAttribute #}
	buildList list = sequence (map (append list) attrs) >> io list
	append list (TextAttribute n x) = appendString list n x
	append list (WordAttribute n x) = appendUInt32 list n x

{# fun unsafe attribute_list_append_string as appendString
	{ id `Ptr ()'
	, withText* `Text'
	, withText* `Text'
	} -> `()' id #}

{# fun unsafe attribute_list_append_uint32 as appendUInt32
	{ id `Ptr ()'
	, withText* `Text'
	, fromIntegral `Word32'
	} -> `()' id #}

peekAttributeList :: Ptr () -> IO [Attribute]
peekAttributeList array = do
	len <- {# get GArray->len #} array
	start <- {# get GArray->data #} array
	peekAttributeList' (fromIntegral len) (castPtr start)

peekAttributeList' :: Integer -> Ptr () -> IO [Attribute]
peekAttributeList' 0   _ = return []
peekAttributeList' n ptr = do
	attr <- peekAttribute ptr
	attrs <- peekAttributeList' (n - 1) (plusPtr ptr {# sizeof GnomeKeyringAttribute #})
	return $ attr : attrs

peekAttribute :: Ptr () -> IO Attribute
peekAttribute attr = do
	name <- peekText =<< {# get GnomeKeyringAttribute->name #} attr
	cType <- {# get GnomeKeyringAttribute->type #} attr
	case toEnum . fromIntegral $ cType of
		ATTRIBUTE_TYPE_STRING -> do
			value <- peekText =<< {# get GnomeKeyringAttribute.value.string #} attr
			return $ TextAttribute name value
		ATTRIBUTE_TYPE_UINT32 -> do
			cValue <- {# get GnomeKeyringAttribute.value.integer #} attr
			return $ WordAttribute name $ fromIntegral cValue

stealAttributeList :: Ptr (Ptr ()) -> IO [Attribute]
stealAttributeList ptr = bracket (peek ptr) freeList peekAttributeList

freeList :: Ptr () -> IO ()
freeList = {# call unsafe attribute_list_free #}
