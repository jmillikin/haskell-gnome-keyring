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
#include <gnome-keyring.h>
{# context prefix = "gnome_keyring_" #}

module Gnome.Keyring.Attribute.Internal where
import Control.Exception (bracket)
import Data.Text.Lazy (Text)

import Foreign
import Foreign.C
import Gnome.Keyring.FFI

data Attribute
	= TextAttribute Text Text
	| WordAttribute Text Word32

attributeName :: Attribute -> Text
attributeName (TextAttribute n _) = n
attributeName (WordAttribute n _) = n

withAttributeList :: [Attribute] -> (Ptr () -> IO a) -> IO a
withAttributeList attrs io = bracket newList freeList buildList where
	newList = {# call g_array_new #} 0 0 {# sizeof GnomeKeyringAttribute #}
	freeList = {# call attribute_list_free #}
	buildList list = sequence (map (append list) attrs) >> io list
	append list (TextAttribute n x) = appendString list n x
	append list (WordAttribute n x) = appendUInt32 list n x

{# fun attribute_list_append_string as appendString
	{ id `Ptr ()'
	, withText* `Text'
	, withText* `Text'
	} -> `()' id #}

{# fun attribute_list_append_uint32 as appendUInt32
	{ id `Ptr ()'
	, withText* `Text'
	, fromIntegral `Word32'
	} -> `()' id #}
