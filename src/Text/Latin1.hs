{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Latin-1 utility functions.
module Text.Latin1
  (
  -- * Latin-1 checks 
    IsLatin1(..)
  , isLatin1
  , Latin1
  , asciiIsLatin1
  , maybeLatin1
  , latin1
  -- * Character properties
  , isControl
  , isPrintable
  , isWhiteSpace
  , isLower
  , isUpper
  , toLower
  , toUpper
  , isAlpha
  , isAlphaNum
  -- * Byte properties
  , isControl8
  , isPrintable8
  , isWhiteSpace8
  , isLower8
  , isUpper8
  , toLower8
  , toUpper8
  , isAlpha8
  , isAlphaNum8
  ) where

import Data.Checked
import Data.Char (ord, chr)
import Data.Word (Word8)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Ascii (Ascii)
import qualified Text.Ascii as A

data IsLatin1 = IsLatin1

instance Property IsLatin1 Char where
  holds _ = (< 256) . ord
  {-# INLINE holds #-}

instance Property IsLatin1 α ⇒ Property IsLatin1 [α] where
  holds _ = all isLatin1
  {-# INLINE holds #-}

instance Property IsLatin1 TS.Text where
  holds _ = TS.all isLatin1
  {-# INLINE holds #-}

instance Property IsLatin1 TL.Text where
  holds _ = TL.all isLatin1
  {-# INLINE holds #-}

isLatin1 ∷ Property IsLatin1 v ⇒ v → Bool 
isLatin1 = holds IsLatin1
{-# INLINE isLatin1 #-}

type Latin1 α = Checked IsLatin1 α

-- | ASCII values are Latin-1 values.
asciiIsLatin1 ∷ Ascii α → Latin1 α
asciiIsLatin1 = trustMe . checked
{-# INLINE asciiIsLatin1 #-}

-- | Map a character to its Latin-1 encoding if possible, otherwise
--   return 'Nothing'.
maybeLatin1 ∷ Char → Maybe Word8
maybeLatin1 c | isLatin1 c = Just $ latin1 c
              | otherwise  = Nothing
{-# INLINABLE maybeLatin1 #-}

-- | Encode a Latin-1 character. No checks is performed.
latin1 ∷ Char → Word8
latin1 = fromIntegral . ord
{-# INLINE latin1 #-}

-- | Test if a character is a Latin-1 control character.
isControl ∷ Char → Bool
isControl c = w < 32 || (w >= 127 && w <= 159)
  where w = ord c
{-# INLINE isControl #-}

-- | Test if a character is a Latin-1 printable character.
isPrintable ∷ Char → Bool
isPrintable c = A.isPrintable c || (w >= 160 && w < 256)
  where w = ord c
{-# INLINE isPrintable #-}

-- | Test if a character is a Latin-1 whitespace character.
isWhiteSpace ∷ Char → Bool
isWhiteSpace c = A.isWhiteSpace c || w == 133 || w == 160
  where w = ord c
{-# INLINE isWhiteSpace #-}

-- | Test if a character is a Latin-1 lower-case letter.
isLower ∷ Char → Bool
isLower c = A.isLower c || (w >= 223 && w < 256 && w /= 247)
  where w = ord c
{-# INLINE isLower #-}

-- | Test if a character is a Latin-1 upper-case letter.
isUpper ∷ Char → Bool
isUpper c = A.isUpper c || (w >= 192 && w <= 222 && w /= 215)
  where w = ord c
{-# INLINE isUpper #-}

-- | Map lower-case Latin-1 letters to the corresponding upper-case letters,
--   leaving other characters as is.
toLower ∷ Char → Char
toLower c | isUpper c = chr (ord c + 32)
          | otherwise = c
{-# INLINABLE toLower #-}

-- | Map upper-case Latin-1 letters to the corresponding lower-case letters,
--   leaving other characters as is.
toUpper ∷ Char → Char
toUpper c | w ← ord c
          , A.isLower c || (w >= 224 && w <= 254 && w /= 247)
          = chr (ord c - 32)
          | otherwise
          = c
{-# INLINABLE toUpper #-}

-- | Test if a character is a Latin-1 letter.
isAlpha ∷ Char → Bool
isAlpha c = isUpper c || isLower c
{-# INLINABLE isAlpha #-}

-- | Test if a character is either a Latin-1 letter or a decimal digit.
isAlphaNum ∷ Char → Bool
isAlphaNum c = A.isDigit c || isAlpha c
{-# INLINABLE isAlphaNum #-}

-- | Test if a byte is the encoding of a Latin-1 control character.
isControl8 ∷ Word8 → Bool
isControl8 w = w < 32 || (w >= 127 && w <= 159)
{-# INLINE isControl8 #-}

-- | Test if a byte is the encoding of a Latin-1 printable character.
isPrintable8 ∷ Word8 → Bool
isPrintable8 w = A.isPrintable8 w || w >= 160
{-# INLINE isPrintable8 #-}

-- | Test if a byte is the encoding of a Latin-1 whitespace character.
isWhiteSpace8 ∷ Word8 → Bool
isWhiteSpace8 w = A.isWhiteSpace8 w || w == 133 || w == 160
{-# INLINE isWhiteSpace8 #-}

-- | Test if a byte is the encoding of a Latin-1 lower-case letter.
isLower8 ∷ Word8 → Bool
isLower8 w = A.isLower8 w || (w >= 223 && w /= 247)
{-# INLINE isLower8 #-}

-- | Test if a byte is the encoding of a Latin-1 upper-case letter.
isUpper8 ∷ Word8 → Bool
isUpper8 w = A.isUpper8 w || (w >= 192 && w <= 222 && w /= 215)
{-# INLINE isUpper8 #-}

-- | Map the encodings of lower-case Latin-1 letters to the encodings of
--   the corresponding upper-case letters, leaving other bytes as is.
toLower8 ∷ Word8 → Word8
toLower8 w | isUpper8 w = w + 32
           | otherwise  = w
{-# INLINABLE toLower8 #-}

-- | Map the encodings of upper-case Latin-1 letters to the encodings of
--   the corresponding lower-case letters, leaving other bytes as is.
toUpper8 ∷ Word8 → Word8
toUpper8 w | A.isLower8 w || (w >= 224 && w /= 247 && w /= 255) = w - 32
           | otherwise = w
{-# INLINABLE toUpper8 #-}

-- | Test if a byte is the encoding of a Latin-1 letter.
isAlpha8 ∷ Word8 → Bool
isAlpha8 w = isUpper8 w || isLower8 w
{-# INLINABLE isAlpha8 #-}

-- | Test if a byte is the encoding of either a Latin-1 letter
--   or a decimal digit.
isAlphaNum8 ∷ Word8 → Bool
isAlphaNum8 w = A.isDigit8 w || isAlpha8 w
{-# INLINABLE isAlphaNum8 #-}

