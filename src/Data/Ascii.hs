{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Ascii
  ( IsAscii(..)
  , isAscii
  , Ascii
  , maybeAscii
  , ascii
  , isControl
  , isPrintable
  , isWhiteSpace
  , isSpaceOrTab
  , isLower
  , isUpper
  , toLower
  , toUpper
  , isAlpha
  , isDigit
  , isNzDigit
  , isAlphaNum
  , fromDigit
  , fromNzDigit
  , unsafeFromDigit
  , isOctDigit
  , isNzOctDigit
  , fromOctDigit
  , fromNzOctDigit
  , unsafeFromOctDigit
  , isUpHexDigit
  , isNzUpHexDigit
  , fromUpHexDigit
  , fromNzUpHexDigit
  , unsafeFromUpHexDigit
  , isLowHexDigit
  , isNzLowHexDigit
  , fromLowHexDigit
  , fromNzLowHexDigit
  , unsafeFromLowHexDigit
  , isHexDigit
  , isNzHexDigit
  , fromHexDigit
  , fromNzHexDigit
  , unsafeFromHexDigit
  , isControl8
  , isPrintable8
  , isWhiteSpace8
  , isSpaceOrTab8
  , toLower8
  , toUpper8
  , isAlpha8
  , isDigit8
  , isNzDigit8
  , isAlphaNum8
  , fromDigit8
  , fromNzDigit8
  , unsafeFromDigit8
  , isOctDigit8
  , isNzOctDigit8
  , fromOctDigit8
  , fromNzOctDigit8
  , unsafeFromOctDigit8
  , isUpHexDigit8
  , isNzUpHexDigit8
  , fromUpHexDigit8
  , fromNzUpHexDigit8
  , unsafeFromUpHexDigit8
  , isLowHexDigit8
  , isNzLowHexDigit8
  , fromLowHexDigit8
  , fromNzLowHexDigit8
  , unsafeFromLowHexDigit8
  , isHexDigit8
  , isNzHexDigit8
  , fromHexDigit8
  , fromNzHexDigit8
  , unsafeFromHexDigit8
  ) where

import Data.Checked
import Data.Char (ord, chr)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

data IsAscii = IsAscii

instance Property IsAscii Word8 where
  holds _ = (< 128)
  {-# INLINE holds #-}

instance Property IsAscii ByteString where
  holds _ = BS.all isAscii
  {-# INLINE holds #-}

instance Property IsAscii Char where
  holds _ = (< 128) . ord
  {-# INLINE holds #-}

instance Property IsAscii Text where
  holds _ = T.all isAscii
  {-# INLINE holds #-}

isAscii ∷ Property IsAscii v ⇒ v → Bool 
isAscii = holds IsAscii
{-# INLINE isAscii #-}

type Ascii α = Checked IsAscii α

maybeAscii ∷ Char → Maybe Word8
maybeAscii c | isAscii c = Just $ ascii c
             | otherwise = Nothing
{-# INLINABLE maybeAscii #-}

ascii ∷ Char → Word8
ascii = fromIntegral . ord
{-# INLINE ascii #-}

isControl ∷ Char → Bool
isControl c = w < 32 || w == 127
  where w = ord c
{-# INLINE isControl #-}

isPrintable ∷ Char → Bool
isPrintable c = w >= 32 && w < 127
  where w = ord c
{-# INLINE isPrintable #-}

isWhiteSpace ∷ Char → Bool
isWhiteSpace c = c == ' ' || (w >= 9 && w <= 13)
  where w = ord c
{-# INLINE isWhiteSpace #-}

isSpaceOrTab ∷ Char → Bool
isSpaceOrTab c = c == ' ' || c == '\t'
{-# INLINE isSpaceOrTab #-}

isLower ∷ Char → Bool
isLower c = c >= 'a' && c <= 'z'
{-# INLINE isLower #-}

isUpper ∷ Char → Bool
isUpper c = c >= 'A' && c <= 'Z'
{-# INLINE isUpper #-}

toLower ∷ Char → Char
toLower c | isUpper c = chr (ord c + 32)
          | otherwise = c
{-# INLINABLE toLower #-}

toUpper ∷ Char → Char
toUpper c | isLower c = chr (ord c - 32)
          | otherwise = c
{-# INLINABLE toUpper #-}

isAlpha ∷ Char → Bool
isAlpha c = isUpper c || isLower c
{-# INLINABLE isAlpha #-}

isDigit ∷ Char → Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

isNzDigit ∷ Char → Bool
isNzDigit c = c >= '1' && c <= '9'
{-# INLINE isNzDigit #-}

isAlphaNum ∷ Char → Bool
isAlphaNum c = isDigit c || isAlpha c
{-# INLINABLE isAlphaNum #-}

fromDigit ∷ Num a ⇒ Char → Maybe a
fromDigit c | isDigit c = Just $ unsafeFromDigit c
            | otherwise = Nothing
{-# INLINABLE fromDigit #-}

fromNzDigit ∷ Num a ⇒ Char → Maybe a
fromNzDigit c | isNzDigit c = Just $ unsafeFromDigit c
              | otherwise   = Nothing
{-# INLINABLE fromNzDigit #-}

unsafeFromDigit ∷ Num a ⇒ Char → a
unsafeFromDigit c = fromIntegral (ord c - ord '0')
{-# INLINE unsafeFromDigit #-}

isOctDigit ∷ Char → Bool
isOctDigit c = c >= '0' && c <= '7'
{-# INLINE isOctDigit #-}

isNzOctDigit ∷ Char → Bool
isNzOctDigit c = c >= '1' && c <= '7'
{-# INLINE isNzOctDigit #-}

fromOctDigit ∷ Num a ⇒ Char → Maybe a
fromOctDigit c | isOctDigit c = Just $ unsafeFromOctDigit c
               | otherwise    = Nothing
{-# INLINABLE fromOctDigit #-}

fromNzOctDigit ∷ Num a ⇒ Char → Maybe a
fromNzOctDigit c | isNzOctDigit c = Just $ unsafeFromOctDigit c
                 | otherwise      = Nothing
{-# INLINABLE fromNzOctDigit #-}

unsafeFromOctDigit ∷ Num a ⇒ Char → a
unsafeFromOctDigit = unsafeFromDigit
{-# INLINE unsafeFromOctDigit #-}

isLowAF ∷ Char → Bool
isLowAF c = c >= 'a' && c <= 'f'
{-# INLINE isLowAF #-}

fromLowAF ∷ Num a ⇒ Char → a
fromLowAF c = fromIntegral (ord c - ord 'a' + 10)
{-# INLINE fromLowAF #-}

isLowHexDigit ∷ Char → Bool
isLowHexDigit c = isDigit c || isLowAF c
{-# INLINABLE isLowHexDigit #-}

isNzLowHexDigit ∷ Char → Bool
isNzLowHexDigit c = isNzDigit c || isLowAF c
{-# INLINABLE isNzLowHexDigit #-}

fromLowHexDigit ∷ Num a ⇒ Char → Maybe a
fromLowHexDigit c | isDigit c = Just $ unsafeFromDigit c
                  | isLowAF c = Just $ fromLowAF c
                  | otherwise = Nothing
{-# INLINABLE fromLowHexDigit #-}

fromNzLowHexDigit ∷ Num a ⇒ Char → Maybe a
fromNzLowHexDigit c | isNzDigit c = Just $ unsafeFromDigit c
                    | isLowAF c   = Just $ fromLowAF c
                    | otherwise   = Nothing
{-# INLINABLE fromNzLowHexDigit #-}

unsafeFromLowHexDigit ∷ Num a ⇒ Char → a
unsafeFromLowHexDigit c | c < 'a'   = unsafeFromDigit c
                        | otherwise = fromLowAF c
{-# INLINE unsafeFromLowHexDigit #-}

isUpAF ∷ Char → Bool
isUpAF c = c >= 'A' && c <= 'F'
{-# INLINE isUpAF #-}

fromUpAF ∷ Num a ⇒ Char → a
fromUpAF c = fromIntegral (ord c - ord 'A' + 10)
{-# INLINE fromUpAF #-}

isUpHexDigit ∷ Char → Bool
isUpHexDigit c = isDigit c || isUpAF c
{-# INLINABLE isUpHexDigit #-}

isNzUpHexDigit ∷ Char → Bool
isNzUpHexDigit c = isNzDigit c || isUpAF c
{-# INLINABLE isNzUpHexDigit #-}

fromUpHexDigit ∷ Num a ⇒ Char → Maybe a
fromUpHexDigit c | isDigit c = Just $ unsafeFromDigit c
                 | isUpAF c  = Just $ fromUpAF c
                 | otherwise = Nothing
{-# INLINABLE fromUpHexDigit #-}

fromNzUpHexDigit ∷ Num a ⇒ Char → Maybe a
fromNzUpHexDigit c | isNzDigit c = Just $ unsafeFromDigit c
                   | isUpAF c    = Just $ fromUpAF c
                   | otherwise   = Nothing
{-# INLINABLE fromNzUpHexDigit #-}

unsafeFromUpHexDigit ∷ Num a ⇒ Char → a
unsafeFromUpHexDigit c | c < 'A'   = unsafeFromDigit c
                       | otherwise = fromUpAF c
{-# INLINE unsafeFromUpHexDigit #-}

isHexDigit ∷ Char → Bool
isHexDigit c = isDigit c || isUpAF c || isLowAF c
{-# INLINABLE isHexDigit #-}

isNzHexDigit ∷ Char → Bool
isNzHexDigit c = isNzDigit c || isUpAF c || isLowAF c
{-# INLINABLE isNzHexDigit #-}

fromHexDigit ∷ Num a ⇒ Char → Maybe a
fromHexDigit c | isDigit c = Just $ unsafeFromDigit c
               | isUpAF c  = Just $ fromUpAF c
               | isLowAF c = Just $ fromLowAF c
               | otherwise = Nothing
{-# INLINABLE fromHexDigit #-}

fromNzHexDigit ∷ Num a ⇒ Char → Maybe a
fromNzHexDigit c | isNzDigit c = Just $ unsafeFromDigit c
                 | isUpAF c    = Just $ fromUpAF c
                 | isLowAF c   = Just $ fromLowAF c
                 | otherwise   = Nothing
{-# INLINABLE fromNzHexDigit #-}

unsafeFromHexDigit ∷ Num a ⇒ Char → a
unsafeFromHexDigit c | c < 'A'   = unsafeFromDigit c
                     | c < 'a'   = fromUpAF c
                     | otherwise = fromLowAF c
{-# INLINE unsafeFromHexDigit #-}

isControl8 ∷ Word8 → Bool
isControl8 w = w < 32 || w == 127
{-# INLINE isControl8 #-}

isPrintable8 ∷ Word8 → Bool
isPrintable8 w = w >= 32 && w < 127
{-# INLINE isPrintable8 #-}

isWhiteSpace8 ∷ Word8 → Bool
isWhiteSpace8 w = w == ascii ' ' || w >= 9 && w <= 13
{-# INLINE isWhiteSpace8 #-}

isSpaceOrTab8 ∷ Word8 → Bool
isSpaceOrTab8 w = w == ascii ' ' || w == ascii '\t'
{-# INLINE isSpaceOrTab8 #-}

isLower8 ∷ Word8 → Bool
isLower8 w = w >= ascii 'a' && w <= ascii 'z'
{-# INLINE isLower8 #-}

isUpper8 ∷ Word8 → Bool
isUpper8 w = w >= ascii 'A' && w <= ascii 'Z'
{-# INLINE isUpper8 #-}

toLower8 ∷ Word8 → Word8
toLower8 w | isUpper8 w = w + 32
           | otherwise  = w
{-# INLINABLE toLower8 #-}

toUpper8 ∷ Word8 → Word8
toUpper8 w | isLower8 w = w - 32
           | otherwise  = w
{-# INLINABLE toUpper8 #-}

isAlpha8 ∷ Word8 → Bool
isAlpha8 w = isUpper8 w || isLower8 w
{-# INLINABLE isAlpha8 #-}

isDigit8 ∷ Word8 → Bool
isDigit8 w = w >= ascii '0' && w <= ascii '9'
{-# INLINE isDigit8 #-}

isNzDigit8 ∷ Word8 → Bool
isNzDigit8 w = w >= ascii '1' && w <= ascii '9'
{-# INLINE isNzDigit8 #-}

isAlphaNum8 ∷ Word8 → Bool
isAlphaNum8 w = isDigit8 w || isAlpha8 w
{-# INLINABLE isAlphaNum8 #-}

fromDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromDigit8 w | isDigit8 w = Just $ unsafeFromDigit8 w
             | otherwise  = Nothing
{-# INLINABLE fromDigit8 #-}

fromNzDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromNzDigit8 w | isNzDigit8 w = Just $ unsafeFromDigit8 w
               | otherwise    = Nothing
{-# INLINABLE fromNzDigit8 #-}

unsafeFromDigit8 ∷ Num a ⇒ Word8 → a
unsafeFromDigit8 w = fromIntegral (w - ascii '0')
{-# INLINE unsafeFromDigit8 #-}

isOctDigit8 ∷ Word8 → Bool
isOctDigit8 w = w >= ascii '0' && w <= ascii '7'
{-# INLINE isOctDigit8 #-}

isNzOctDigit8 ∷ Word8 → Bool
isNzOctDigit8 w = w >= ascii '1' && w <= ascii '7'
{-# INLINE isNzOctDigit8 #-}

fromOctDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromOctDigit8 w | isOctDigit8 w = Just $ unsafeFromOctDigit8 w
                | otherwise     = Nothing
{-# INLINABLE fromOctDigit8 #-}

fromNzOctDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromNzOctDigit8 w | isNzOctDigit8 w = Just $ unsafeFromOctDigit8 w
                  | otherwise       = Nothing
{-# INLINABLE fromNzOctDigit8 #-}

unsafeFromOctDigit8 ∷ Num a ⇒ Word8 → a
unsafeFromOctDigit8 = unsafeFromDigit8
{-# INLINE unsafeFromOctDigit8 #-}

isLowAF8 ∷ Word8 → Bool
isLowAF8 w = w >= ascii 'a' && w <= ascii 'f'
{-# INLINE isLowAF8 #-}

fromLowAF8 ∷ Num a ⇒ Word8 → a
fromLowAF8 w = fromIntegral (w - ascii 'a' + 10)
{-# INLINE fromLowAF8 #-}

isLowHexDigit8 ∷ Word8 → Bool
isLowHexDigit8 w = isDigit8 w || isLowAF8 w
{-# INLINABLE isLowHexDigit8 #-}

isNzLowHexDigit8 ∷ Word8 → Bool
isNzLowHexDigit8 w = isNzDigit8 w || isLowAF8 w
{-# INLINABLE isNzLowHexDigit8 #-}

fromLowHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromLowHexDigit8 w | isDigit8 w = Just $ unsafeFromDigit8 w
                   | isLowAF8 w = Just $ fromLowAF8 w
                   | otherwise  = Nothing
{-# INLINABLE fromLowHexDigit8 #-}

fromNzLowHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromNzLowHexDigit8 w | isNzDigit8 w = Just $ unsafeFromDigit8 w
                     | isLowAF8 w   = Just $ fromLowAF8 w
                     | otherwise    = Nothing
{-# INLINABLE fromNzLowHexDigit8 #-}

unsafeFromLowHexDigit8 ∷ Num a ⇒ Word8 → a
unsafeFromLowHexDigit8 w | w < ascii 'a' = unsafeFromDigit8 w
                         | otherwise     = fromLowAF8 w
{-# INLINE unsafeFromLowHexDigit8 #-}

isUpAF8 ∷ Word8 → Bool
isUpAF8 w = w >= ascii 'A' && w <= ascii 'F'
{-# INLINE isUpAF8 #-}

fromUpAF8 ∷ Num a ⇒ Word8 → a
fromUpAF8 w = fromIntegral (w - ascii 'A' + 10)
{-# INLINE fromUpAF8 #-}

isUpHexDigit8 ∷ Word8 → Bool
isUpHexDigit8 w = isDigit8 w || isUpAF8 w
{-# INLINABLE isUpHexDigit8 #-}

isNzUpHexDigit8 ∷ Word8 → Bool
isNzUpHexDigit8 w = isNzDigit8 w || isUpAF8 w
{-# INLINABLE isNzUpHexDigit8 #-}

fromUpHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromUpHexDigit8 w | isDigit8 w = Just $ unsafeFromDigit8 w
                  | isUpAF8 w  = Just $ fromUpAF8 w
                  | otherwise  = Nothing
{-# INLINABLE fromUpHexDigit8 #-}

fromNzUpHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromNzUpHexDigit8 w | isNzDigit8 w = Just $ unsafeFromDigit8 w
                    | isUpAF8 w    = Just $ fromUpAF8 w
                    | otherwise    = Nothing
{-# INLINABLE fromNzUpHexDigit8 #-}

unsafeFromUpHexDigit8 ∷ Num a ⇒ Word8 → a
unsafeFromUpHexDigit8 w | w < ascii 'A' = unsafeFromDigit8 w
                        | otherwise     = fromUpAF8 w
{-# INLINE unsafeFromUpHexDigit8 #-}

isHexDigit8 ∷ Word8 → Bool
isHexDigit8 w = isDigit8 w || isUpAF8 w || isLowAF8 w
{-# INLINABLE isHexDigit8 #-}

isNzHexDigit8 ∷ Word8 → Bool
isNzHexDigit8 w = isNzDigit8 w || isUpAF8 w || isLowAF8 w
{-# INLINABLE isNzHexDigit8 #-}

fromHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromHexDigit8 w | isDigit8 w = Just $ unsafeFromDigit8 w
                | isUpAF8 w  = Just $ fromUpAF8 w
                | isLowAF8 w = Just $ fromLowAF8 w
                | otherwise  = Nothing
{-# INLINABLE fromHexDigit8 #-}

fromNzHexDigit8 ∷ Num a ⇒ Word8 → Maybe a
fromNzHexDigit8 w | isNzDigit8 w = Just $ unsafeFromDigit8 w
                  | isUpAF8 w    = Just $ fromUpAF8 w
                  | isLowAF8 w   = Just $ fromLowAF8 w
                  | otherwise    = Nothing
{-# INLINABLE fromNzHexDigit8 #-}

unsafeFromHexDigit8 ∷ Num a ⇒ Word8 → a
unsafeFromHexDigit8 w | w < ascii 'A' = unsafeFromDigit8 w
                      | w < ascii 'a' = fromUpAF8 w
                      | otherwise     = fromLowAF8 w
{-# INLINE unsafeFromHexDigit8 #-}

