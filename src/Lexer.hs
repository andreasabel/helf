{-# OPTIONS -cpp #-}
{-# LINE 2 "Lexer.x" #-}


module Lexer where


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

{-# LINE 18 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, s))


{-# LINE 51 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 162 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 251 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 273 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 297 "templates/wrappers.hs" #-}

{-# LINE 322 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 354 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: Array Int Int
alex_base = listArray (0,182) [-8,110,0,115,-92,-91,-106,-90,-109,2,3,-100,-88,-89,-105,7,8,18,29,-83,-85,-99,12,16,19,15,31,30,17,126,128,42,32,43,133,135,136,138,139,141,140,143,145,37,55,44,36,149,150,53,65,46,49,64,156,157,71,72,61,70,73,164,166,76,62,78,66,74,79,75,81,77,80,82,89,171,179,83,90,92,181,185,84,88,94,91,103,86,102,93,194,195,105,107,101,111,108,96,209,211,114,112,127,123,223,225,129,132,134,137,142,231,233,131,153,144,241,243,146,154,155,147,158,244,248,160,151,152,162,159,254,255,161,169,257,261,163,165,175,176,167,168,170,172,267,270,173,182,177,174,272,274,178,186,276,279,282,284,0,180,0,183,184,258,259,0,0,0,0,0,0,0,0,343,435,527,619,711,803,895,987,1079,1171]

alex_table :: Array Int Int
alex_table = listArray (0,1426) [0,1,1,1,1,1,5,6,7,2,12,8,-1,-1,13,9,14,-1,-1,20,21,15,-1,146,1,182,-1,182,182,3,182,182,169,170,182,182,182,174,172,182,182,182,182,182,182,182,182,182,182,182,171,182,176,177,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,167,182,168,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,181,182,182,182,182,182,182,165,182,166,182,1,1,1,1,1,156,158,156,156,156,24,19,26,18,27,25,28,-1,22,-1,32,132,33,1,-1,29,-1,-1,156,-1,-1,-1,-1,154,-1,44,-1,45,34,46,-1,-1,50,51,52,53,47,-1,-1,57,59,58,60,82,54,-1,125,-1,64,65,66,67,-1,113,68,69,70,71,74,61,-1,79,-1,72,75,78,-1,73,84,83,85,87,86,88,89,-1,-1,93,94,80,97,96,90,4,100,49,63,136,118,95,-1,11,-1,101,102,77,31,103,17,43,92,98,56,152,-1,106,-1,39,110,161,40,107,-1,109,-1,42,36,108,114,41,38,115,-1,111,-1,-1,120,121,104,-1,116,119,126,127,129,-1,-1,128,-1,123,122,130,-1,139,142,140,141,134,-1,137,133,-1,138,-1,148,-1,143,-1,150,147,-1,144,149,-1,153,-1,159,160,0,0,0,0,0,0,0,0,164,0,0,164,164,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,182,0,182,182,0,182,182,164,163,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,173,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,175,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,178,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,179,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,180,182,0,182,182,182,182,182,0,182,182,0,0,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,182,0,182,0,182,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1426) [-1,9,10,11,12,13,98,98,114,118,110,101,10,10,102,120,105,10,10,102,105,120,10,115,32,33,10,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,111,101,116,114,102,115,105,10,120,10,97,111,109,32,10,101,10,10,32,10,10,10,10,37,10,117,10,101,121,114,10,10,108,97,117,115,101,10,10,97,108,98,101,101,100,10,104,10,101,116,101,114,10,111,109,105,110,105,105,99,10,100,10,115,101,111,10,116,109,114,105,97,110,116,101,10,10,101,100,115,101,99,115,97,98,99,100,101,102,117,10,105,10,108,111,109,110,99,112,113,114,107,116,117,10,119,10,97,100,123,98,111,10,108,10,101,100,114,116,108,116,97,10,108,10,10,101,101,115,10,101,114,101,111,101,10,10,114,10,109,122,101,10,97,105,98,108,104,10,115,118,10,116,10,101,10,115,10,101,115,10,116,114,10,115,10,37,37,-1,-1,-1,-1,-1,-1,-1,-1,125,-1,-1,125,125,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,33,-1,35,36,-1,38,39,125,125,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,-1,126,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,182) [-1,-1,-1,-1,-1,-1,-1,-1,-1,10,10,-1,-1,-1,-1,16,16,-1,-1,-1,-1,-1,23,23,-1,-1,-1,-1,-1,30,30,-1,-1,-1,35,35,37,37,35,35,35,35,35,-1,-1,-1,-1,48,48,-1,-1,-1,-1,-1,55,55,-1,-1,-1,-1,-1,62,62,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,76,76,-1,-1,-1,81,81,-1,-1,-1,-1,-1,-1,-1,-1,91,91,-1,-1,-1,-1,-1,-1,99,99,-1,-1,-1,-1,105,105,-1,-1,-1,-1,-1,112,112,-1,-1,-1,117,117,-1,-1,-1,-1,-1,124,124,-1,-1,-1,-1,-1,131,131,-1,-1,135,135,-1,-1,-1,-1,-1,-1,-1,-1,145,145,-1,-1,-1,-1,151,151,-1,-1,155,155,157,157,-1,162,-1,162,162,162,162,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,182) [[],[(AlexAccSkip)],[(AlexAcc (alex_action_1))],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))]]
{-# LINE 61 "Lexer.x" #-}

-- \_                              { tok (\p s -> Hole p) }

data Token = Id String AlexPosn
           | Abbrev AlexPosn
           | BrOpen AlexPosn
           | BrClose AlexPosn
           | BracketOpen AlexPosn
           | BracketClose AlexPosn
           | PrOpen AlexPosn
           | PrClose AlexPosn
           | Col AlexPosn
	   | Dot AlexPosn
           | Arrow AlexPosn
           | RevArrow AlexPosn
           | Hole AlexPosn
           | Eq AlexPosn
           | Type AlexPosn 
           | NotUsed AlexPosn -- so happy doesn't generate overlap case pattern warning
             deriving (Eq)

prettyTok :: Token -> String
prettyTok c = "\"" ++ tk ++ "\" at " ++ (prettyAlexPosn pos) where   
  (tk,pos) = showTok c

showTok c =
  case c of 
    (Id s p) -> (show s,p)
    Abbrev p -> ("%abbrev",p)
    Type p -> ("type",p)
    BrOpen p -> ("{",p)
    BrClose p -> ("}",p)
    BracketOpen p -> ("[",p)
    BracketClose p -> ("]",p)
    PrOpen p -> ("(",p)
    PrClose p -> (")",p)
    Col p -> (":",p)
    Dot p -> (".",p)
    Arrow p -> ("->",p)
    RevArrow p -> ("<-",p)
    Hole p -> ("_",p)
    Eq p -> ("=",p)
    _ -> error "not used"    

instance Show Token where
  show = fst . showTok


prettyAlexPosn (AlexPn _ line row) = "line " ++ show line ++ ", row " ++ show row

tok f p s = f p s


alex_action_1 =  tok (\p s -> Abbrev p) 
alex_action_27 =  tok (\p s -> BrOpen p) 
alex_action_28 =  tok (\p s -> BrClose p) 
alex_action_29 =  tok (\p s -> BracketOpen p) 
alex_action_30 =  tok (\p s -> BracketClose p) 
alex_action_31 =  tok (\p s -> PrOpen p) 
alex_action_32 =  tok (\p s -> PrClose p) 
alex_action_33 =  tok (\p s -> Col p) 
alex_action_34 =  tok (\p s -> Dot p) 
alex_action_35 =  tok (\p s -> Arrow p)  
alex_action_36 =  tok (\p s -> RevArrow p)  
alex_action_37 =  tok (\p s -> Eq p) 
alex_action_38 =  tok (\p s -> Type p) 
alex_action_39 =  tok (\p s -> (Id s p )) 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 37 "templates/GenericTemplate.hs" #-}

{-# LINE 47 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 89 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 100 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input'' len, _) ->



		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->



		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		(base) = alexIndexInt32OffAddr alex_base s
		((ord_c)) = ord c
		(offset) = (base + ord_c)
		(check)  = alexIndexInt16OffAddr alex_check offset
		
		(new_s) = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
