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
{-# LINE 1 "<command-line>" #-}
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
alex_base = listArray (0,180) [-8,110,115,-4,-3,120,-2,-1,0,-115,0,-114,-113,98,99,0,36,-85,-100,-86,-102,0,-93,-84,-83,-101,0,-91,37,-81,-79,17,0,24,25,38,39,23,135,136,52,41,53,143,145,146,148,149,150,152,151,153,47,64,54,45,157,159,62,74,55,58,73,165,166,80,81,70,79,82,173,175,85,71,87,75,83,88,84,90,86,89,91,100,180,187,92,102,97,190,194,93,101,103,96,112,95,111,113,203,204,114,116,109,128,130,123,225,230,133,131,155,137,235,237,138,141,142,156,147,241,247,154,161,158,251,253,160,163,164,162,167,257,259,170,168,169,171,172,263,265,174,176,266,268,178,179,183,184,177,181,182,185,277,278,186,189,188,191,281,284,193,195,0,0,0,0,0,0,0,0,276,368,460,552,644,736,828,920,1012,1104]

alex_table :: Array Int Int
alex_table = listArray (0,1359) [0,2,2,2,2,2,-1,-1,-1,-1,14,14,14,18,19,20,15,23,24,21,33,30,25,28,2,180,31,180,180,5,180,180,167,168,180,180,180,172,170,180,180,180,180,180,180,180,180,180,180,180,169,180,174,175,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,165,180,166,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,179,180,180,180,180,180,180,163,180,164,180,2,2,2,2,2,2,2,2,2,2,6,8,6,6,6,17,9,10,26,29,34,36,35,2,32,37,-1,-1,2,141,41,42,155,6,-1,38,-1,-1,3,-1,-1,-1,-1,-1,-1,53,54,43,-1,55,-1,59,60,61,62,56,-1,-1,66,68,67,69,91,63,-1,134,-1,73,74,75,76,-1,122,77,78,79,80,83,-1,84,70,-1,81,88,87,-1,82,95,92,94,96,93,97,98,-1,-1,102,103,16,109,58,72,145,127,14,13,22,104,105,89,86,40,106,27,52,101,-1,65,161,99,115,-1,110,111,11,107,-1,48,-1,49,116,118,-1,51,45,112,117,119,-1,124,47,50,-1,113,-1,129,130,120,-1,125,-1,123,135,138,-1,128,-1,-1,139,-1,136,148,132,149,137,131,150,151,-1,-1,143,157,-1,142,146,-1,147,159,152,0,0,0,156,158,0,0,0,0,153,162,180,0,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,171,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,173,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,176,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,177,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,178,180,0,180,180,180,180,180,0,180,180,0,0,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,0,180,0,180,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1359) [-1,9,10,11,12,13,10,10,10,10,125,125,125,98,114,101,118,110,102,120,111,102,105,114,32,33,105,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,9,10,11,12,13,98,37,37,120,101,115,102,116,32,120,105,10,10,32,111,97,109,115,32,10,101,10,10,37,10,10,10,10,10,10,117,101,121,10,114,10,108,97,117,115,101,10,10,97,108,98,101,101,100,10,104,10,101,116,101,114,10,111,109,105,110,105,105,10,101,99,10,115,100,111,10,116,110,114,105,97,109,116,101,10,10,101,100,97,98,99,100,101,102,125,125,105,117,99,115,109,110,101,112,113,114,10,116,117,115,119,10,108,111,123,107,10,97,10,98,111,108,10,101,100,99,114,100,10,97,116,108,10,115,10,101,101,108,10,101,10,116,101,101,10,114,10,10,101,10,111,97,109,98,114,122,108,105,10,10,104,101,10,118,115,10,116,101,115,-1,-1,-1,115,114,-1,-1,-1,-1,116,115,33,-1,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,-1,126,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,180) [-1,-1,-1,4,4,-1,7,7,-1,12,-1,12,12,12,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,39,-1,-1,-1,44,44,46,46,44,44,44,44,44,-1,-1,-1,-1,57,57,-1,-1,-1,-1,-1,64,64,-1,-1,-1,-1,-1,71,71,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,85,85,-1,-1,-1,90,90,-1,-1,-1,-1,-1,-1,-1,-1,100,100,-1,-1,-1,-1,-1,-1,108,108,-1,-1,-1,-1,114,114,-1,-1,-1,-1,-1,121,121,-1,-1,-1,126,126,-1,-1,-1,-1,-1,133,133,-1,-1,-1,-1,-1,140,140,-1,-1,144,144,-1,-1,-1,-1,-1,-1,-1,-1,154,154,-1,-1,-1,-1,160,160,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,180) [[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_5))],[],[],[],[],[],[(AlexAcc (alex_action_6))],[],[],[],[],[(AlexAcc (alex_action_7))],[],[],[],[],[],[(AlexAcc (alex_action_8))],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))]]
{-# LINE 61 "Lexer.x" #-}

-- \_                              { tok (\p s -> Hole p) }

data Token = Id String AlexPosn
           | Abbrev AlexPosn
           | Infix AlexPosn
           | Prefix AlexPosn
           | Postfix AlexPosn
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
    Infix p -> ("%infix",p)
    Prefix p -> ("%prefix",p)
    Postfix p -> ("%postfix",p)
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


alex_action_5 =  tok (\p s -> Abbrev p) 
alex_action_6 =  tok (\p s -> Infix p) 
alex_action_7 =  tok (\p s -> Prefix p) 
alex_action_8 =  tok (\p s -> Postfix p) 
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
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}

{-# LINE 66 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 87 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 98 "templates/GenericTemplate.hs" #-}
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

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


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
		base   = alexIndexInt32OffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
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
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
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
