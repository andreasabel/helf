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
alex_base = listArray (0,179) [-8,110,115,-4,-3,120,-2,-1,0,-115,0,-114,-113,98,99,0,36,-85,-100,-86,-102,0,-93,-84,-83,-101,0,-91,37,-81,-79,17,0,24,25,38,39,23,135,136,52,41,53,143,145,146,148,149,150,152,151,153,47,64,54,45,0,59,72,55,56,69,163,164,78,79,68,77,80,171,173,83,70,84,73,81,86,82,88,85,87,89,96,178,186,90,97,101,188,189,91,95,102,100,109,92,108,111,201,202,112,114,118,116,126,113,206,221,130,129,142,137,232,235,138,133,147,154,141,240,241,155,160,156,248,251,158,161,162,144,166,255,258,168,159,165,172,167,264,265,169,176,268,270,170,174,184,185,180,177,179,182,274,279,181,190,183,187,282,283,191,194,0,0,0,0,0,0,0,0,266,358,450,542,634,726,818,910,1002,1094]

alex_table :: Array Int Int
alex_table = listArray (0,1349) [0,2,2,2,2,2,-1,-1,-1,-1,14,14,14,18,19,20,15,23,24,21,33,30,25,28,2,179,31,179,179,5,179,179,166,167,179,179,179,171,169,179,179,179,179,179,179,179,179,179,179,179,168,179,173,174,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,164,179,165,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,178,179,179,179,179,179,179,162,179,163,179,2,2,2,2,2,2,2,2,2,2,6,8,6,6,6,17,9,10,26,29,34,36,35,2,32,37,-1,-1,2,140,41,42,154,6,-1,38,-1,-1,3,-1,-1,-1,-1,-1,-1,53,54,43,58,55,59,56,61,60,-1,-1,65,67,66,68,90,62,-1,133,-1,72,74,73,75,-1,121,76,77,78,79,82,69,-1,87,-1,-1,80,86,83,81,92,91,95,93,96,97,94,-1,-1,101,102,104,-1,16,108,57,71,144,126,14,13,22,88,105,98,85,40,-1,27,52,100,103,64,160,109,114,110,111,-1,11,106,-1,48,116,49,115,-1,-1,51,45,118,117,112,123,-1,47,50,-1,128,129,119,-1,130,124,-1,134,135,122,127,137,-1,-1,131,138,-1,136,-1,147,150,148,-1,145,142,141,149,-1,146,156,-1,-1,151,158,155,157,0,179,0,179,179,152,179,179,161,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,170,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,172,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,175,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,176,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,177,179,0,179,179,179,179,179,0,179,179,0,0,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,0,179,0,179,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1349) [-1,9,10,11,12,13,10,10,10,10,125,125,125,98,114,101,118,110,102,120,111,102,105,114,32,33,105,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,9,10,11,12,13,98,37,37,120,101,115,102,116,32,120,105,10,10,32,111,97,109,115,32,10,101,10,10,37,10,10,10,10,10,10,117,101,121,108,114,97,101,115,117,10,10,97,108,98,101,101,100,10,104,10,101,101,116,114,10,111,109,105,110,105,105,99,10,100,10,10,115,111,101,116,109,114,97,105,116,101,110,10,10,101,100,99,10,97,98,99,100,101,102,125,125,105,115,101,115,109,110,10,112,113,114,117,116,117,108,119,111,99,10,123,107,10,97,114,98,111,10,10,101,100,100,108,115,97,10,116,108,10,101,101,108,10,122,101,10,101,111,116,114,101,10,10,109,101,10,114,10,97,105,98,10,115,104,118,108,10,116,101,10,10,115,101,115,114,-1,33,-1,35,36,116,38,39,115,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,-1,126,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,179) [-1,-1,-1,4,4,-1,7,7,-1,12,-1,12,12,12,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,39,-1,-1,-1,44,44,46,46,44,44,44,44,44,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,63,63,-1,-1,-1,-1,-1,70,70,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,84,84,-1,-1,-1,89,89,-1,-1,-1,-1,-1,-1,-1,-1,99,99,-1,-1,-1,-1,-1,-1,107,107,-1,-1,-1,-1,113,113,-1,-1,-1,-1,-1,120,120,-1,-1,-1,125,125,-1,-1,-1,-1,-1,132,132,-1,-1,-1,-1,-1,139,139,-1,-1,143,143,-1,-1,-1,-1,-1,-1,-1,-1,153,153,-1,-1,-1,-1,159,159,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,179) [[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_5))],[],[],[],[],[],[(AlexAcc (alex_action_6))],[],[],[],[],[(AlexAcc (alex_action_7))],[],[],[],[],[],[(AlexAcc (alex_action_8))],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_11))],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))]]
{-# LINE 61 "Lexer.x" #-}

-- \_                              { tok (\p s -> Hole p) }

data Token = Id String AlexPosn
           | Abbrev AlexPosn
           | Clause AlexPosn
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
    Clause p -> ("%clause",p)
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
alex_action_11 =  tok (\p s -> Clause p) 
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
