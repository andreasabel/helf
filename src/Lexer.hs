{-# LANGUAGE CPP #-}
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
                AlexError ((AlexPn _ line column),_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 354 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: Array Int Int
alex_base = listArray (0,178) [-8,110,-4,-3,115,-2,-1,0,-115,0,-114,-113,93,94,0,31,-85,-100,-86,-102,0,-93,-84,-83,-101,0,-91,32,-81,-79,12,0,19,20,33,34,17,128,130,44,35,47,135,139,140,141,143,144,145,146,148,40,58,46,41,0,53,66,48,49,65,157,158,72,73,62,71,74,165,167,77,63,79,67,75,80,76,82,78,81,83,90,172,180,84,91,95,182,184,85,89,96,92,103,87,104,106,194,196,107,109,105,108,122,111,200,201,125,119,136,129,227,229,132,127,137,149,131,234,240,138,155,147,246,247,150,159,160,151,161,248,249,162,154,152,166,163,258,259,153,169,264,265,164,168,179,183,170,175,171,173,272,273,174,186,176,177,275,278,181,190,0,0,0,0,0,0,0,0,262,354,446,538,630,722,814,906,998,1090]

alex_table :: Array Int Int
alex_table = listArray (0,1345) [0,1,1,1,1,1,-1,-1,-1,-1,13,13,13,17,18,19,14,22,23,20,32,29,24,27,1,178,30,178,178,4,178,178,165,166,178,178,178,170,168,178,178,178,178,178,178,178,178,178,178,178,167,178,172,173,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,163,178,164,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,177,178,178,178,178,178,178,161,178,162,178,1,1,1,1,1,5,7,5,5,5,16,8,9,25,28,33,35,34,31,-1,36,-1,40,1,139,41,-1,153,5,37,-1,-1,-1,2,-1,-1,-1,-1,52,-1,53,54,57,42,58,60,59,55,-1,-1,64,66,65,67,89,61,-1,132,-1,71,72,73,74,-1,120,75,76,77,78,81,68,-1,86,-1,79,-1,85,82,80,91,90,94,92,93,95,-1,96,-1,103,100,101,-1,-1,15,107,56,70,143,125,13,12,21,87,102,104,84,39,97,26,51,99,109,63,159,108,113,110,105,-1,10,-1,47,115,48,114,-1,116,111,50,44,117,-1,46,122,49,121,118,-1,-1,-1,-1,127,128,123,133,126,134,135,136,-1,-1,137,140,130,129,-1,-1,146,141,148,144,149,147,-1,-1,145,-1,150,155,-1,154,156,157,0,151,0,178,160,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,169,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,171,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,174,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,175,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,176,178,0,178,178,178,178,178,0,178,178,0,0,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,178,0,178,0,178,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1345) [-1,9,10,11,12,13,10,10,10,10,125,125,125,98,114,101,118,110,102,120,111,102,105,114,32,33,105,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,98,37,37,120,101,115,102,116,120,10,105,10,97,32,111,109,10,115,32,101,10,10,10,37,10,10,10,10,117,10,101,114,108,121,97,115,117,101,10,10,97,108,98,101,101,100,10,104,10,101,116,101,114,10,111,109,105,110,105,105,99,10,100,10,115,10,111,101,116,109,114,97,105,110,116,10,101,10,99,101,100,10,10,97,98,99,100,101,102,125,125,105,115,117,101,109,110,115,112,113,114,111,116,117,108,119,99,107,10,123,10,97,114,98,111,10,108,115,101,100,100,10,116,97,108,116,108,10,10,10,10,101,101,101,101,114,111,114,101,10,10,101,118,109,122,10,10,97,104,108,115,105,98,10,10,116,10,115,101,10,115,114,101,-1,116,-1,33,115,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,33,126,35,36,-1,38,39,-1,-1,42,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,-1,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,124,-1,126,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,178) [-1,-1,3,3,-1,6,6,-1,11,-1,11,11,11,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,38,38,-1,-1,-1,43,43,45,45,43,43,43,43,43,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,62,-1,-1,-1,-1,-1,69,69,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,83,83,-1,-1,-1,88,88,-1,-1,-1,-1,-1,-1,-1,-1,98,98,-1,-1,-1,-1,-1,-1,106,106,-1,-1,-1,-1,112,112,-1,-1,-1,-1,-1,119,119,-1,-1,-1,124,124,-1,-1,-1,-1,-1,131,131,-1,-1,-1,-1,-1,138,138,-1,-1,142,142,-1,-1,-1,-1,-1,-1,-1,-1,152,152,-1,-1,-1,-1,158,158,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,178) [[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_5))],[],[],[],[],[],[(AlexAcc (alex_action_6))],[],[],[],[],[(AlexAcc (alex_action_7))],[],[],[],[],[],[(AlexAcc (alex_action_8))],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_11))],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_39))]]
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
