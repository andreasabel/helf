{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Parser where

import qualified Lexer as T
import qualified OperatorPrecedenceParser as C
-- import qualified Common as C
import qualified Concrete as C

-- parser produced by Happy Version 1.18.5

data HappyAbsSyn 
	= HappyTerminal (T.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (C.Declarations)
	| HappyAbsSyn5 ([C.Declaration])
	| HappyAbsSyn6 (C.Declaration)
	| HappyAbsSyn7 (C.Associativity)
	| HappyAbsSyn8 (Int)
	| HappyAbsSyn11 (C.Expr)
	| HappyAbsSyn13 ([C.Expr])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (15) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (17) = happyShift action_8
action_0 (18) = happyShift action_9
action_0 (19) = happyShift action_10
action_0 (20) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 _ = happyReduce_2

action_1 (15) = happyShift action_6
action_1 (16) = happyShift action_7
action_1 (17) = happyShift action_8
action_1 (18) = happyShift action_9
action_1 (19) = happyShift action_10
action_1 (20) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (9) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (28) = happyShift action_23
action_3 _ = happyFail

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (27) = happyShift action_21
action_6 (31) = happyShift action_22
action_6 _ = happyFail

action_7 (15) = happyShift action_19
action_7 (10) = happyGoto action_20
action_7 _ = happyFail

action_8 (15) = happyShift action_19
action_8 (10) = happyGoto action_18
action_8 _ = happyFail

action_9 (15) = happyShift action_17
action_9 (7) = happyGoto action_16
action_9 _ = happyFail

action_10 (15) = happyShift action_14
action_10 (8) = happyGoto action_15
action_10 _ = happyFail

action_11 (15) = happyShift action_14
action_11 (8) = happyGoto action_13
action_11 _ = happyFail

action_12 (34) = happyAccept
action_12 _ = happyFail

action_13 (15) = happyShift action_38
action_13 _ = happyFail

action_14 _ = happyReduce_12

action_15 (15) = happyShift action_37
action_15 _ = happyFail

action_16 (15) = happyShift action_14
action_16 (8) = happyGoto action_36
action_16 _ = happyFail

action_17 _ = happyReduce_11

action_18 _ = happyReduce_7

action_19 (27) = happyShift action_35
action_19 (31) = happyShift action_22
action_19 _ = happyFail

action_20 _ = happyReduce_6

action_21 (15) = happyShift action_29
action_21 (21) = happyShift action_30
action_21 (23) = happyShift action_31
action_21 (25) = happyShift action_32
action_21 (33) = happyShift action_33
action_21 (11) = happyGoto action_34
action_21 (12) = happyGoto action_26
action_21 (13) = happyGoto action_27
action_21 (14) = happyGoto action_28
action_21 _ = happyFail

action_22 (15) = happyShift action_29
action_22 (21) = happyShift action_30
action_22 (23) = happyShift action_31
action_22 (25) = happyShift action_32
action_22 (33) = happyShift action_33
action_22 (11) = happyGoto action_25
action_22 (12) = happyGoto action_26
action_22 (13) = happyGoto action_27
action_22 (14) = happyGoto action_28
action_22 _ = happyFail

action_23 (15) = happyShift action_6
action_23 (16) = happyShift action_7
action_23 (17) = happyShift action_8
action_23 (18) = happyShift action_9
action_23 (19) = happyShift action_10
action_23 (20) = happyShift action_11
action_23 (5) = happyGoto action_24
action_23 (6) = happyGoto action_3
action_23 (9) = happyGoto action_4
action_23 (10) = happyGoto action_5
action_23 _ = happyReduce_2

action_24 _ = happyReduce_3

action_25 _ = happyReduce_15

action_26 (29) = happyShift action_46
action_26 _ = happyReduce_18

action_27 (15) = happyShift action_29
action_27 (23) = happyShift action_31
action_27 (25) = happyShift action_32
action_27 (33) = happyShift action_33
action_27 (14) = happyGoto action_45
action_27 _ = happyReduce_19

action_28 _ = happyReduce_20

action_29 _ = happyReduce_23

action_30 (15) = happyShift action_44
action_30 _ = happyFail

action_31 (15) = happyShift action_43
action_31 _ = happyFail

action_32 (15) = happyShift action_29
action_32 (21) = happyShift action_30
action_32 (23) = happyShift action_31
action_32 (25) = happyShift action_32
action_32 (33) = happyShift action_33
action_32 (11) = happyGoto action_42
action_32 (12) = happyGoto action_26
action_32 (13) = happyGoto action_27
action_32 (14) = happyGoto action_28
action_32 _ = happyFail

action_33 _ = happyReduce_22

action_34 (31) = happyShift action_41
action_34 _ = happyReduce_13

action_35 (15) = happyShift action_29
action_35 (21) = happyShift action_30
action_35 (23) = happyShift action_31
action_35 (25) = happyShift action_32
action_35 (33) = happyShift action_33
action_35 (11) = happyGoto action_40
action_35 (12) = happyGoto action_26
action_35 (13) = happyGoto action_27
action_35 (14) = happyGoto action_28
action_35 _ = happyFail

action_36 (15) = happyShift action_39
action_36 _ = happyFail

action_37 _ = happyReduce_9

action_38 _ = happyReduce_10

action_39 _ = happyReduce_8

action_40 (31) = happyShift action_41
action_40 _ = happyFail

action_41 (15) = happyShift action_29
action_41 (21) = happyShift action_30
action_41 (23) = happyShift action_31
action_41 (25) = happyShift action_32
action_41 (33) = happyShift action_33
action_41 (11) = happyGoto action_52
action_41 (12) = happyGoto action_26
action_41 (13) = happyGoto action_27
action_41 (14) = happyGoto action_28
action_41 _ = happyFail

action_42 (26) = happyShift action_51
action_42 _ = happyFail

action_43 (24) = happyShift action_49
action_43 (27) = happyShift action_50
action_43 _ = happyFail

action_44 (27) = happyShift action_48
action_44 _ = happyFail

action_45 _ = happyReduce_21

action_46 (15) = happyShift action_29
action_46 (21) = happyShift action_30
action_46 (23) = happyShift action_31
action_46 (25) = happyShift action_32
action_46 (33) = happyShift action_33
action_46 (11) = happyGoto action_47
action_46 (12) = happyGoto action_26
action_46 (13) = happyGoto action_27
action_46 (14) = happyGoto action_28
action_46 _ = happyFail

action_47 _ = happyReduce_17

action_48 (15) = happyShift action_29
action_48 (21) = happyShift action_30
action_48 (23) = happyShift action_31
action_48 (25) = happyShift action_32
action_48 (33) = happyShift action_33
action_48 (11) = happyGoto action_55
action_48 (12) = happyGoto action_26
action_48 (13) = happyGoto action_27
action_48 (14) = happyGoto action_28
action_48 _ = happyFail

action_49 (15) = happyShift action_29
action_49 (21) = happyShift action_30
action_49 (23) = happyShift action_31
action_49 (25) = happyShift action_32
action_49 (33) = happyShift action_33
action_49 (11) = happyGoto action_54
action_49 (12) = happyGoto action_26
action_49 (13) = happyGoto action_27
action_49 (14) = happyGoto action_28
action_49 _ = happyFail

action_50 (15) = happyShift action_29
action_50 (21) = happyShift action_30
action_50 (23) = happyShift action_31
action_50 (25) = happyShift action_32
action_50 (33) = happyShift action_33
action_50 (11) = happyGoto action_53
action_50 (12) = happyGoto action_26
action_50 (13) = happyGoto action_27
action_50 (14) = happyGoto action_28
action_50 _ = happyFail

action_51 _ = happyReduce_24

action_52 _ = happyReduce_14

action_53 (24) = happyShift action_57
action_53 _ = happyFail

action_54 _ = happyReduce_26

action_55 (22) = happyShift action_56
action_55 _ = happyFail

action_56 (15) = happyShift action_29
action_56 (21) = happyShift action_30
action_56 (23) = happyShift action_31
action_56 (25) = happyShift action_32
action_56 (33) = happyShift action_33
action_56 (11) = happyGoto action_59
action_56 (12) = happyGoto action_26
action_56 (13) = happyGoto action_27
action_56 (14) = happyGoto action_28
action_56 _ = happyFail

action_57 (15) = happyShift action_29
action_57 (21) = happyShift action_30
action_57 (23) = happyShift action_31
action_57 (25) = happyShift action_32
action_57 (33) = happyShift action_33
action_57 (11) = happyGoto action_58
action_57 (12) = happyGoto action_26
action_57 (13) = happyGoto action_27
action_57 (14) = happyGoto action_28
action_57 _ = happyFail

action_58 _ = happyReduce_25

action_59 _ = happyReduce_16

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (C.Declarations happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 6 happyReduction_8
happyReduction_8 ((HappyTerminal (T.Id happy_var_4 _)) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.Fixity happy_var_4 (C.Infix happy_var_3 happy_var_2)
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyTerminal (T.Id happy_var_3 _))
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.Fixity happy_var_3 (C.Prefix happy_var_2)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyTerminal (T.Id happy_var_3 _))
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.Fixity happy_var_3 (C.Postfix happy_var_2)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn7
		 (read happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn8
		 (read happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn6
		 (C.TypeSig happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_1 _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.Defn happy_var_1 (Just happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn6
		 (C.Defn happy_var_1 Nothing happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 6 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Pi happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (C.Fun happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (if length happy_var_1 == 1 then (head happy_var_1) else C.Apps (reverse happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  13 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn11
		 (C.Typ
	)

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn11
		 (C.Ident happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 6 14 happyReduction_25
happyReduction_25 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Lam happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 14 happyReduction_26
happyReduction_26 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Lam happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Id happy_dollar_dollar _ -> cont 15;
	T.Abbrev _ -> cont 16;
	T.Clause _ -> cont 17;
	T.Infix _ -> cont 18;
	T.Prefix _ -> cont 19;
	T.Postfix _ -> cont 20;
	T.BrOpen _ -> cont 21;
	T.BrClose _ -> cont 22;
	T.BracketOpen _ -> cont 23;
	T.BracketClose _ -> cont 24;
	T.PrOpen _ -> cont 25;
	T.PrClose _ -> cont 26;
	T.Col _ -> cont 27;
	T.Dot _ -> cont 28;
	T.Arrow _ -> cont 29;
	T.RevArrow _ -> cont 30;
	T.Eq _ -> cont 31;
	T.Hole _ -> cont 32;
	T.Type _ -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(T.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 310 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
