{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Parser where

import qualified Lexer as T
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
 action_57 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_25 :: () => ({-HappyReduction (HappyIdentity) = -}
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
action_0 (4) = happyGoto action_11
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
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (9) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (27) = happyShift action_21
action_3 _ = happyFail

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (26) = happyShift action_19
action_6 (30) = happyShift action_20
action_6 _ = happyFail

action_7 (15) = happyShift action_18
action_7 (10) = happyGoto action_17
action_7 _ = happyFail

action_8 (15) = happyShift action_16
action_8 (7) = happyGoto action_15
action_8 _ = happyFail

action_9 (15) = happyShift action_13
action_9 (8) = happyGoto action_14
action_9 _ = happyFail

action_10 (15) = happyShift action_13
action_10 (8) = happyGoto action_12
action_10 _ = happyFail

action_11 (33) = happyAccept
action_11 _ = happyFail

action_12 (15) = happyShift action_36
action_12 _ = happyFail

action_13 _ = happyReduce_11

action_14 (15) = happyShift action_35
action_14 _ = happyFail

action_15 (15) = happyShift action_13
action_15 (8) = happyGoto action_34
action_15 _ = happyFail

action_16 _ = happyReduce_10

action_17 _ = happyReduce_6

action_18 (26) = happyShift action_33
action_18 (30) = happyShift action_20
action_18 _ = happyFail

action_19 (15) = happyShift action_27
action_19 (20) = happyShift action_28
action_19 (22) = happyShift action_29
action_19 (24) = happyShift action_30
action_19 (32) = happyShift action_31
action_19 (11) = happyGoto action_32
action_19 (12) = happyGoto action_24
action_19 (13) = happyGoto action_25
action_19 (14) = happyGoto action_26
action_19 _ = happyFail

action_20 (15) = happyShift action_27
action_20 (20) = happyShift action_28
action_20 (22) = happyShift action_29
action_20 (24) = happyShift action_30
action_20 (32) = happyShift action_31
action_20 (11) = happyGoto action_23
action_20 (12) = happyGoto action_24
action_20 (13) = happyGoto action_25
action_20 (14) = happyGoto action_26
action_20 _ = happyFail

action_21 (15) = happyShift action_6
action_21 (16) = happyShift action_7
action_21 (17) = happyShift action_8
action_21 (18) = happyShift action_9
action_21 (19) = happyShift action_10
action_21 (5) = happyGoto action_22
action_21 (6) = happyGoto action_3
action_21 (9) = happyGoto action_4
action_21 (10) = happyGoto action_5
action_21 _ = happyReduce_2

action_22 _ = happyReduce_3

action_23 _ = happyReduce_14

action_24 (28) = happyShift action_44
action_24 _ = happyReduce_17

action_25 (15) = happyShift action_27
action_25 (22) = happyShift action_29
action_25 (24) = happyShift action_30
action_25 (32) = happyShift action_31
action_25 (14) = happyGoto action_43
action_25 _ = happyReduce_18

action_26 _ = happyReduce_19

action_27 _ = happyReduce_22

action_28 (15) = happyShift action_42
action_28 _ = happyFail

action_29 (15) = happyShift action_41
action_29 _ = happyFail

action_30 (15) = happyShift action_27
action_30 (20) = happyShift action_28
action_30 (22) = happyShift action_29
action_30 (24) = happyShift action_30
action_30 (32) = happyShift action_31
action_30 (11) = happyGoto action_40
action_30 (12) = happyGoto action_24
action_30 (13) = happyGoto action_25
action_30 (14) = happyGoto action_26
action_30 _ = happyFail

action_31 _ = happyReduce_21

action_32 (30) = happyShift action_39
action_32 _ = happyReduce_12

action_33 (15) = happyShift action_27
action_33 (20) = happyShift action_28
action_33 (22) = happyShift action_29
action_33 (24) = happyShift action_30
action_33 (32) = happyShift action_31
action_33 (11) = happyGoto action_38
action_33 (12) = happyGoto action_24
action_33 (13) = happyGoto action_25
action_33 (14) = happyGoto action_26
action_33 _ = happyFail

action_34 (15) = happyShift action_37
action_34 _ = happyFail

action_35 _ = happyReduce_8

action_36 _ = happyReduce_9

action_37 _ = happyReduce_7

action_38 (30) = happyShift action_39
action_38 _ = happyFail

action_39 (15) = happyShift action_27
action_39 (20) = happyShift action_28
action_39 (22) = happyShift action_29
action_39 (24) = happyShift action_30
action_39 (32) = happyShift action_31
action_39 (11) = happyGoto action_50
action_39 (12) = happyGoto action_24
action_39 (13) = happyGoto action_25
action_39 (14) = happyGoto action_26
action_39 _ = happyFail

action_40 (25) = happyShift action_49
action_40 _ = happyFail

action_41 (23) = happyShift action_47
action_41 (26) = happyShift action_48
action_41 _ = happyFail

action_42 (26) = happyShift action_46
action_42 _ = happyFail

action_43 _ = happyReduce_20

action_44 (15) = happyShift action_27
action_44 (20) = happyShift action_28
action_44 (22) = happyShift action_29
action_44 (24) = happyShift action_30
action_44 (32) = happyShift action_31
action_44 (11) = happyGoto action_45
action_44 (12) = happyGoto action_24
action_44 (13) = happyGoto action_25
action_44 (14) = happyGoto action_26
action_44 _ = happyFail

action_45 _ = happyReduce_16

action_46 (15) = happyShift action_27
action_46 (20) = happyShift action_28
action_46 (22) = happyShift action_29
action_46 (24) = happyShift action_30
action_46 (32) = happyShift action_31
action_46 (11) = happyGoto action_53
action_46 (12) = happyGoto action_24
action_46 (13) = happyGoto action_25
action_46 (14) = happyGoto action_26
action_46 _ = happyFail

action_47 (15) = happyShift action_27
action_47 (20) = happyShift action_28
action_47 (22) = happyShift action_29
action_47 (24) = happyShift action_30
action_47 (32) = happyShift action_31
action_47 (11) = happyGoto action_52
action_47 (12) = happyGoto action_24
action_47 (13) = happyGoto action_25
action_47 (14) = happyGoto action_26
action_47 _ = happyFail

action_48 (15) = happyShift action_27
action_48 (20) = happyShift action_28
action_48 (22) = happyShift action_29
action_48 (24) = happyShift action_30
action_48 (32) = happyShift action_31
action_48 (11) = happyGoto action_51
action_48 (12) = happyGoto action_24
action_48 (13) = happyGoto action_25
action_48 (14) = happyGoto action_26
action_48 _ = happyFail

action_49 _ = happyReduce_23

action_50 _ = happyReduce_13

action_51 (23) = happyShift action_55
action_51 _ = happyFail

action_52 _ = happyReduce_25

action_53 (21) = happyShift action_54
action_53 _ = happyFail

action_54 (15) = happyShift action_27
action_54 (20) = happyShift action_28
action_54 (22) = happyShift action_29
action_54 (24) = happyShift action_30
action_54 (32) = happyShift action_31
action_54 (11) = happyGoto action_57
action_54 (12) = happyGoto action_24
action_54 (13) = happyGoto action_25
action_54 (14) = happyGoto action_26
action_54 _ = happyFail

action_55 (15) = happyShift action_27
action_55 (20) = happyShift action_28
action_55 (22) = happyShift action_29
action_55 (24) = happyShift action_30
action_55 (32) = happyShift action_31
action_55 (11) = happyGoto action_56
action_55 (12) = happyGoto action_24
action_55 (13) = happyGoto action_25
action_55 (14) = happyGoto action_26
action_55 _ = happyFail

action_56 _ = happyReduce_24

action_57 _ = happyReduce_15

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

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyTerminal (T.Id happy_var_4 _)) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.Fixity happy_var_4 (C.Infix happy_var_3 happy_var_2)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyTerminal (T.Id happy_var_3 _))
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.Fixity happy_var_3 (C.Prefix happy_var_2)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyTerminal (T.Id happy_var_3 _))
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.Fixity happy_var_3 (C.Postfix happy_var_2)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn7
		 (read happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn8
		 (read happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn6
		 (C.TypeSig happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_1 _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.Defn happy_var_1 (Just happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn6
		 (C.Defn happy_var_1 Nothing happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 11 happyReduction_15
happyReduction_15 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Pi happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (C.Fun happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (if length happy_var_1 == 1 then (head happy_var_1) else C.Apps (reverse happy_var_1)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn11
		 (C.Type
	)

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn11
		 (C.Ident happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 6 14 happyReduction_24
happyReduction_24 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Lam happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 14 happyReduction_25
happyReduction_25 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (C.Lam happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Id happy_dollar_dollar _ -> cont 15;
	T.Abbrev _ -> cont 16;
	T.Infix _ -> cont 17;
	T.Prefix _ -> cont 18;
	T.Postfix _ -> cont 19;
	T.BrOpen _ -> cont 20;
	T.BrClose _ -> cont 21;
	T.BracketOpen _ -> cont 22;
	T.BracketClose _ -> cont 23;
	T.PrOpen _ -> cont 24;
	T.PrClose _ -> cont 25;
	T.Col _ -> cont 26;
	T.Dot _ -> cont 27;
	T.Arrow _ -> cont 28;
	T.RevArrow _ -> cont 29;
	T.Eq _ -> cont 30;
	T.Hole _ -> cont 31;
	T.Type _ -> cont 32;
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
