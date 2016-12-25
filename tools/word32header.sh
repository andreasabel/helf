#!/bin/sh

# concatenate Twelf input files and make word32 header for them

numbers=$HOME/helf/tools/numbers.sh
arithexpr=$HOME/helf/tools/arithexpr.sh

tmp=w32h0394875kejbdfkugh
tmp1=$tmp.1.tmp
# tmp2=$tmp.2.tmp
# tmp3=$tmp.3.tmp
# tmp4=$tmp.4.tmp

cat <<EOF
%% Twelf %word32 preamble generated automatically by word32header.sh
word32 : type.
+ : word32 -> word32 -> word32 -> type.
* : word32 -> word32 -> word32 -> type.
/ : word32 -> word32 -> word32 -> type.
prove+ : {X:word32} {Y:word32} {Z:word32} {P:+ X Y Z} type.
proof+ : {X:word32} {Y:word32} {Z:word32} {P:+ X Y Z} prove+ X Y Z P.
prove* : {X:word32} {Y:word32} {Z:word32} {P:* X Y Z} type.
proof* : {X:word32} {Y:word32} {Z:word32} {P:* X Y Z} prove* X Y Z P.
prove/ : {X:word32} {Y:word32} {Z:word32} {P:+ X Y Z} type.
proof/ : {X:word32} {Y:word32} {Z:word32} {P:+ X Y Z} prove/ X Y Z P.

%% Numbers and arithmetic
EOF
cat $* | $arithexpr > $tmp1
cat $tmp1 $* | $numbers
cat $tmp1 $*

rm -rf $tmp.*.tmp
