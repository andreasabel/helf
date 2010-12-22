#!/bin/sh
# this is a filter that finds expressions of the form n+m n*m n/m
# and for each print a Twelf %word32 line that defines a 
# constant representing this computation

sed=gsed

# grep option -o means print Only match
# grep option -w means only whole Words
# these options do not work together, -o is disabled by -w ??

grep -o "\<[0-9]\+\(+\|*\|/\)[0-9]\+\>" | sort | uniq | while read line; do echo "$line" $(( $line )) "."; done | $sed "s/\([0-9]*\)\(+\|*\|\/\)\([0-9]*\)/\1\2\3 : \2 \1 \3/"

exit 0

# explanation of filters
# 1 find addition, multiplication, and division expressions
# 2 sort, remove duplicates
# 3 add result of expression and "." to each line
# 4 transform into Twelf arithmetic syntax

## testing
cat <<EOF | grep -o "\<[0-9]\+\(+\|*\|/\)[0-9]\+\>" | sort | uniq | while read line; do echo "$line" $(( $line )) "."; done | $sed "s/\([0-9]*\)\(+\|*\|\/\)\([0-9]*\)/\1\2\3 : \2 \1 \3/"
sepp = 4+5 
0+1 
+1 
a+b 
a*b 
345*5
0/1
4*455a
5/3
6/4 bledl
EOF

# echo <<EOF ... EOF does not work, since echo does not read from stdin

# remove blank lines = $sed '/^$/d' | 

# EOF
