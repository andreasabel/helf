#!/bin/sh
# this is a filter that finds numbers
# and for each prints a Twelf line that defines this number as word32

sed=gsed

# grep option -o means print Only match
# grep option -w means only whole Words
# these options do not work together, -o is disabled by -w ??

grep -o "\<[0-9]\+\>" | sort -n | uniq | $sed "s/\([0-9]*\)/\1 : word32./"

# EOF
