#!/bin/bash

#set -x

# usage: cat [FILE] | check_code_tokens.sh

sed -e "s/^\[//g" \
    -e "s/\]$//g" \
    -e "s/\(\(Special\)*Keyword \"\)/\\n\1/g" \
    -e "s/\(Spacing \"\)/\\n\1/g" \
    -e "s/\(VarOrFunOrConst \"\)/\\n\1/g" \
    -e "s/\(ShortComment \"\)/\\n\1/g" \
    -e "s/\(LongComment \"\)/\\n\1/g" | sed "s/,$//g"
