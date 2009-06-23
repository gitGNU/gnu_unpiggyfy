#!/bin/bash

set -x

sed -e "s/^\[//g" -e "s/\]$//g" -e "s/\(Keyword \"\)/\\n\1/g" -e "s/\(Spacing \"\)/\\n\1/g" -e "s/\(VarOrFunOrConst \"\)/\\n\1/g"| sed "s/,$//g"
