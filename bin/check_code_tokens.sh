#!/bin/bash

set -x

sed "s/^\[//g" | sed "s/\]$//g" | sed "s/\(Keyword \"\)/\\n\1/g" | sed "s/\(Spacing \"\)/\\n\1/g" | sed "s/\(VarOrFunOrConst \"\)/\\n\1/g" | sed "s/,$//g"
