#!/bin/sh

#set -x

echo "### tests begin ###"

test="low level tokenization"
test_01_in=data/CommentRemoval.test_input
test_01_out=tmp/CommentRemoval.test_output
test_01_ref=data/CommentRemoval.test_reference
test_01_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

unpig -llt $test_01_in > $test_01_out
diff $test_01_out $test_01_ref > $test_01_diff

if [ `wc -c $test_01_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS:"
    cat tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

echo "### tests end ###"
