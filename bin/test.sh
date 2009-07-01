#!/bin/bash

#set -x

echo "### tests begin ###"

# -----------------------------------------------------------------------------
test="LOW LEVEL TOKENIZATION"
test_01_in=data/CommentRemoval.test_input
test_01_out=tmp/CommentRemoval.test_01_output
test_01_ref=data/CommentRemoval.test_01_reference
test_01_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

bin/unpig -llt $test_01_in > $test_01_out
diff $test_01_out $test_01_ref > $test_01_diff 2>&1

if [ `wc -c $test_01_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS in diff "$test_01_out" "$test_01_ref
    head -6 tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

# -----------------------------------------------------------------------------
test="HIGH LEVEL TOKENIZATION"
test_02_in=data/CommentRemoval.test_input
test_02_out=tmp/CommentRemoval.test_02_output
test_02_ref=data/CommentRemoval.test_02_reference
test_02_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

bin/unpig -hlt $test_02_in > $test_02_out
diff $test_02_out $test_02_ref > $test_02_diff 2>&1

if [ `wc -c $test_02_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS in diff "$test_02_out" "$test_02_ref
    head -6 tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

# -----------------------------------------------------------------------------
test="CODE TOKENIZATION"
test_04_in=data/CommentRemoval.test_input
test_04_out=tmp/CommentRemoval.test_04_output
test_04_ref=data/CommentRemoval.test_04_reference
test_04_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

bin/unpig -tc $test_04_in > $test_04_out
diff $test_04_out $test_04_ref > $test_04_diff 2>&1

if [ `wc -c $test_04_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS in diff "$test_04_out" "$test_04_ref
    head -6 tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

# -----------------------------------------------------------------------------
test="REMOVING COMMENTS"
test_03_in=data/CommentRemoval.test_input
test_03_out=tmp/CommentRemoval.test_03_output
test_03_ref=data/CommentRemoval.test_03_reference
test_03_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

bin/unpig -rc $test_03_in > $test_03_out
diff $test_03_out $test_03_ref > $test_03_diff 2>&1

if [ `wc -c $test_03_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS in diff "$test_03_out" "$test_03_ref
    head -6 tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

# -----------------------------------------------------------------------------
test="CODE COMPRESSION"
test_05_in=data/CommentRemoval.test_input
test_05_out=tmp/CommentRemoval.test_05_output
test_05_ref=data/CommentRemoval.test_05_reference
test_05_diff=tmp/CommentRemoval.test_diff

echo "testing "$test"..."

bin/unpig -cc $test_05_in > $test_05_out
diff 2>&1 $test_05_out $test_05_ref > $test_05_diff 2>&1

if [ `wc -c $test_05_diff | awk '{print $1}'` -ne 0 ] ; then
    echo "###"$test" ERRORS in diff "$test_05_out" "$test_05_ref
    head -6 tmp/CommentRemoval.test_diff
    exit 1
else
    echo "OK"
fi

# -----------------------------------------------------------------------------
echo "### tests end ###"
