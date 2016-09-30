#!/bin/bash

# 作業用の一時ファイルを作成
TMPDIR=$(mktemp -d /tmp/git-latexdiff.XXXXXX)
export this=`mktemp $TMPDIR/latex-diff.XXXXXX`
export that=`mktemp $TMPDIR/latex-diff.XXXXXX`

# gitからソースを取得
git show "$2:$1" > $this
git show "$3:$1" > $that

latexdiff -e utf8 -t CFONT "$this" "$that" > $TMPDIR/diff.tex
platex -output-directory $TMPDIR $TMPDIR/diff.tex
dvipdfmx $TMPDIR/diff.dvi
cp -rf $TMPDIR/diff.tex .

# macを使用している場合
open diff.pdf

#rm -rf $TMPDIR