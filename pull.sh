#!/bin/bash

rm -Rf .emacs.d

cp ~/.emacs .
mkdir -p .emacs.d
cp ~/.emacs.d/rc-*.el .emacs.d/

mkdir -p .emacs.d/snippets
cp -R ~/.emacs.d/snippets/* .emacs.d/snippets/

mkdir -p .emacs.d/elisp
cp -R ~/.emacs.d/elisp/* .emacs.d/elisp/
