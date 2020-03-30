#!/usr/bin/env sh

echo "Install needed dependancies"
make
touch debug
echo "Open Emacs"
emacs -Q --debug-init -l test-profile.el example.org
