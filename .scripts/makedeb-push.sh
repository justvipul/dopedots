#!/bin/bash
makedeb --printsrcinfo > .SRCINFO
git add .
git config user.name "only_vip"
git config user.email "onlyme_vip@protonmail.com"
git commit -m "version update"
#git commit -m "upstream change fix"
#git commit -m "simplified the echo commands and creation of certain directories thus"
#git commit -m "initial commit"
#git commit -m "fixed a naming issue for bat with debian upstream and another package having the same name"
git push
