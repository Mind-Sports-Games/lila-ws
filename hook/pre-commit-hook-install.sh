#!/bin/sh
#create the file if not exist
touch ../.git/hooks/pre-commit

#delete the file
rm ../.git/hooks/pre-commit

#create a file link
ln -s pre-commit-hook.sh ../.git/hooks/pre-commit

#copy paste the code in the hook folder
cp pre-commit-hook.sh ../.git/hooks/pre-commit-hook.sh
