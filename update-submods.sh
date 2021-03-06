#!/bin/bash
git submodule foreach "pwd" |
    grep -v "^Entering" |
    xargs -n 1 -I {} -P 10 bash -c "cd {}; git checkout master; git fetch; git merge --ff-only origin/master"
