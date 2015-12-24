#!/bin/bash

for i in `find . -name "*.imageset"`; do
    file=`basename -s .imageset "$i"`
    result=`ack -i "$file" --ignore-dir="*.xcassets"`
    if [ -z "$result" ]; then
        echo "$i"
    fi
done

# Ex: to remove from git
# # for i in `./script/unused_images.sh`; do git rm "$i"; done
