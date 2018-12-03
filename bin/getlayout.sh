#!/bin/bash

layout="$(xset -q | grep -A 0 'LED' | cut -c59-67)"

if [[ $layout =~ 1 ]]; then
    echo gr
else
    echo en
fi
