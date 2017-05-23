#!/bin/bash

setxkbmap -option ctrl:nocaps \
          -option grp:alt_space_toggle && \
    xcape -e 'Control_L=Escape' -t 200
