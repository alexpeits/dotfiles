#!/bin/bash

bl=$(xbacklight -get | sed 's/\..*$//')

echo "$bl%"
