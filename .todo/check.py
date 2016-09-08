#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import os
import argparse


HOME = os.path.expanduser('~')
TODO_DIR = os.path.join(HOME, '.todo')
EXT = '.todo'

ZERO_C = 'green'
OK_C = 'yellow'
WARN_C = 'colour210'
CLR_TMPL = '#[fg={}]'
CLR_DEF = '#[default]'
# SYMBOL = '☑'
SYMBOL = '✓'


def count_files(folder, ext):
    files = os.listdir(folder)
    files = filter(lambda f: os.path.isfile(os.path.join(folder, f)), files)
    files = filter(lambda f: f.endswith(ext), files)
    try:
        return len(files)
    except TypeError:
        return len(list(files))


def display(count, tmux):
    if tmux:
        if count > 5:
            color = WARN_C
        elif count > 0:
            color = OK_C
        else:
            color = ZERO_C
        parts = {
            'symbol': SYMBOL,
            'color': CLR_TMPL.format(color),
            'count': count,
            'default': CLR_DEF
        }
        return '{color}{symbol} {count}{default}'.format(**parts)
    else:
        return count




if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-t', '--tmux',
        dest='tmux',
        action='store_true',
        default=False
    )

    args = parser.parse_args()

    count = count_files(TODO_DIR, EXT)
    print(display(count, args.tmux))
