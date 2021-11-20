#!/usr/bin/env bash
wmctrl -xa emacs && wmctrl -r emacs -b add,fullscreen || emacsclient -c && wmctrl -r emacs -b add,fullscreen
