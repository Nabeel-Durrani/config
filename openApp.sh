#!/usr/bin/env bash
wmctrl -xa $1 && wmctrl -r $1 -b add,fullscreen || $1 && wmctrl -xa $1 && wmctrl -r $1 -b add,fullscreen
