#!/bin/sh
paplay "$(find $HOME/.tones/ | shuf | head -n 1)"
