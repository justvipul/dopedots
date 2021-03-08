#!/bin/sh
paplay "$(find /home/mxtest/.tones/ | shuf | head -n 1)"
