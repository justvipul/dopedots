#!/bin/bash
rm /tmp/OTHER*;
eyeD3 --write-images=/tmp/ $HOME/Music/$(mpc -f %file% current) &> /dev/null
dunstify --icon=/tmp/OTHER.jpg "$(mpc --format '%title% \n%artist% - %album%' current)"
