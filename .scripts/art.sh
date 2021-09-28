#!/bin/bash
if [ ! -f '/tmp/OTHER*' ] 
then
  rm -r $(find /tmp/OTHER*)
else
  echo "no files created by eyed3 creating now using eyed3"  
fi
 eyeD3 --write-images=/tmp/ $HOME/Music/"$(mpc -f %file% current)" &> /dev/null
#dunstify --icon=/tmp/OTHER.jpg "$(mpc --format '%title% \n%artist% - %album%' current)"
