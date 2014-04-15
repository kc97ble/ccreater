#!/bin/bash

Executable=ccreater
Folder=$RANDOM$RANDOM
echo Packing $Executable project into packed/$Folder/
sleep 1s
mkdir -p packed/$Folder/
cp -avr *.lpi *.lpr *.ico *.pas *.lfm packed/$Folder/
cp -avr components packed/$Folder/
cp -avr languages packed/$Folder/
strip $Executable
cp $Executable packed/$Folder/
zip -r packed/$Folder'.zip' packed/$Folder/
sleep 1s

