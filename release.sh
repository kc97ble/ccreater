#!/bin/bash

Executable=ccreater
Folder=release/$RANDOM$RANDOM

echo Releasing $Executable project into $Folder
mkdir -p $Folder/
strip $Executable
cp $Executable $Folder/
cp -avr languages $Folder/
zip -r $Folder'.zip' $Folder
echo Released successfully into $Folder

