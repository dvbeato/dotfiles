#!/bin/bash

cd /usr/share/icons/Yaru

for f in $(find -name "*Nautilus*"); do
  folder_size=$(echo "$f" | sed s/\\/apps\\/org.gnome.Nautilus.\*//g)
  user_home=$folder_size/places/folder.png
  echo "copying $user_home to $f"
  sudo cp $user_home $f
done;
