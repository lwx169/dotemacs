#!/bin/bash

dot_emacs_path=~/.emacs
dot_emacs_path_backup=~/.emacs.bkp
dot_emacsd_path=~/.emacs.d
dot_emacsd_path_backup=~/.emacs.d.bkp

# back up .emacs
if [ -f $dot_emacs_path ]
then
	echo "Backup old .emacs to .emacs.bkp"
	mv -fv $dot_emacs_path $dot_emacs_path_backup
fi

# back up .emacs.d
if [ -d $dot_emacsd_path ]
then
	echo "Backup old .emacs.d to .emacs.d.bkp"
	mv -fv $dot_emacsd_path $dot_emacsd_path_backup
fi

# create .emacs.d directory
echo "Create .emacs.d directory" 
mkdir ~/.emacs.d

# link files to home directory
echo "Set config file and plugins"
ln -s $(pwd)/init.el ~/.emacs
ln -s $(pwd)/lisp ~/.emacs.d/
ln -s $(pwd)/plugins ~/.emacs.d/
ln -s $(pwd)/templates ~/.emacs.d/
ln -s $(pwd)/ac-dict ~/.emacs.d/

echo "Configure Emacs seccess!"
