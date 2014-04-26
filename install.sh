#!/bin/bash

mkdir ~/.emacs.d

ln -s $(pwd)/init.el ~/.emacs
ln -s $(pwd)/*-cfg.el ~/.emacs.d/
ln -s $(pwd)/plugins ~/.emacs.d/
ln -s $(pwd)/templates ~/.emacs.d/
ln -s $(pwd)/ac-dict ~/.emacs.d/

