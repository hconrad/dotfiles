#/bin/sh

ln -s `pwd`/../.vimrc ~/.vimrc
ln -s `pwd`/../.vimrc ~/.ideavimrc
ln -s `pwd`/../.zshrc ~/.zshrc
mkdir -p ~/.config/kitty && ln -s `pwd`/../kitty/kitty.conf ~/.config/kitty/kitty.conf
