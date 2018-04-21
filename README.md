# dotfiles
Common dotfiles and a general guide to set up new environments

# Setup

# Install Terminal
* Mac - [Iterm2](https://www.iterm2.com)
* Win - [ConEmu](https://conemu.github.io/)
* Nix - [Terminator](./scripts/install_terminator.sh)
    * Install Instructions:
    ```sh
    sudo add-apt-repository ppa:gnome-terminator
    sudo apt-get update
    sudo apt-get install terminator
    ```

# Install Zsh
* Nix - [Zsh](./scripts/install_zsh_nix.sh)
    * Install Instructions:
    ```sh
    sudo apt-get update
    sudo apt-get upgrade
    sudo apt-get install zsh
    ```
* Mac - [Zsh](./scripts/install_zsh_mac.sh)
   * Install Instructions:
   ```sh
   ruby -e "$(curl -fsSL https://raw.zshhubusercontent.com/Homebrew/install/master/install)"
   brew doctor
   brew install zsh
   ```

# Install Oh my Zsh
* [Oh my Z](./scripts/install_oh_my_zsh.sh)
* Current Theme "agnoster"


# Changelog
* Added .vimrc file
* Added Terminal install instructions
