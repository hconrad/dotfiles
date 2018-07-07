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

# Install Neo Vim
* Mac - [NeoVim](./scripts/install_neovim_mac.sh) 
    ```sh
    brew install neovim
    ```
* Win - [NeoVim](./scripts/install_neovim_win.sh)
    ```sh
    choco install neovim
    ```
* Nix - [NeoVim](./scripts/install_neovim_nix.sh)
   ```sh
   sudo apt-get install neovim
   ```
# Install ack 
* Mac
   ```sh
   brew install ack 
   ```
* Win 
   ```sh
   choco install ack 
   ```
* Nix
  ```sh
  sudo cpan install App:Ack
  ```


# Install [WebStorm](https://jetbrains.com)
* Plugin Material Theme UI
* Plugin IdeaVim

# Oh my Zsh theme
[Bullet Train](https://github.com/caiogondim/bullet-train.zsh)

# Vim
Once everything is installed run 
```sh
scripts/install_vimrc.sh
```
This will create a symbolic link to this .vimrc and setup vim for the next load


