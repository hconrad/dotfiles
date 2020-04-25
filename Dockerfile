FROM alpine as init
RUN apk update && apk add vim curl git zsh
WORKDIR /repos
RUN git clone https://github.com/hconrad/dotfiles.git
WORKDIR /repos/dotfiles/scripts
RUN ./install_oh_my_zsh.sh && rm ~/.zshrc
RUN ./install_dotfiles.sh
WORKDIR /root
CMD ["zsh"]

