" Vim Configuration  
" =================
" Install Vim - Plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif



" Plugins
" =======
call plug#begin('~/.vim/plugged')
Plug 'VundleVim/Vundle.vim'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'altercation/vim-colors-solarized'
Plug 'scrooloose/nerdtree'
Plug 'git://git.wincent.com/command-t.git'
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'pangloss/vim-javascript'
Plug 'Chiel92/vim-autoformat'
Plug 'tpope/vim-surround'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'christoomey/vim-tmux-navigator'
call plug#end()            " required

filetype plugin indent on    " required
set background=dark
colorscheme solarized
set termguicolors

" ====================
" Airline
" ====================
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_powerline_fonts = 1
let g:airline_theme='deus'

augroup config_group
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType sh setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType html setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType css setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType scss setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
augroup END

" ============
" Vim Mappings
" ============
:nmap ff :Autoformat <CR>
:nmap gd :Gdiff <CR>
map <C-n> :NERDTreeToggle<CR>
nmap <C-h> <C-W>h<C-W>_
nmap <C-l> <C-W>l<C-W>_

" ==========
" VIM General
" ==========
set number
set relativenumber
set undofile
set undodir=~/.vim/undodir
set nocompatible              " be iMproved, required
filetype off                  " required
syntax enable  

	  
