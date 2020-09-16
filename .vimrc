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
Plug 'scrooloose/nerdtree'
Plug 'git://git.wincent.com/command-t.git'
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'pangloss/vim-javascript'
Plug 'Chiel92/vim-autoformat'
Plug 'tpope/vim-surround'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'christoomey/vim-tmux-navigator'
Plug 'w0rp/ale'
Plug 'tpope/vim-fireplace'
Plug 'guns/vim-clojure-static'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'vim-scripts/paredit.vim'

call plug#end()            " required

filetype plugin indent on    " required

" ====================
" W0rp Ale
" ====================
let g:ale_linters = { 
\ 'javascript': ['eslint'], 
\}
let g:ale_javascript_eslint_executable = 'eslint'
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
autocmd FileType lisp,clojure,scheme RainbowParentheses
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
"Recusrive search
set path+=**
set wildignore+=**/node_modules/**
"Display all matching files when we tab complete
set wildmenu
set number
set relativenumber
set undofile
set undodir=~/.vim/undodir
set nocompatible              " be iMproved, required
syntax on 

	  
