" Vim Configuration  
" =================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Plugins
" =======
Plugin 'VundleVim/Vundle.vim'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'altercation/vim-colors-solarized'
Plugin 'scrooloose/nerdtree'
Plugin 'git://git.wincent.com/command-t.git'
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'pangloss/vim-javascript'
Plugin 'Chiel92/vim-autoformat'
Plugin 'iCyMind/NeoSolarized'

call vundle#end()            " required
filetype plugin indent on    " required
set background=dark
colorscheme NeoSolarized
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

	  
