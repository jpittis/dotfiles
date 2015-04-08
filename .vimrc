set nocompatible " use more than vi

" start Vundle setup
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/Vundle.vim'

" bottom bar customizing
Plugin 'bling/vim-airline'

" molokai theme
Plugin 'tomasr/molokai'

" syntax checking
Plugin 'scrooloose/syntastic'

" end Vundle setup
call vundle#end()
filetype plugin indent on

" general configuration
set number
syntax on
set hlsearch

" tab configuration should be language specific
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent

" language specific tabs
autocmd FileType html       setl sw=2 sts=2 et
autocmd FileType ruby       setl sw=2 sts=2 et

" monokai
let g:rehash256 = 1
colorscheme molokai

" default syntax checking
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" airline
set laststatus=2
