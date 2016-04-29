" -------------------- vim-plug plugins --------------------

call plug#begin('~/.config/nvim/plugged')

Plug 'benekastah/neomake'
Plug 'altercation/vim-colors-solarized'
Plug 'janko-m/vim-test'

call plug#end()

" -------------------- neovim defaults ---------------------

" Leader key.
let mapleader = ','

" Exit back to normal mode in terminal with escape.
tnoremap <Esc> <C-\><C-n>

" Line numbers.
set number

" Clearer cursor.
set cursorline

" Highlight while searching and clear search with <C-L>.
set hlsearch
nnoremap <C-L> :nohl<CR><C-L>

" Flash open paren upon typing close.
set showmatch

" Keep 5 lines of context when scrolling.
set scrolloff=5

" Solarized
set background=dark
colorscheme solarized

" -------------------- neomake -----------------------------

" Run upon write.
autocmd! BufWritePost * NeomakeFile

" Open the error window upon error.
let g:neomake_open_list = 2

" Set warning and error gutter signs.
let g:neomake_warning_sign = { 'text': 'W', 'texthl': 'WarningMsg' }
let g:neomake_error_sign = { 'text': 'E', 'texthl': 'ErrorMsg' }

" -------------------- vim-test ----------------------------

" Shortcuts.
nmap <leader>t :TestNearest<CR>
nmap <leader>T :TestFile<CR>
