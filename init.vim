" -------------------- vim-plug plugins --------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'benekastah/neomake'
" Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'altercation/vim-colors-solarized'
Plug 'janko-m/vim-test'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'rhysd/vim-crystal'

call plug#end()

" -------------------- neovim defaults ---------------------
" Leader key.
let mapleader = ','

" Exit back to normal mode in terminal with escape.
tnoremap <Esc> <C-\><C-n>

" Open terminal in right window with <leader>m.
nnoremap <Leader>m :vsp term:///bin/bash<CR>:startinsert<CR>

" Line numbers.
set number
set ruler

" Stop beeping!
set noerrorbells

" By default, use two spaces.
set expandtab
set tabstop=2
set shiftwidth=2

" Clearer cursor.
set cursorline

" Highlight while searching and clear search with <C-L>.
set hlsearch
nnoremap <C-L> :nohl<CR><C-L>

" Reasonable searching.
set ignorecase
set smartcase
set incsearch

" Flash open paren upon typing close.
set showmatch

" Keep 5 lines of context when scrolling.
set scrolloff=5

" Solarized
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set background=dark
colorscheme solarized
set t_Co=256

" Natural splitting.
set splitbelow
set splitright

" Toggle relative numbering.
function! NumberToggle()
  if(&relativenumber == 1)
    set nornu
    set number
  else
    set rnu
  endif
endfunc

" Start in relative mode and toggle when using insert mode.
call NumberToggle()
autocmd InsertEnter * :call NumberToggle()
autocmd InsertLeave * :call NumberToggle()

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

" -------------------- fzf ---------------------------------
nnoremap <Leader>o :FZF<CR>
