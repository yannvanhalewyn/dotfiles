"/* GENERAL BEHAVIOR
"============================ */

:au FocusLost * :wa " Save on focus lost
set backspace=2     " Backspace deletes like most programs in insert mode
set nocompatible    " Use Vim settings, rather then Vi settings
set noswapfile      " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set showcmd         " display incomplete commands
set lazyredraw      " don't redraw when don't have to"
set laststatus=2    " Always display the status line (Arline bottom bar!)
set autowrite       " Automatically :write before running commands
filetype plugin indent on

" Tabsize
set tabstop=2
set shiftwidth=2
set expandtab

" Persistent undo
set undodir=~/.vim/undo/
set undofile
set undolevels=1000
set undoreload=10000

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" When editing a file, always jump to the last known cursor position.
" Don't do it for commit messages, when the position is invalid, or when
" inside an event handler (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif


"/* KEY MAPPINGS
"============================ */

" Remap tab and shift-tab to switch buffers
:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>

let mapleader = "\<Space>"          " set leader key
" close buffer
map <Leader>q :bd<CR>
map <Leader>n :NERDTreeToggle<CR>   " toggle NerdTree
nnoremap <leader><leader> <c-^>     " Switch between the last two files
nnoremap <CR> o<esc>k               " Remap ENTER and SHIFT-ENTER to append or prepend newlines in normal mode

" Spec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>


"/* LAYOUT
"============================ */

" Color scheme
" set background=dark
syntax on
colorscheme jellybeans
set encoding=utf-8

" Highlight line number of where cursor currently is
" hi CursorLineNr guifg=#050505
" hi Comment ctermfg=8

" highlight vertical column of cursor
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline 
set cursorline 

" Line numbers
set number
set numberwidth=5

" Display extra whitespace
set list listchars=tab:»·,trail:·

" Remove trailing whitespace on save for ruby files.
au BufWritePre *.rb :%s/\s\+$//e


"/* AIRLINE
"============================ */

set guifont=Liberation\ Mono\ for\ Powerline\ 12

" Colors and font
let g:Powerline_symbols = 'fancy'
set encoding=utf-8
set t_Co=256
set fillchars+=stl:\ ,stlnc:\
set term=xterm-256color
set termencoding=utf-8
let g:airline_powerline_fonts = 1

" The symbols
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

:set smartcase
:set ignorecase
:set noantialias



"/* VUNDLE
"============================ */

filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
" Emmet
Plugin 'mattn/emmet-vim'
" CTRL-P
Plugin 'kien/ctrlp.vim'
" Airline status bar
Plugin 'bling/vim-airline'
" NerdTree
Plugin 'scrooloose/nerdtree'
" Endwise (Ruby)
Plugin 'tpope/vim-endwise'
" DelimitMate
Plugin 'Raimondi/delimitMate'
" vim-rspec
Plugin 'thoughtbot/vim-rspec'
" RSPEC synthax higlighting
Plugin 'Keithbsmiley/rspec.vim'
" UltiSnip
Plugin 'SirVer/ultisnips'
" Otherwise it interferes with my tab completion..
let g:UltiSnipsExpandTrigger="<S-tab>"

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


"/* PLUGIN SPECIFIC CONFIG
"============================ */

" EMMET
autocmd FileType html,css EmmetInstall    " Use only with certain files
let g:user_emmet_expandabbr_key = '<c-e>' " Use the ctrl-e key to expand

" NERDTREE
let NERDTreeShowHidden=1
