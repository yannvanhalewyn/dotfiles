" ================
" GENERAL BEHAVIOR
" ================

set relativenumber
set nocompatible
syntax on

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

" Remove trailing whitespace on save for all files.
au BufWritePre * :%s/\s\+$//e

" When editing a file, always jump to the last known cursor position.
" Don't do it for commit messages, when the position is invalid, or when
" inside an event handler (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif


"/* MAPPINGS
"============================ */

" Remap tab and shift-tab to switch buffers
:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>

" set leader key
let mapleader = "\<Space>"

" close buffer using a script in ~/.vim/plugin/BufOnly.vim
" It swaps it with the previouse buffer, or an empty one if needed.
map <Leader>q :Bclose<CR>
map <Leader>Q :Bonly<CR>

" Moving lines/selection up and down - direct map for vim-pasta
nmap <UP> ddkP
nmap <DOWN> ddp
nnoremap <LEFT> <<
nnoremap <RIGHT> >>
vmap <UP> dkPV`>kk
vmap <DOWN> <esc>jjmm`<V`>dpV`mk
vnoremap <LEFT> <V`>
vnoremap <RIGHT> >V`>

" Auto center!
nmap G Gzz
nmap n nzz
nmap N Nzz
nmap { {zz
nmap } }zz

"Edit vimrc in split/source vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap<leader>sv :source $MYVIMRC<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l


"/* VUNDLE
"============================ */

filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.nvim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
" Emmet
" Plugin 'mattn/emmet-vim'
" CTRL-P
Plugin 'kien/ctrlp.vim'
" Airline status bar
Plugin 'bling/vim-airline'
" Git wrapper/airline branch display
" Plugin 'tpope/vim-fugitive'
" NerdTree
" Plugin 'scrooloose/nerdtree'
" Endwise (Ruby)
" Plugin 'tpope/vim-endwise'
" DelimitMate
" Plugin 'Raimondi/delimitMate'
" vim-rspec
" Plugin 'thoughtbot/vim-rspec'
" To send commands to TMUX (RSpec!!)
" Plugin 'benmills/vimux'
" RSPEC synthax higlighting
" Plugin 'Keithbsmiley/rspec.vim'
" Rails.vim
" Plugin 'tpope/vim-rails'
" Some snippets
" Plugin 'honza/vim-snippets'
" UltiSnips
" Plugin 'SirVer/ultisnips'
" Supertab so that ultisnips and completions play nice
" Plugin 'ervandew/supertab'
" Coffee script support
" Plugin 'kchmck/vim-coffee-script'
" Linting
" Plugin 'scrooloose/syntastic'
" Surround
" Plugin 'tpope/vim-surround'
" Improved c++ syntax highlighting
" Plugin 'octol/vim-cpp-enhanced-highlight'
" Improved indentation after paste
" Plugin 'sickill/vim-pasta'
" Easymotion for crazy motions!
" Plugin 'lokaltog/vim-easymotion'
" For rails formatting
" Plugin 'KurtPreston/vim-autoformat-rails'
" Easy commenting
" Plugin 'tomtom/tcomment_vim'
" AG! search pleasures
" Plugin 'rking/ag.vim'
" All the colorschemes of the world
Plugin 'flazz/vim-colorschemes'
" And more
Plugin 'chriskempson/base16-vim'
" Undo branching
" Plugin 'sjl/gundo.vim'
" Auto completion
" Plugin 'Shougo/neocomplete.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


filetype on
color apprentice
