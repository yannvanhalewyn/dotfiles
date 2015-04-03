set shell=/bin/bash
runtime macros/matchit.vim
" use old regext engine. speed up ruby syntax highlighting
set re=1

set ttyfast
set lazyredraw

let g:ruby_path="~/.rvm/bin/ruby"

"Save on focus lost
:au FocusLost * :wa 

" Sessions
let g:session_autoload = 'no'

" Leader Mappings
let mapleader = "\<Space>"
map <Leader>r :update<CR>
map <Leader>q :bd<CR>
map <Leader>n :NERDTreeToggle<CR>

" Spec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" Switch between the last two files
nnoremap <leader><leader> <c-^>

" Remap tab and shift-tab to switch buffers
:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>

" Remap ENTER and SHIFT-ENTER to append or prepend newlines in normal mode
noremap <CR> o<esc>k

" Current file in nerdtree
map <F9> :NERDTreeFind<CR>


" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
set complete=.,w,t
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

" Reduce timeout after <ESC> is recvd. This is only a good idea on fast links.
set ttimeout
set ttimeoutlen=20
set notimeout

" highlight vertical column of cursor
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline 
set cursorline 

" Leave paste mode on exit
au InsertLeave * set nopaste

set backspace=2   " Backspace deletes like most programs in insert mode
set nocompatible  " Use Vim settings, rather then Vi settings
set nobackup
set nowritebackup
set noswapfile    " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set history=500
set ruler         " show the cursor position all the time
set showcmd       " display incomplete commands
set incsearch     " do incremental searching
set laststatus=2  " Always display the status line
set autowrite     " Automatically :write before running commands

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

filetype plugin indent on

augroup vimrcEx
  autocmd!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile Appraisals set filetype=ruby
  autocmd BufRead,BufNewFile *.md set filetype=markdown

  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell

  " Automatically wrap at 80 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=80
augroup END

" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab

" Display extra whitespace
set list listchars=tab:»·,trail:·

" ##############
" ## Airline! ##
" ##############
"
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

" Color scheme
set background=dark
" color desert
syntax on
colorscheme jellybeans
set encoding=utf-8

" Highlight line number of where cursor currently is
hi CursorLineNr guifg=#050505
hi Comment ctermfg=8

" Numbers
set number
set numberwidth=5

" Persistent undo
set undodir=~/.vim/undo/
set undofile
set undolevels=1000
set undoreload=10000

" :nnoremap <expr> y (v:register ==# '"' ? '"+' : '') . 'y'
" :nnoremap <expr> yy (v:register ==# '"' ? '"+' : '') . 'yy'
" :nnoremap <expr> Y (v:register ==# '"' ? '"+' : '') . 'Y'
" :xnoremap <expr> y (v:register ==# '"' ? '"+' : '') . 'y'
" :xnoremap <expr> Y (v:register ==# '"' ? '"+' : '') . 'Y'

" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" configure syntastic syntax checking to check on open as well as save
let g:syntastic_ruby_checkers = ['mri']
let g:syntastic_enable_highlighting=0

" Remove trailing whitespace on save for ruby files.
au BufWritePre *.rb :%s/\s\+$//e

" Easy navigation between splits. Instead of ctrl-w + j. Just ctrl-j
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


" #############
" ## VUNDLE! ##
" #############
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

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
" UltiSnip
Plugin 'SirVer/ultisnips'
" Otherwise it interferes with my tab completion..
let g:UltiSnipsExpandTrigger="<S-tab>"

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Emmet prefs
autocmd FileType html,css EmmetInstall    " Use only with certain files
let g:user_emmet_expandabbr_key = '<c-e>' " Use the ctrl-e key to expand
