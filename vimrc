"
" `8.`888b           ,8'  8 8888          ,8.       ,8.          8 888888888o.      ,o888888o.
"  `8.`888b         ,8'   8 8888         ,888.     ,888.         8 8888    `88.    8888     `88.
"   `8.`888b       ,8'    8 8888        .`8888.   .`8888.        8 8888     `88 ,8 8888       `8.
"    `8.`888b     ,8'     8 8888       ,8.`8888. ,8.`8888.       8 8888     ,88 88 8888
"     `8.`888b   ,8'      8 8888      ,8'8.`8888,8^8.`8888.      8 8888.   ,88' 88 8888
"      `8.`888b ,8'       8 8888     ,8' `8.`8888' `8.`8888.     8 888888888P'  88 8888
"       `8.`888b8'        8 8888    ,8'   `8.`88'   `8.`8888.    8 8888`8b      88 8888
"        `8.`888'         8 8888   ,8'     `8.`'     `8.`8888.   8 8888 `8b.    `8 8888       .8'
"         `8.`8'          8 8888  ,8'       `8        `8.`8888.  8 8888   `8b.     8888     ,88'
"          `8.`           8 8888 ,8'         `         `8.`8888. 8 8888     `88.    `8888888P'

"/* GENERAL BEHAVIOR
"============================ */

au  FocusLost * :wa               " Save on focus lost
au  InsertLeave * :w              " Save when leaving insert mode
set autowrite                     " Automatically :write before running commands
set autoread<                     " Auto reload files when changed on disk
set backspace=2                   " Backspace deletes like most programs in insert mode
set nocompatible                  " Use Vim settings, rather then Vi settings
set noswapfile                    " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set lazyredraw                    " don't redraw when don't have to
set laststatus=2                  " Always display the status line (Arline bottom bar!)
set completeopt=longest,menuone   " Dont auto-jump to an autocompl
set diffopt+=iwhite               " ignore whitespace in vimdiff
set encoding=utf-8                " Encoding
set timeoutlen=600 ttimeoutlen=10 " faster timeout for escape key
set smartcase                     " caps sensitive searching
set wildmenu                      " Showing a list of command completions
set wildmode=longest,list,full    " Have a zsh like completion in cmd mode
set history=200                   " More ex-commands history

" Tabsize
set softtabstop=2
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


"/* AUTOCOMMANDS
"============================ */

" Remove trailing whitespace on save for all files.
au BufWritePre * :%s/\s\+$//e

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

" set leader key
let mapleader = "\<Space>"

" close buffer using a script in ~/.vim/plugin/BufOnly.vim
" It swaps it with the previouse buffer, or an empty one if needed.
map <Leader>q :Bclose<CR>
map <Leader>Q :Bonly<CR>

" toggle NerdTree / Gundo
map <Leader>n :NERDTreeToggle<CR>
map <Leader>N :NERDTreeFind<CR>
map <Leader>g :GundoToggle<CR>

" TCommenter (Like TCommenter more, but got used to NerdTree comment
map <leader>cs :TCommentBlock<CR>
map <leader>cc :TComment<CR>
map <leader>ci :TCommentInline<CR>
" Sexy titles
nmap <leader>ct yyppVr=kkVr=Vjj cs
au filetype ruby nmap <leader>ct yyppv$r=kkv$r=Vjj cc
" Call vimux commands
map <Leader>vp :call VimuxPromptCommand()<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vv :VimuxZoomRunner<CR>
map <Leader>vc :VimuxCloseRunner<CR>

" Moving lines/selection up and down - direct map for vim-pasta
nmap <UP> ddkP
nmap <DOWN> ddp
nnoremap <LEFT> <<
nnoremap <RIGHT> >>
vmap <UP> dkPV`>kk
vmap <DOWN> <esc>jjmm`<V`>dpV`mk
vnoremap <LEFT> <V`>
vnoremap <RIGHT> >V`>

" Map @ in visual mode to execute reg in normal mode on every line
vnoremap @ :normal@

" Auto center!
nmap G Gzz
nmap n nzz
nmap N Nzz
nmap { {zz
nmap } }zz

" Surround with quotes / #{} for ruby vars in quotes / parens
nmap <leader>= ^v$hS=
nmap <leader>- ^v$hS-
vnoremap <leader># <esc>`>a}<esc>`<i#{<esc>
nmap <leader># viw<leader>#
vnoremap <leader>erb <esc>`>a %><esc>`<i<%= <esc>

" Breakout selection on own line
vnoremap <leader><CR> <esc>a<CR><esc>`<i<CR><esc>

" Spec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

"Edit vimrc in split/source vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap<leader>sv :source $MYVIMRC<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Change inside parens from outside the parens! - change inside next parens
:onoremap in( :<c-u>normal! f(vi(<CR>
:onoremap in) :<c-u>normal! f)vi)<CR>
:onoremap in[ :<c-u>normal! f[vi[<CR>
:onoremap in] :<c-u>normal! f]vi]<CR>
:onoremap in{ :<c-u>normal! f{vi{<CR>
:onoremap in} :<c-u>normal! f}vi}<CR>
" Change inside last(prev)
:onoremap il( :<c-u>normal! F(vi(<CR>
:onoremap il) :<c-u>normal! F)vi)<CR>
:onoremap il[ :<c-u>normal! F[vi[<CR>
:onoremap il] :<c-u>normal! F]vi]<CR>
:onoremap il{ :<c-u>normal! F{vi{<CR>
:onoremap il} :<c-u>normal! F}vi}<CR>

" Yank from cursor to end, copy to "o reg and execute
nnoremap <leader>o "oyy:<C-r>o<Backspace><CR>
"nnoremap <leader>o Y:@"<CR>

" Makes more sense
map Y y$

" Go to help
nmap <leader>H :help <c-r><c-w><cr>

"/* ABBREVIATIONS (TYPOS)
"============================ */

:iabbrev adn and
:iabbrev waht what
:iabbrev tehn then
:iabbrev succes success
:iabbrev ressource resource
:iabbrev ressources resources
:iabbrev widht width
:iabbrev heigth height
:iabbrev ture ture
:iabbrev flase false


"/* AIRLINE
"============================ */

let g:airline_powerline_fonts = 1 " This actually makes the top buffer bar have the 's
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


"/* VIM-PLUG
"============================ */

let g:plug_threads=1
" Set correct editor root path
if has('nvim')
  let s:editor_root=expand("~/.nvim")
else
  let s:editor_root=expand("~/.vim")
endif
call plug#begin(s:editor_root . "/plugged")

Plug 'mattn/emmet-vim'                   " Emmet
Plug 'kien/ctrlp.vim'                    " CTRL-P
Plug 'bling/vim-airline'                 " Airline status bar
Plug 'tpope/vim-fugitive'                " Git wrapper/airline branch display
Plug 'scrooloose/nerdtree'               " NerdTree
Plug 'tpope/vim-endwise'                 " Endwise (Ruby)
Plug 'Raimondi/delimitMate'              " DelimitMate
Plug 'thoughtbot/vim-rspec'              " vim-rspec
Plug 'benmills/vimux'                    " To send commands to TMUX (RSpec!!)
Plug 'Keithbsmiley/rspec.vim'            " RSPEC synthax higlighting
Plug 'tpope/vim-rails'                   " Rails.vim
Plug 'honza/vim-snippets'                " Some snippets
Plug 'SirVer/ultisnips'                  " UltiSnips
Plug 'ervandew/supertab'                 " Supertab so that ultisnips and completions play nice
Plug 'tpope/vim-surround'                " Surround
Plug 'octol/vim-cpp-enhanced-highlight'  " Improved c++ syntax highlighting
Plug 'sickill/vim-pasta'                 " Improved indentation after paste
Plug 'lokaltog/vim-easymotion'           " Easymotion for crazy motions!
Plug 'tomtom/tcomment_vim'               " Easy commenting
Plug 'rking/ag.vim'                      " AG! search pleasures
Plug 'flazz/vim-colorschemes'            " All the colorschemes of the world
Plug 'chriskempson/base16-vim'           " And more
Plug 'sjl/gundo.vim'                     " Undo branching
" Plug 'Shougo/neocomplete.vim'          " Auto completion
Plug 'Valloric/YouCompleteMe'            " Youcompleteme
Plug 'mhinz/vim-startify'                " Vim startify
Plug 'junegunn/vim-easy-align'           " Aligning stuff

call plug#end()


"/* PLUGIN SPECIFIC CONFIG
"============================ */

" EMMET
autocmd FileType html,css EmmetInstall    " Use only with certain files
let g:user_emmet_expandabbr_key = '<c-e>' " Use the ctrl-e key to expand

" NERDTREE
let NERDTreeShowHidden=1
let NERDTreeAutoDeleteBuffer=1

" CTRLP
let g:ctrlp_custom_ignore = 'tmp\|node_modules\|bin\|obj\|undo'

" Ultisnips
let g:UltiSnipsJumpForwardTrigger      = "<tab>"
let g:UltiSnipsJumpBackwardTrigger     = "<s-tab>"

" You complete me
let g:ycm_key_list_select_completion   = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType    = '<C-n>'
let g:ycm_global_ycm_extra_conf        = s:editor_root . "/plugged/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py"

" rspec-vim - Send to tmux pane if tmux
if exists('$TMUX')
  let g:rspec_command = 'call VimuxRunCommand("rspec {spec}\n")'
endif

" Neocomplete
let g:neocomplete#enable_at_startup = 1

" The command to make text into multiline shizzle ⇒   :'<,'>normal 0v$hS'i\d0A,
" http://www.cowsays.com
let g:startify_custom_header = [
\'         ________________________',
\'        < Stay hungry my friend. >',
\'         ------------------------',
\'                \   ^__^',
\'                 \  (oo)\_______',
\'                    (__)\       )\/\ ',
\'                        ||----w |',
\'                        ||     ||',
\''
\]

" Easy Align
vmap <Enter> <Plug>(EasyAlign)

" Weird bug in Tmux where background won't fill workspace.
:set t_ut=


"/* FILETYPE SPECIFIC CONFIG
"============================ */

" Syntax highlighting for javascript templating
au BufNewFile,BufRead *.tpl set syntax=jst

" Map <leader>r to run files with some extensions
au FileType ruby nnoremap <leader>r :!ruby %<CR>
au FileType {cpp,make} nnoremap <leader>r :!make<CR>
au FileType cpp nnoremap <leader>l :SyntasticCheck<CR>
au FileType cpp set softtabstop=4
au FileType cpp set shiftwidth=4

" Spell check for .md files
au FileType markdown setlocal spell


"/* LAYOUT
"============================ */

" Basic
set relativenumber
syntax on

" highlight vertical column of cursor
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline
set cursorline

" Display extra whitespace
set list listchars=tab:»·,trail:·

" The colorscheme
set background=dark
colorscheme jellybeans
au VimEnter AirlineTheme monochrome


"/* My favorite colorschemes
"=========================== */

" colorscheme base16-chalk
" colorscheme base16-aterlierdune
" colorscheme candyman
" colorscheme zendune
" colorscheme tomorrow-night
