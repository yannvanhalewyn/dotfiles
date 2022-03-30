"                                           .         .
"  `8.`888b           ,8'  8 8888          ,8.       ,8.          8 888888888o.      ,o888888o.
"   `8.`888b         ,8'   8 8888         ,888.     ,888.         8 8888    `88.    8888     `88.
"    `8.`888b       ,8'    8 8888        .`8888.   .`8888.        8 8888     `88 ,8 8888       `8.
"     `8.`888b     ,8'     8 8888       ,8.`8888. ,8.`8888.       8 8888     ,88 88 8888
"      `8.`888b   ,8'      8 8888      ,8'8.`8888,8^8.`8888.      8 8888.   ,88' 88 8888
"       `8.`888b ,8'       8 8888     ,8' `8.`8888' `8.`8888.     8 888888888P'  88 8888
"        `8.`888b8'        8 8888    ,8'   `8.`88'   `8.`8888.    8 8888`8b      88 8888
"         `8.`888'         8 8888   ,8'     `8.`'     `8.`8888.   8 8888 `8b.    `8 8888       .8'
"          `8.`8'          8 8888  ,8'       `8        `8.`8888.  8 8888   `8b.     8888     ,88'
"           `8.`           8 8888 ,8'         `         `8.`8888. 8 8888     `88.    `8888888P'

"/* GENERAL BEHAVIOR
"============================ */

set autowrite                     " Automatically :write before running commands
set autoread                      " Auto reload files when changed on disk
set backspace=2                   " Backspace deletes like most programs in insert mode
set nocompatible                  " Use Vim settings, rather then Vi settings
set noswapfile                    " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set lazyredraw                    " don't redraw when don't have to
set laststatus=2                  " Always display the status line (Arline bottom bar!)
set completeopt=longest,menuone   " Dont auto-jump to an autocompl
set encoding=utf-8                " Set encoding
set timeoutlen=200 ttimeoutlen=10 " faster timeout for escape key
set smartcase                     " caps sensitive searching
set wildmenu                      " Showing a list of command completions
set wildmode=longest,list,full    " get a shell like completion
set history=200                   " More ex-commands history
set hlsearch                      " Show what 'n' would go to
set incsearch                     " Highlight matching search patterns while typing
set formatoptions+=j              " remove comment chars when joining lines
set t_ut=                         " Weird bug in Tmux where background won't fill workspace.
let $NVIM_TUI_ENABLE_TRUE_COLOR=1 " True gui colors in terminal

" Tabsize
set softtabstop=2
set shiftwidth=2
set expandtab

" Persistent undo
set undodir=~/.vim/undo
set undofile
set undolevels=1000
set undoreload=10000

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

"/* AUTOCOMMANDS
"============================ */

augroup AUTOSAVE
  au  FocusLost * :wa               " Save on focus lost
  au  InsertLeave * :w              " Save when leaving insert mode
augroup END

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

" set leader key
let mapleader = "\<Space>"

" Remap tab and shift-tab to switch buffers
nnoremap <Tab>   :tabnext<CR>
nnoremap <S-Tab> :tabprevious<CR>
nnoremap <leader>p :set invpaste<CR>

" close buffer using a script in ~/.vim/plugin/BufOnly.vim
" It swaps it with the previouse buffer, or an empty one if needed.
map  <Leader>q  :Bclose<CR>
map  <Leader>Q  :Bonly<CR>
" toggle NerdTree
map  <Leader>n  :NERDTreeToggle<CR>
map  <Leader>N  :NERDTreeFind<CR>
" Fugitive mappings
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gd :Gvdiff<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gb :Gblame<CR>
" TCommenter (Like TCommenter more, but got used to NerdTree comment
map  <Leader>cs :TCommentBlock<CR>
map  <Leader>cc :TComment<CR>
map  <Leader>ci :TCommentInline<CR>
" Call vimux commands
map  <Leader>vp :call VimuxPromptCommand()<CR>
map  <Leader>vl :VimuxRunLastCommand<CR>
map  <Leader>vv :VimuxZoomRunner<CR>
map  <Leader>vc :VimuxCloseRunner<CR>
map  <Leader>vk :VimuxInterruptRunner<CR>
nmap  <a-j>     :VimuxScrollDownInspect<CR>
nmap  <a-k>     :VimuxScrollUpInspect<CR>
" Spec.vim mappings
map  <Leader>t  :call RunCurrentSpecFile()<CR>
map  <Leader>s  :call RunNearestSpec()<CR>
map  <Leader>l  :call RunLastSpec()<CR>
map  <Leader>a  :call RunAllSpecs()<CR>
" UltisnipsEdit
map <Leader>u   :UltiSnipsEdit<CR>
map <Leader>m   :call RenameFile()<CR>
" Using Ag for current word
map <Leader>x   :Ag <C-r><C-w>
map <Leader>X   :Ag <C-r><C-w>

" Surround with quotes / #{} for ruby vars in quotes / parens
nmap      <Leader>=   ^v$hS=
nmap      <Leader>-   ^v$hS-
vnoremap  <Leader>#   <esc>`>a}<esc>`<i#{<esc>
nmap      <Leader>#   viw<Leader>#
vnoremap  <Leader>erb <esc>`>a %><esc>`<i<%= <esc>

" Breakout selection on own line
vnoremap <Leader><CR> <esc>a<CR><esc>`<i<CR><esc>

" Rotate windows
nnoremap <Leader>w <c-w>r

"Edit vimrc in split/source vimrc
nnoremap <Leader>ev  :vsplit $MYVIMRC<CR>
nnoremap <Leader>sv  :source $MYVIMRC<CR>

" Sexy titles
nmap <Leader>ct yyppVr=kkVr=Vjj cs
au filetype ruby nmap <Leader>ct yyppv$r=kkv$r=Vjj cc
nmap <Leader>ft 0mm"zY:r !figlet -w 120 -f broadway <c-r>z<CR>V`m ccdd

" Yank from cursor to end, copy to "o reg and execute
nnoremap <Leader>o "oyy:<C-r>o<Backspace><CR>

" Go to help
nmap <Leader>H :help <c-r><c-w><cr>

" Moving lines/selection up and down - direct map for vim-pasta
nmap        <UP> ddkP
nmap      <DOWN> ddp
nnoremap  <LEFT> <<
nnoremap <RIGHT> >>
vmap        <UP> dkPV`>kk
vmap      <DOWN> <esc>jjmm`<V`>dpV`mk
vnoremap  <LEFT> <V`>
vnoremap <RIGHT> >V`>

" Map @ in visual mode to execute reg in normal mode on every line
vnoremap @ :normal@

" Auto center!
nmap G Gzz
nmap n nzz
nmap N Nzz
nmap { {zz
nmap } }zz

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Increment for tmux
nnoremap <C-I> <C-A>

" Makes more sense
map Y y$

" Magical regex
" nmap / /\v

" Recentering while typing
inoremap <c-z> <c-o>zz

" Tag finder and navigation
nmap <c-t> :CtrlPTag<CR>
nmap <Leader>b :CtrlPBuffer<CR>
nmap <Leader>f :CtrlP<CR>

" this thing is annoying
nmap ' <NOP>

"/* ABBREVIATIONS (TYPOS)
"============================ */

iabbrev adn        and
iabbrev waht       what
iabbrev tehn       then
iabbrev succes     success
iabbrev ressource  resource
iabbrev ressources resources
iabbrev widht      width
iabbrev heigth     height
iabbrev ture       ture
iabbrev flase      false


"/* VIM-PLUG
"============================ */

" Set correct editor root path
let s:editor_root=expand("~/.vim")
call plug#begin(s:editor_root . "/plugged")

" Favorites
Plug 'bling/vim-airline'                               " Airline status bar
Plug 'tpope/vim-fugitive'                              " Git wrapper/airline branch display
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}   " NerdTree
Plug 'ctrlpvim/ctrlp.vim'                              " CTRL-P
Plug 'jiangmiao/auto-pairs'
Plug 'SirVer/ultisnips'                                " UltiSnips
Plug 'tpope/vim-surround'                              " Surround
Plug 'yannvanhalewyn/vim-mocha', {'for': 'javascript'} " Same as thoughtbots vim-rspec
Plug 'ervandew/supertab'                               " Supertab so that ultisnips and completions play nice
Plug 'tomtom/tcomment_vim'                             " Easy commenting
Plug 'benmills/vimux'                                  " To send commands to TMUX (RSpec!!)
Plug 'tpope/vim-abolish'                               " Badass replace

" Useful
Plug 'rhysd/clever-f.vim'                              " Inline easymotion with f and t commands
Plug 'rking/ag.vim', {'on': 'Ag'}                      " AG! search pleasures
Plug 'tpope/vim-endwise', {'for': ['ruby','sh','vim']} " Add matching 'end' in ruby/shell
Plug 'honza/vim-snippets'                              " Some snippets
Plug 'sickill/vim-pasta'                               " Improved indentation after paste
Plug 'thoughtbot/vim-rspec', {'for': 'ruby'}           " vim-rspec
Plug 'yannvanhalewyn/vim-run'                          " Run files of different FT
Plug 'pangloss/vim-javascript', {'for': 'javascript'}  " JS Syntax and indentation
Plug 'tpope/vim-unimpaired'                            " Good mappings
Plug 'kana/vim-textobj-user'                           " Easily create new text objects.
Plug 'nelstrom/vim-textobj-rubyblock', {'for': 'ruby'} " Change inside ruby {cir}
Plug 'wellle/targets.vim'                              " Some extra targets like inside next parens, etc..
Plug 'heavenshell/vim-jsdoc', {'for': 'javascript'}    " Easy interface for adding javascript documentation.
Plug 'tpope/vim-rails', {'for': 'ruby'}
" Plug 'AndrewRadev/splitjoin.vim'

" Layout / syntax support
Plug 'w0rp/ale'                                        " Async linter
" Plug 'flazz/vim-colorschemes'                          " All the colorschemes of the world
" Plug 'chriskempson/base16-vim'                         " And more
Plug 'embark-theme/vim', { 'as': 'embark' }            " Another great colorscheme"
Plug 'mustache/vim-mustache-handlebars'                " Syntax for handlebars/mustache
Plug 'junegunn/vim-easy-align'                         " Aligning stuff
Plug 'Keithbsmiley/rspec.vim', {'for': 'ruby'}         " RSPEC syntax higlighting
Plug 'octol/vim-cpp-enhanced-highlight', {'for':'cpp'} " Improved c++ syntax highlighting
Plug 'justinmk/vim-syntax-extra', {'for': 'c'}
Plug 'othree/yajs.vim'
Plug 'mxw/vim-jsx'
Plug 'kchmck/vim-coffee-script'
Plug 'slim-template/vim-slim'

runtime macros/matchit.vim

call plug#end()


"/* PLUGIN SPECIFIC CONFIG
"============================ */

" AIRLINE
let g:airline_powerline_fonts = 1
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#fnamemod = ':t'

" NERDTREE
let NERDTreeShowHidden=1
let NERDTreeAutoDeleteBuffer=1

" CTRLP
let g:ctrlp_custom_ignore = 'tmp\|vcr_cassettes\|node_modules\|obj/\|undo\|vim/plugged'
let g:ctrlp_clear_cache_on_exit=0

" Ultisnips
let g:UltiSnipsJumpForwardTrigger      = "<tab>"
let g:UltiSnipsJumpBackwardTrigger     = "<s-tab>"

" vim-mocha and vim-rspec in tmux
if exists('$TMUX')
  let g:rspec_command = 'VimuxRunCommand("bin/rspec {spec}\n")'
  let g:mocha_js_command = 'VimuxRunCommand("mocha {spec}")'
else
  set clipboard=unnamed             " use system clipboard by default
endif

" Unimpaired for tabs
nmap ]w :tabnext<CR>
nmap [w :tabprev<CR>

" Easy Align
vmap <Enter> <Plug>(EasyAlign)
if !exists('g:easy_align_delimiters')
  let g:easy_align_delimiters = {}
endif

" Ignore groups override so that it looks for the chars in comments
let g:easy_align_delimiters['"'] = { 'pattern': '"', 'ignore_groups': ['String'] }

" rspec
let g:rspec_runner='os_x_iterm2'

" JSDOC
let g:jsdoc_allow_input_prompt=1
let g:jsdoc_input_description=1
let g:jsdoc_default_mapping=0

"/* FILETYPE SPECIFIC CONFIG
"============================ */

" Syntax highlighting for javascript templating
au BufNewFile,BufRead *.tpl set syntax=jst
" JS mocha
au FileType javascript vnoremap <Leader>be d?describe<CR>o<CR>beforeEach(function() {<CR>});<esc>P<esc>
" CPP
au FileType {cpp,java,c} set softtabstop=4
au FileType {cpp,java,c} set shiftwidth=4
au FileType cpp nmap ga :e %:p:s,.h$,.X123X,:s,.cpp$,.h,:s,.X123X$,.cpp,<CR>
au FileType cpp nmap gi ^YgaGo<ESC>o<ESC>pI<c-r>=expand("%:t:r")<CR>::<ESC>A<BS> {<CR><ESC>O<F37>
au FileType {ruby,javascript} set softtabstop=2
au FileType {ruby,javascript} set shiftwidth=2
" Markdown
au FileType markdown setlocal spell
au FileType markdown nnoremap <leader>sh "zyy"zpVr-
au FileType markdown nnoremap <leader>h "zyy"zpVr=

"/* LAYOUT
"============================ */

" Basic
set number
set nowrap
set scrolljump=5
syntax on
set textwidth=80

" Display extra whitespace
set list listchars=tab:»·,trail:·

" The colorscheme
if has('nvim')
  au VimEnter * colorscheme base16-ocean
  au VimEnter * set background=dark
  au VimEnter * AirlineTheme dark
else
  au VimEnter * colorscheme monokai
  au VimEnter * set background=dark
  au VimEnter * AirlineTheme dark

  " au VimEnter * set backgrouj
  " au VimEnter * colorscheme embark
  " let g:embark_terminal_italics = 1
  " au VimEnter * AirlineTheme base16
endif

"/* My favorite colorschemes
"=========================== */

" colorscheme jellybeans
" colorscheme solarized
" colorscheme base16-ocean
" colorscheme base16-chalk
" colorscheme base16-aterlierdune set bg=dark
" colorscheme candyman
" colorscheme zendune
" colorscheme tomorrow-night
" colorscheme gruvbox
" colorscheme codeschool
" colorscheme lucius (bg=dark)
