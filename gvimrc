set guifont=Liberation\ Mono\ for\ Powerline:h13
set antialias
" Remove right and left scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Yank to system clipboard
:nnoremap <expr> y (v:register ==# '"' ? '"+' : '') . 'y'
:nnoremap <expr> yy (v:register ==# '"' ? '"+' : '') . 'yy'
:nnoremap <expr> Y (v:register ==# '"' ? '"+' : '') . 'Y'
:xnoremap <expr> y (v:register ==# '"' ? '"+' : '') . 'y'
:xnoremap <expr> Y (v:register ==# '"' ? '"+' : '') . 'Y'
