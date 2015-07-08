function! s:CompareWithClipboard()
  only
  diffthis
  vs
  Scratch
  normal "+p
  diffthis
endfunction
command! CompareWithClipboard call <SID>CompareWithClipboard()
