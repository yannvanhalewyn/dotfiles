"*
"* Use 's:' to not pollute the global namespace
"*

function! s:RevealInFinder()
  if filereadable(expand("%"))
    let l:command = "open -R %"
  elseif getftype(expand("%:p:h")) == "dir"
    let l:command = "open %:p:h"
  else
    let l:command = "open ."
  endif
  execute ":silent! !" . l:command
 " For terminal Vim not to look messed up.
 redraw!
endfunction
command! Reveal call <SID>RevealInFinder()


function! s:Search()
  let query = inputdialog("Search for: ", "", "cancel")
  if query == "" || query == "cancel"
    echo "\nCan't search for nothing!"
    return
  endif

  let dir = inputdialog("In dir: ", "", "cancel")
  if dir == "cancel"
    echo "Cancel!"
    return
  elseif dir != ""
    let dir .= "/"
  endif

  let ext = inputdialog("With extension: ", "", "cancel")
  if ext == "cancel"
    echo "Cancel!"
    return
  endif
  if ext != "" && ext[0] != "."
    let ext = "." . ext
  endif

  let command = "vimgrep /" . query . "/ " . fnameescape(dir) . "**/*" . fnameescape(ext)
  echo "\n"
  echo command
  :execute command

endfunction
command! Find call <SID>Search()
