" Show current file directory in current window
au BufEnter * if !exists("b:NERDTreeType") | nnoremap <buffer> <silent> - :EditCurrentDir<CR> |
au BufLeave * if !exists("b:NERDTreeType") | let w:nerdtree_previous_file = expand("%:p") . '' |

call NERDTreeAddKeyMap({
        \ 'key': '-',
        \ 'callback': 'NERDTreeBackToPreviousFile',
        \ 'quickhelpText': 'If activated inside a buffer (like netrw), gets back to the "calling" file' })

function! NERDTreeBackToPreviousFile()
  if exists("w:nerdtree_previous_file")
    execute 'edit ' . w:nerdtree_previous_file
  endif
endfunction
