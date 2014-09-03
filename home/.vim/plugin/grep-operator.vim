function! s:GrepOperatorSearch(type, with_file_type)
  let saved_unnamed_register = @@
  let message = "The selected word have less than 3 characters. Are you sure you want to search for it?"

  if a:type ==# 'v'
    execute "normal! `<v`>y"
  elseif  a:type ==# 'char'
    execute "normal! `[v`]y"
  else
    return
  endif

  if strlen(@@) <= 3
    if confirm(message, "&Yes\n&no") == 2
      return
    endif
  endif

  let grep_command = "grep! " . shellescape(@@) . " ."

  if a:with_file_type == 1
    let grep_command = grep_command.' --'.&filetype
  endif

  let grep_command = grep_command.' | redraw!'

  silent execute grep_command
  copen

  let @@ = saved_unnamed_register
endfunction

function! s:GrepOperator(type)
  call s:GrepOperatorSearch(a:type, 0)
endfunction

function! s:GrepOperatorWithFileType(type)
  call s:GrepOperatorSearch(a:type, 1)
endfunction

nnoremap <Leader>g :set operatorfunc=<SID>GrepOperator<CR>g@
nnoremap <Leader>gt :set operatorfunc=<SID>GrepOperatorWithFileType<CR>g@
vnoremap <Leader>g :<C-u>call <SID>GrepOperator(visualmode())<CR>
vnoremap <Leader>gt :<C-u>call <SID>GrepOperatorWithFileType(visualmode())<CR>
