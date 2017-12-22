if exists('g:expand_completed')
  finish
endif
let g:expand_completed = 1

function! ExpandSnippetOrCarriageReturn()
  if !pumvisible()
    return "\<CR>"
  endif

  let l:snippet = UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return l:snippet
  endif

  if exists('v:completed_item') && !empty('v:completed_item')
    let g:last_completed_item = v:completed_item
    let l:snippet = expand_completed#snippet(&filetype, v:completed_item)
    if l:snippet !=# ''
      return UltiSnips#Anon(l:snippet)
    endif
  endif

  return "\<C-y>"
endfunction
