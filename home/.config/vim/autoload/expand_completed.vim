let s:arguments_regex = '(\([^()]*\))\s\=(\=.*)\='
let s:function_regex = '\w\+' . s:arguments_regex

let s:filetype_functions = {
      \ 'go': 'expand_completed#go_snippet',
      \ 'python': 'expand_completed#python_snippet',
      \ 'javascript': 'expand_completed#javascript_snippet',
      \ 'javascript.jsx': 'expand_completed#javascript_snippet',
      \ 'typescript': 'expand_completed#typescript_snippet'
      \ }

function! expand_completed#snippet_placeholder(index, text)
  return '${' . a:index . ':' . a:text . '}'
endfunction

function! expand_completed#snippet_for_arguments(arguments)
  let l:argument_list = map(
        \ split(a:arguments, ', '),
        \ 'expand_completed#snippet_placeholder(v:key + 1, v:val)'
        \ )
  return '(' . join(l:argument_list, ', ') . ')$0'
endfunction

function! expand_completed#javascript_snippet(item)
  if get(a:item, 'info', '') !~# '^fn(.*)'
    return ''
  endif

  let l:arguments = get(matchlist(a:item.info, s:function_regex), 1, '')
  return expand_completed#snippet_for_arguments(l:arguments)
endfunction

function! expand_completed#go_snippet(item)
  if get(a:item, 'kind', '') !=# 'f' || get(a:item, 'menu', '') ==# ''
    return ''
  endif
  let l:arguments = get(
        \ matchlist(a:item.menu, 'func' . s:arguments_regex),
        \ 1,
        \ '')
  return expand_completed#snippet_for_arguments(l:arguments)
endfunction

function! expand_completed#python_snippet(item)
  if get(a:item, 'menu', '') !~# '^function' || get(a:item, 'info', '') ==# ''
    return ''
  endif

  let l:arguments = get(matchlist(a:item.info, s:function_regex), 1, '')

  " Remove self argument if present
	let l:arguments = substitute(l:arguments, 'self\(, \)\?', '', '')
  return expand_completed#snippet_for_arguments(l:arguments)
endfunction

function! expand_completed#typescript_snippet(item)
  if get(a:item, 'kind', '') !~# 'm' || get(a:item, 'abbr', '') ==# ''
    return ''
  endif

  let l:arguments = get(matchlist(a:item.abbr, s:function_regex), 1, '')
  return expand_completed#snippet_for_arguments(l:arguments)
endfunction

function! expand_completed#snippet(filetype, item)
  let l:function_name = get(s:filetype_functions, a:filetype, '')
  if l:function_name !=# ''
    return function(l:function_name)(a:item)
  endif

  return ''
endfunction
