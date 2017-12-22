function! test#javascript#teaspoon#test_file(file)
  return a:file =~# '_spec\.coffee$' || a:file =~# '_spec\.js$'
endfunction

function! test#javascript#teaspoon#build_position(type, position)
  return []
endfunction

function! test#javascript#teaspoon#build_args(args)
  return a:args
endfunction

" Returns the executable of your test runner
function! test#javascript#teaspoon#executable()
  if filereadable('./bin/rake')
    return './bin/rake teaspoon'
  elseif filereadable('Gemfile')
    return 'bundle exec teaspoon'
  else
    return 'teaspoon'
  endif
endfunction
