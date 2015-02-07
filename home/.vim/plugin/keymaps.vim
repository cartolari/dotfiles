" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

" Save current file
nnoremap <leader>s :w<CR>
inoremap <leader>s <Esc>:w<CR>

" Toggle paste mode
nnoremap <silent> <leader>z :set paste!<CR>
" Toggle fold enable
nnoremap <leader>f :set foldenable!<CR>

" Exit insert mode
inoremap jk <Esc>

" Apply macros with Q
nnoremap Q @q
vnoremap Q :norm @q<cr>

" Inserts one break before and one break after the current line
autocmd FileType * if &buftype ==? "" || &buftype ==? "acwrite" |nnoremap <buffer> <CR><CR> <S-o><Esc>jo<Esc>ki| endif

" Quit current file
nnoremap <leader>q :q<CR>

" Indent current paragraph
nnoremap <Leader>a =ip

"Netrw
nnoremap - :Explore<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! %!sudo tee > /dev/null %

" Fix common typos in command mode
command! -bang Q q<bang>
command! -bang QA q<bang>
command! -bang Qa q<bang>
command! -bang W w<bang>
command! -bang WQ q<bang>
command! -bang Wq q<bang>

" Clear highlighted searchs
nnoremap <silent> <leader>h :<C-u>set hlsearch!<CR>

"Remap arrow keys to easily resize windows
noremap <silent> <Right> :vertical res +3<CR>
noremap <silent> <Left> :vertical res -3<CR>
noremap <silent> <Up> :res +3<CR>
noremap <silent> <Down> :res -3<CR>

" Toggle current folding
nnoremap <Space> za

" Move lines or chunks of lines up and down
" Bubble single lines
nmap <C-Up> [e
nmap <C-Down> ]e
" Bubble multiple lines
vmap <C-Up> [egv
vmap <C-Down> ]egv

" Slimux (for sending text from vim to a tmux pane)
nnoremap <silent> <Leader>l :SlimuxREPLSendLine<CR>
vnoremap <silent> <Leader>l :SlimuxREPLSendSelection<CR>

" Rspec
nnoremap <silent> <Leader>rf :call RunCurrentSpecFile()<CR>
nnoremap <silent> <Leader>rn :call RunNearestSpec()<CR>
nnoremap <silent> <Leader>rl :call RunLastSpec()<CR>
nnoremap <silent> <Leader>ra :call RunAllSpecs()<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsListSnippets="<C-k>"

" Vim multiple cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<C-d>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'
