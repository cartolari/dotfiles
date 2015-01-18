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

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! %!sudo tee > /dev/null %

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
nmap <silent> <Leader>l :SlimuxREPLSendLine<CR>
vmap <silent> <Leader>l :SlimuxREPLSendSelection<CR>

" Rspec
map <Leader>sc :call RunCurrentSpecFile()<CR>
map <Leader>sn :call RunNearestSpec()<CR>
map <Leader>sl :call RunLastSpec()<CR>
map <Leader>sa :call RunAllSpecs()<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
