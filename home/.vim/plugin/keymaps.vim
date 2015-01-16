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

" Tcomment
" Comment Line
nmap <silent> <leader>c :TComment<CR>
vmap <silent> <leader>c :TComment<CR>
" Comment Block
nmap <silent> <leader>b :TCommentBlock<CR>
vmap <silent> <leader>b :TCommentBlock<CR>

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

" Changes the old ruby hash syntax :a => :b
" to the newer a: :b
nmap <silent>ch :.ChangeHashSyntax<CR>

" Toggle current folding
nnoremap <Space> za

" Tabular
nmap <silent> <Leader>t= :Tabularize /=<CR>
vmap <silent> <Leader>t= :Tabularize /=<CR>
nmap <silent> <Leader>t: :Tabularize /:\zs<CR>
vmap <silent> <Leader>t: :Tabularize /:\zs<CR>

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
nmap <silent> <Leader>k :SlimuxSendKeysLast<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
