" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

nnoremap <silent> <F2> :NERDTree<CR>
nnoremap <silent> <F3> :NERDTreeClose<CR>
nnoremap <silent> <F4> :NERDTreeFind<CR>
nnoremap <silent> <F5> :VimShell<CR>

" Save current file
nnoremap <leader>s :w<CR>
inoremap <leader>s <Esc>:w<CR>

" Exit insert mode
inoremap jk <Esc>

" Quit current file
nnoremap <leader>q :q<CR>

" Indent current paragraph
nnoremap <Leader>a =ip

" Tcomment
" Comment Line
nmap <leader>c <c-_><c-_>
vmap <leader>c <c-_><c-_>
" Comment Block
nmap <leader>b <c-_>b
vmap <leader>b <c-_>b

" Move through vim buffers
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! %!sudo tee > /dev/null %

" Clear highlighted searchs
nnoremap <silent> <C-f> :<C-u>nohlsearch<CR>
" Generates ctags file
nnoremap <silent> <F6> :!ctags -R<CR>

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
