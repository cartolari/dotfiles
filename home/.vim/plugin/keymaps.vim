" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

nmap <silent> <F2> :NERDTree<CR>
nmap <silent> <F3> :NERDTreeClose<CR>
nmap <silent> <F4> :NERDTreeFind<CR>
nmap <silent> <F5> :VimShell<CR>

" Custom search mapping
nmap <leader>f :Ack <cword><CR>
nmap <leader>ft :execute 'Ack --'.&filetype.' <cword>'<CR>

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

" Neosnippet
" Press Ctrl-k for select and expand a snippet
" Also used for jumping positions inside snippet
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" Slimux (for sending text from vim to a tmux pane)
nmap <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
nmap <Leader>a :SlimuxShellLast<CR>
nmap <Leader>k :SlimuxSendKeysLast<CR>
