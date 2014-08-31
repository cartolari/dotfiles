" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

nmap <silent> <F2> :NERDTree<CR>
nmap <silent> <F3> :NERDTreeClose<CR>
nmap <silent> <F4> :NERDTreeFind<CR>
nmap <silent> <F5> :VimShell<CR>

" Custom search mapping
" This function uses Ack.vim to search for the word
" under the cursor, optionally restricting the search
" only for files of the same time of the current
function AckSearch(with_file_type)
  let ack_command = ":Ack"
  let current_word = expand("<cword>")
  let message = "The selected word have less than 3 characters. Are you sure you want to search for it?"

  if strlen(current_word) <= 3
    if confirm(message, "&Yes\n&no") == 2
      return
    endif
  endif

  if a:with_file_type == 1
    let ack_command = ack_command.' --'.&filetype
  endif

  let ack_command = ack_command.' '.current_word.expand("<CR>")
  execute ack_command
endfunction
command AckSearch execute AckSearch(0)
command AckSearchCurrentType execute AckSearch(1)
nmap <leader>f :AckSearch<CR>
nmap <leader>ft :AckSearchCurrentType<CR>

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
nmap <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
nmap <Leader>a :SlimuxShellLast<CR>
nmap <Leader>k :SlimuxSendKeysLast<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<Tab>"                                            
let g:UltiSnipsJumpForwardTrigger="<Tab>"                                       
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"                                    
