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
nnoremap <leader>a =ip

" Edit vimrc
nnoremap <leader>e :edit $MYVIMRC<CR>

"Netrw
nnoremap - :Explore<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! %!sudo tee > /dev/null %

" Fix common typos in command mode
command! -bang Q q<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
command! -bang W w<bang>
command! -bang WQ wq<bang>
command! -bang Wq wq<bang>

" Clear highlighted searchs
nnoremap <silent> <leader>h :<C-u>set hlsearch!<CR>

"Remap arrow keys to easily resize windows
noremap <silent> <Right> :vertical res +3<CR>
noremap <silent> <Left> :vertical res -3<CR>
noremap <silent> <Up> :res +3<CR>
noremap <silent> <Down> :res -3<CR>

" Toggle current folding
nnoremap <Space> za

" Indent entire file without moving the cursor
nnoremap == migg=G`i

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

" Run tests from Vim
noremap <silent> <leader>rn :TestNearest<CR>
noremap <silent> <leader>rf :TestFile<CR>
noremap <silent> <leader>ra :TestSuite<CR>
noremap <silent> <leader>rl :TestLast<CR>
noremap <silent> <leader>rg :TestVisit<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsListSnippets="<C-k>"

" Uses <C-p> as fzf trigger
nnoremap <silent> <C-p> :FZF<CR>

" Make
nnoremap <silent> <leader>m :Neomake<CR>

" Start external command with !
nnoremap ! :!
