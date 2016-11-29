" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

" Save current file
nnoremap <leader>s :w<CR>
inoremap <leader>s <Esc>:w<CR>

" Toggle paste mode
nnoremap <silent> <leader>z :set paste!<CR>

" Exit insert mode
inoremap jk <Esc>

" Apply macros with Q
nnoremap Q @q
vnoremap Q :norm @q<cr>

" Inserts one break before and one break after the current line
if has('nvim')
  autocmd FileType * if &l:modifiable | nnoremap <buffer> <M-Enter> O<Esc>jo<Esc>ki| endif
endif

" Quit current file
nnoremap <leader>q :q<CR>

" Indent current paragraph
nnoremap <leader>a =ip

" Edit vimrc
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>eb :edit ~/.vim/bundles.vim<CR>
nnoremap <leader>ek :edit ~/.vim/keymaps.vim<CR>

"Netrw
nnoremap - :Explore<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! w !sudo tee > /dev/null %

" Fix common typos in command mode
command! -bang Q q<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
command! -bang W w<bang>
command! -bang WQ wq<bang>
command! -bang Wq wq<bang>

"Remap arrow keys to easily resize windows
nnoremap <silent> <Right> :vertical res +3<CR>
nnoremap <silent> <Left> :vertical res -3<CR>
nnoremap <silent> <Up> :res +3<CR>
nnoremap <silent> <Down> :res -3<CR>

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

" Run tests from Vim
noremap <silent> <leader>rn :TestNearest<CR>
noremap <silent> <leader>rf :TestFile<CR>
noremap <silent> <leader>ra :TestSuite<CR>
noremap <silent> <leader>rl :TestLast<CR>
noremap <silent> <leader>rg :TestVisit<CR>

" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsListSnippets="<C-l>"

" Tab Completion
function! CleverTabCustom()
  inoremap <silent><tab> <c-r>=CleverTab#Complete('start')<cr>
                        \<c-r>=CleverTab#Complete('tab')<cr>
                        \<c-r>=CleverTab#Complete('ultisnips')<cr>
                        \<c-r>=CleverTab#Complete('omni')<cr>
                        \<c-r>=CleverTab#Complete('user')<cr>
                        \<c-r>=CleverTab#Complete('keyword')<cr>
                        \<c-r>=CleverTab#Complete('stop')<cr>
  inoremap <silent><s-tab> <c-r>=CleverTab#Complete('prev')<cr>
endfunction

augroup Tab
  autocmd!
  autocmd BufEnter * call CleverTabCustom()
augroup END

" Uses <leader>t as fzf trigger
nnoremap <silent> <leader>t :Denite buffer file_rec<CR>

" Make
nnoremap <silent> <leader>m :SyntasticCheck<CR>

" Start external command with !
nnoremap ! :!

" Invert ; and :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" CtrlSF (search)
nmap <leader>f <Plug>CtrlSFPrompt
vmap <leader>f <Plug>CtrlSFVwordPath
vmap <leader>F <Plug>CtrlSFVwordExec
nmap <leader>n <Plug>CtrlSFCwordPath
nmap <leader>p <Plug>CtrlSFPwordPath

" Yank Ring
nnoremap <silent> <Leader>yr :YRGetElem<CR>

" Visual * and # search
vnoremap * :<C-u>call VSetSearch()<CR>/<CR>
vnoremap # :<C-u>call VSetSearch()<CR>?<CR>

nnoremap <silent> <BS> :TmuxNavigateLeft<cr>
