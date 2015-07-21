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
  autocmd FileType * if &l:modifiable | nnoremap <buffer> <M-Enter> <S-o><Esc>jo<Esc>ki| endif
endif

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
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsListSnippets="<C-k>"

" Deoplete
function! ExpandSnippetOrTab()
  call UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res == 0
    return "\<Tab>"
  else
    return ""
  endif
endfunction

function! NextOrExpandSnippet()
  return pumvisible() ? "\<C-n>" : ExpandSnippetOrTab()
endfunction

function! BackOrTab()
  return pumvisible() ? "\<C-p>" : "\<Tab>"
endfunction

function! CompletionMappings()
  inoremap <silent><Tab> <c-r>=NextOrExpandSnippet()<cr>
  inoremap <silent><S-Tab> <c-r>=BackOrTab()<cr>
  inoremap <expr><C-y> deoplete#mappings#close_popup()
  inoremap <expr><C-e> deoplete#mappings#cancel_popup()
endfunction

augroup Tab
  autocmd!
  autocmd BufEnter * call CompletionMappings()
augroup END

" Uses <leader>t as fzf trigger
nnoremap <silent> <leader>t :FZF<CR>
" Uses <leader>T as fzf mru trigger
nnoremap <silent> <leader>T :FZFMru<CR>

" Make
nnoremap <silent> <leader>m :SyntasticCheck<CR>

" Start external command with !
nnoremap ! :!

" Invert ; and :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

augroup NeoTerm
  autocmd!
  autocmd BufEnter * if &buftype==?'terminal' | nnoremap <buffer> <silent> <leader>q :Tclose<CR> | endif
augroup end

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
