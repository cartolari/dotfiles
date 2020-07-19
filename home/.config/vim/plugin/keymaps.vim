" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

let g:mapleader = ','

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
nnoremap <leader><leader> :Commands<CR>

" Quit current file
nnoremap <leader>q :q<CR>

" Indent current paragraph
nnoremap <leader>a =ip

" Edit vimrc
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>eb :execute 'edit ' . g:config_dir . '/bundles.vim'<CR>
nnoremap <leader>ek :execute 'edit ' . g:config_dir . '/plugin/keymaps.vim'<CR>

"Netrw
nnoremap - :Explore<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! w !sudo tee > /dev/null %

"Remap arrow keys to easily resize windows
nnoremap <silent> <Right> :vertical res +3<CR>
nnoremap <silent> <Left> :vertical res -3<CR>
nnoremap <silent> <Up> :res +3<CR>
nnoremap <silent> <Down> :res -3<CR>

" Toggle current folding
nnoremap <Space> za

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

nnoremap <silent> <leader>t :FZF<CR>

" Start external command with !
nnoremap ! :!

" Invert ; and :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" CtrlSF (search)
nmap <leader>f <Plug>CtrlSFPrompt

" Yank Ring
nnoremap <silent> <Leader>yr :YRGetElem<CR>

" Visual * and # search
vnoremap * :<C-u>call VSetSearch()<CR>/<CR>
vnoremap # :<C-u>call VSetSearch()<CR>?<CR>

nnoremap <silent> <BS> :TmuxNavigateLeft<cr>

" WhichKey
nnoremap <silent> <leader> :WhichKey ','<CR>
vnoremap <silent> <leader> :WhichKeyVisual ','<CR>
call which_key#register(',', 'g:which_key_map')

let g:which_key_map = {}
let g:which_key_map.c = 'Commands'
let g:which_key_map.e = {
      \ 'name': '+ Edit Configuration Files',
      \ 'b': 'Bundles (plugins)',
      \ 'c': [':CocConfig', 'Coc'],
      \ 'k': 'Keymaps',
      \ 'v': 'Vimrc',
      \ }
let g:which_key_map.p = {
      \ 'name': '+ Popup',
      \ 'c': [':call popup_clear()', 'Clear']
      \ }
let g:which_key_map.q = 'Quit'
let g:which_key_map.s = 'Save'
let g:which_key_map.z = 'Toggle Paste Mode'

let g:which_key_map.c = {
      \ 'name' : '+ Code Actions (COC)',
      \ 'a' : {
      \   'name': '+ Actions',
      \   'f': [':CocFix', 'AutoFix'],
      \   'l': [':CocFzfList actions', 'List current actions'],
      \ },
      \ 'c' : [':CocFzfList commands', 'Commands'] ,
      \ 'd' : [':CocFzfList diagnostics', 'Diagnostics'] ,
      \ 'e' : [':CocFzfList extensions', 'Extensions'] ,
      \ 'l' : {
      \   'name': '+ List',
      \   'r': [':CocFzfListResume', 'Resume']
      \ },
      \ 'o' : [':CocFzfList outline', 'Outline'] ,
      \ 'r' : {
      \   'name': '+ Refactor',
      \   'c': [':CocRefactor', 'Refactor'],
      \   'o': [':CocOrganizeImports', 'Organize Imports'],
      \   'r': [':CocRename', 'Rename']
      \ },
      \ 's' : [':CocSearch', 'Search'] ,
      \ 'g' : {
      \   'name': '+ GoTo',
      \   'd' : [':CocGoToDefinition'    , 'Definition']       ,
      \   'i' : [':CocGoToImplementation', 'Implementation']   ,
      \   't' : [':CocGoToTypeDefinition', 'Type Definition']  ,
      \   's' : [':CocFzfList symbols', 'Search Symbol in Workspace']  ,
      \ },
      \ }

" COC
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

function! Enter()
  return complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
endfunction
let g:coc_snippet_next = '<TAB>'
let g:coc_snippet_prev = '<S-TAB>'

inoremap <expr> <CR> Enter()

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Formatting selected code.
xmap =  <Plug>(coc-format-selected)
nmap ==  <Plug>(coc-format-selected)

augroup coc
  autocmd!
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a <Plug>(coc-codeaction-selected)
nmap <leader>a <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>cab  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>caf  <Plug>(coc-fix-current)

" Map function and class text objects: i = inner; a = around; f = function; c = class
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

command! -nargs=? CocFold :call CocActionAsync('fold', <f-args>)
command! -nargs=0 CocFormat :call CocActionAsync('format')
command! -nargs=0 CocGoToDefinition :call CocActionAsync('jumpDefinition')
command! -nargs=0 CocGoToImplementation :call CocActionAsync('jumpImplementation')
command! -nargs=0 CocGoToTypeDefinition :call CocActionAsync('jumpDefinition')
command! -nargs=0 CocOrganizeImports :call CocActionAsync('runCommand', 'editor.action.organizeImport')
command! -nargs=0 CocOutline :call CocActionAsync('outline')
command! -nargs=0 CocRefactor :call CocActionAsync('refactor')
command! -nargs=0 CocRename :call CocActionAsync('rename')
