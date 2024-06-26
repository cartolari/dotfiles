" vim: set foldlevel=0 foldmethod=marker:
" cSpell:disable

if has('vim_starting')
  call plug#begin($XDG_DATA_HOME . '/vim/bundle')
endif

" Libs {{{
  Plug 'kana/vim-textobj-user'
  Plug 'tpope/vim-repeat'
  Plug 'vim-scripts/l9'
" }}}

" Color Schemes {{{
  Plug 'cartolari/vim-niji', {'for': ['clojure', 'lisp', 'scheme']}
  Plug 'reedes/vim-colors-pencil'
" }}}

" Completions and Snippets {{{
  Plug 'Sirver/Ultisnips'
  Plug 'honza/vim-snippets'
  Plug 'jiangmiao/auto-pairs'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'tpope/vim-endwise', {'for': ['ruby', 'sh', 'zsh', 'vim', 'c', 'cpp']}
" }}}

" File and Search {{{
  Plug 'antoinemadec/coc-fzf'
  Plug 'dyng/ctrlsf.vim'
  Plug 'henrik/vim-indexed-search'
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'
" }}}

" General Programming {{{
  Plug 'andrewradev/splitjoin.vim', {'for': [
        \ 'ruby', 'eruby', 'coffee', 'python',
        \ 'javascript', 'html', 'xml', 'css',
        \ 'scss', 'less', 'yaml', 'vim' ]}
  Plug 'andrewradev/switch.vim'
  Plug 'danielwe/base16-vim'
  Plug 'embear/vim-localvimrc'
  Plug 'godlygeek/tabular'
  Plug 'janko-m/vim-test'
  Plug 'lambdalisue/suda.vim'
  Plug 'majutsushi/tagbar'
  Plug 'osyo-manga/vim-over'
  Plug 'sjl/gundo.vim'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'tommcdo/vim-exchange'
  Plug 'tomtom/tcomment_vim'
  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-eunuch'
  Plug 'tpope/vim-projectionist'
  Plug 'tpope/vim-rsi'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-unimpaired'
  Plug 'vim-scripts/LargeFile'
  Plug 'vim-scripts/YankRing.vim'
  Plug 'w0rp/ale'
" }}}

" Integrations {{{
  Plug 'airblade/vim-gitgutter'
  Plug 'christoomey/vim-system-copy'
  Plug 'christoomey/vim-tmux-runner'
  Plug 'tpope/vim-fugitive'
" }}}

" Language Specific {{{

" ASCII {{{
  Plug 'vim-scripts/AnsiEsc.vim', {'on': 'AnsiEsc'}
" }}}

" Clojure {{{
  Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme']}
  Plug 'tpope/vim-fireplace', {'for': 'clojure'}
  Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme']}
" }}}

" Coffeescript {{{
  Plug 'kchmck/vim-coffee-script', {'for': 'coffee'}
" }}}

" CSS {{{
  Plug 'gorodinskiy/vim-coloresque'
" }}}

" Elm {{{
  Plug 'ElmCast/elm-vim'
" }}}

" Fish {{{
  Plug 'dag/vim-fish'
" }}}

" HTML {{{
  Plug 'othree/html5.vim', {'for': 'html'}
  Plug 'mattn/emmet-vim'
  Plug 'valloric/MatchTagAlways', {'for': ['html', 'xml']}
" }}}

" Markdown {{{
  Plug 'suan/vim-instant-markdown', {'for': 'markdown', 'do': 'npm install -g instant-markdown-d'}
" }}}

" JavaScript {{{
  Plug 'MaxMEllon/vim-jsx-pretty'
  Plug 'posva/vim-vue'
" }}}

" JSON {{{
  Plug 'kevinoid/vim-jsonc'
" }}}

" Ledger {{{
  Plug 'ledger/vim-ledger'
" }}}

" QML {{{
  Plug 'peterhoeg/vim-qml'
" }}}

" Slim {{{
  Plug 'slim-template/vim-slim'
" }}}

" PHP {{{
  Plug 'joonty/vdebug'
" }}}

" Ruby {{{
  Plug 'ecomba/vim-ruby-refactoring'
  Plug 'nelstrom/vim-textobj-rubyblock', {'for': 'ruby'}
  Plug 'sunaku/vim-ruby-minitest'
  Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby', 'yaml', 'haml', 'javascript', 'coffee', 'sass', 'scss']}
  Plug 'vim-ruby/vim-ruby', {'for': ['ruby', 'eruby']}
" }}}

" {{{ Scala
  Plug 'derekwyatt/vim-scala'
" }}}

" {{{ Sway
  Plug 'jamespeapen/swayconfig.vim'
" }}}

" Stylus {{{
  Plug 'wavded/vim-stylus'
" }}}

" Terraform {{{
  Plug 'hashivim/vim-terraform'
" }}}

" Typescript {{{
  Plug 'leafgarland/typescript-vim'
" }}}

" Tmux {{{
  Plug 'keithbsmiley/tmux.vim', {'for': 'tmux'}
" }}}

" Yaml {{{
  Plug 'chase/vim-ansible-yaml', {'for': 'yaml'}
" }}}

" }}}

" Syntax and Indentation {{{
  Plug 'bronson/vim-trailing-whitespace'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'michaeljsmith/vim-indent-object'
" }}}

" UI {{{
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'itchyny/lightline.vim'
  Plug 'konfekt/fastfold'
  Plug 'liuchengxu/vim-which-key'
  Plug 'machakann/vim-highlightedyank'
" }}}

call plug#end()
