if has('vim_starting')
  call plug#begin('~/.vim/bundle')
endif

"Commands and mappings
Plug 'andrewradev/splitjoin.vim', {'for': [
      \ 'ruby', 'eruby', 'coffee', 'python',
      \ 'javascript', 'html', 'xml', 'css',
      \ 'scss', 'less', 'yaml', 'vim' ]}
Plug 'henrik/vim-indexed-search'
Plug 'kana/vim-textobj-user'
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/syntastic'
Plug 'thoughtbot/vim-rspec', {'for': 'ruby'}
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/starrange'
Plug 'wolfy87/vim-enmasse'

"Completions and snippets
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'marijnh/tern_for_vim', {'for': ['html', 'javascript'], 'do': 'npm install'}
Plug 'rstacruz/sparkup', {'for': 'html'}
Plug 'ervandew/supertab'
Plug 'sirver/ultisnips'
Plug 'tpope/vim-endwise', {'for': ['ruby', 'sh', 'zsh', 'vim', 'c', 'cpp']}

"Colorschemes
Plug 'csapprox'
Plug 'trapd00r/neverland-vim-theme'
Plug 'amdt/vim-niji', {'for': ['clojure', 'lisp', 'scheme']}

"File finders and browsers
Plug 'ctrlpvim/ctrlp.vim'
Plug 'rking/ag.vim', {'on': ['Ag']}

"Integrations
Plug 'epeli/slimux'
Plug 'tpope/vim-fugitive'

"Interface
Plug 'bling/vim-airline'
Plug 'christoomey/vim-tmux-navigator'

"Language specific
Plug 'ingydotnet/yaml-vim', {'for': 'yaml'}
Plug 'suan/vim-instant-markdown', {'for': 'markdown', 'do': 'npm install -g instant-markdown-d'}
Plug 'keithbsmiley/tmux.vim', {'for': 'tmux'}
Plug 'nelstrom/vim-textobj-rubyblock', {'for': 'ruby'}
Plug 'othree/html5.vim', {'for': 'html'}
Plug 'tpope/vim-rails'
Plug 'vim-ruby/vim-ruby', {'for': ['ruby', 'eruby']}

"Syntax and indetation
Plug 'AnsiEsc.vim', {'on': 'AnsiEsc'}
Plug 'bronson/vim-trailing-whitespace'
Plug 'drslump/vim-syntax-js', {'for': ['javascript', 'html', 'haml']}
Plug 'editorconfig/editorconfig-vim'
Plug 'ekalinin/Dockerfile.vim', {'for': 'Dockerfile'}
Plug 'Valloric/MatchTagAlways', {'for': 'html'}
Plug 'kchmck/vim-coffee-script', {'for': 'coffee'}
Plug 'vim-scripts/JavaScript-Indent', {'for': ['javascript', 'html', 'haml']}

call plug#end()
