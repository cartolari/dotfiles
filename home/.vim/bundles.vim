if has('vim_starting')
  call plug#begin('~/.vim/bundle')
endif

"Commands and mappings
Plug 'andrewradev/splitjoin.vim', {'for': [
      \ 'ruby', 'eruby', 'coffee', 'python',
      \ 'javascript', 'html', 'xml', 'css',
      \ 'scss', 'less', 'yaml', 'vim' ]}
Plug 'andrewradev/switch.vim'
Plug 'henrik/vim-indexed-search'
Plug 'kana/vim-textobj-user', {'for': 'ruby'}
Plug 'terryma/vim-multiple-cursors'
Plug 'benekastah/neomake', {'dir': '~/code/neomake/'}
Plug 'thoughtbot/vim-rspec', {'for': 'ruby'}
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/starrange'
Plug 'wolfy87/vim-enmasse', {'on': ['EnMasse', 'EnMasseVersion']}

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
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': 'yes \| ./install'}
Plug 'rking/ag.vim', {'on': ['Ag']}

"Integrations
Plug 'epeli/slimux', {'on': ['SlimuxREPLSendLine', 'SlimuxREPLSendSelection',
      \ 'SlimuxREPLConfigure', 'SlimuxShellPrompt', 'SlimuxShellLast',
      \ 'SlimuxShellRun', 'SlimuxShellConfigure']}
Plug 'tpope/vim-fugitive'

"Interface
Plug 'konfekt/fastfold'
Plug 'ap/vim-buftabline'
Plug 'christoomey/vim-tmux-navigator'
Plug 'itchyny/lightline.vim'

"Language specific
Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme']}
Plug 'ingydotnet/yaml-vim', {'for': 'yaml'}
Plug 'keithbsmiley/tmux.vim', {'for': 'tmux'}
Plug 'nelstrom/vim-textobj-rubyblock', {'for': 'ruby'}
Plug 'othree/html5.vim', {'for': 'html'}
Plug 'suan/vim-instant-markdown', {'for': 'markdown', 'do': 'npm install -g instant-markdown-d'}
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby', 'yaml', 'haml', 'javascript', 'coffee', 'sass', 'scss']}
Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme']}
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
