# Path to your oh-my-zsh installation.export ZSH=$HOME/.oh-my-zsh

unsetopt nomatch

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="avit"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git rails ruby command-not-found gem vagrant docker node npm bower)

export ZSH=~/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/home/bruno/npm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/java/jre1.7.0_60/bin:/opt/eb/eb/linux/python2.7/"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Disable software control flow
stty -ixon

# Enable completion for tmuxinator
tmuxinator_completion_file=$(ruby -e 'puts Gem::Specification.find_by_name("tmuxinator").gem_dir')/completion/tmuxinator.zsh
if [ -r $tmuxinator_completion_file  ]; then
  source $tmuxinator_completion_file
fi

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
bindkey -v

# Fixes the way zle handles <Esc>/
vi-search-fix() {
  zle vi-cmd-mode
  zle .vi-history-search-backward
}
autoload vi-search-fix
zle -N vi-search-fix
bindkey -M viins '\e/' vi-search-fix

bindkey jk vi-cmd-mode
bindkey "^R" history-incremental-search-backward
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward
bindkey "^Y" accept-and-hold
bindkey "^[OA" up-line-or-search
bindkey "^[OB" down-line-or-search

alias ack='ack-grep'
alias cls='printf "\033c"'
alias ccat='pygmentize -g -O style=monokai -f console256 -g'
alias tmux='TERM=screen-256color-bce tmux'

export EDITOR=vim
export M2_HOME=/opt/apache-maven-3.2.3
export M2=$M2_HOME:/bin
export PATH=$HOME/npm/bin:$PATH
export PATH=$M2:$PATH
export RSENSE_HOME=/opt/rsense-0.3
export PATH=$PATH:/opt/adt/sdk/tools/:/opt/adt/sdk/platform-tools/

if [[ -z "$TMUX" ]]
then
  tmux attach-session -t "$USER" || tmux new-session -s "$USER"
fi
