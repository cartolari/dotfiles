unsetopt nomatch

export UPDATE_ZSH_DAYS=7
ZSH_THEME="agnoster"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

ZSH_CUSTOM=~/.zsh

plugins=(
  autojump
  aws
  colored-man-pages
  command-not-found
  docker
  docker-compose
  git
  vagrant
)

export ZSH=~/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

source ~/.azure-completion.zsh

# User configuration
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$PATH:/home/bruno/.npm/bin
export PATH=$PATH:/home/bruno/.local/bin

# Disable software control flow
stty -ixon

export EDITOR=nvim
export PATH=$PATH:/home/bruno/bin
export PATH=$PATH:/home/bruno/scripts

export GOPATH=~/go

if [[ -z "$TMUX" ]]
then
  tmux attach-session -t "$USER" || tmux new-session -s "$USER"
fi
if [[ -n ${INSIDE_EMACS} ]]; then
  # This shell runs inside an Emacs *shell*/*term* buffer.
  unsetopt zle
fi
export FZF_DEFAULT_COMMAND='ag -l -g ""'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVIM_TUI_ENABLE_TRUE_COLOR=1
