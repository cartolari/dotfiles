unsetopt nomatch

ZSH_THEME="avit"
export UPDATE_ZSH_DAYS=7
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

ZSH_CUSTOM=~/.zsh

plugins=(command-not-found git vagrant docker autojump)

export ZSH=~/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/home/bruno/.npm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/java/jre1.7.0_60/bin:/opt/eb/eb/linux/python2.7/"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# Disable software control flow
stty -ixon

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

export EDITOR=vim
export PATH=$PATH:/opt/adt/tools

if [[ -z "$TMUX" ]]
then
  tmux attach-session -t "$USER" || tmux new-session -s "$USER"
fi
