unsetopt nomatch

ZSH_THEME="avit"
export UPDATE_ZSH_DAYS=7
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

ZSH_CUSTOM=~/.zsh

plugins=(command-not-found git vagrant docker autojump colored-man docker-compose)

export ZSH=~/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/home/bruno/.npm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/java/jre1.7.0_60/bin:/opt/eb/eb/linux/python2.7/"
[[ -f /usr/local/share/chruby/chruby.sh ]] && source /usr/local/share/chruby/chruby.sh
[[ -f /usr/share/chruby/chruby.sh ]] && source /usr/share/chruby/chruby.sh
chruby ruby-2.3.0

# Disable software control flow
stty -ixon

export EDITOR=nvim
export PATH=$PATH:/opt/adt/tools
export PATH=$PATH:~/.local/bin
export PATH=$PATH:/opt/maven/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:/home/bruno/go/bin

export ANDROID_HOME=/opt/adt
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
