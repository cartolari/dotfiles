unsetopt nomatch

ZSH_THEME="agnoster"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
export UPDATE_ZSH_DAYS=7

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
  zsh-completions
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

export GOPATH=~/go
export EDITOR=nvim
export PATH=$PATH:/home/bruno/bin
export PATH=$PATH:/home/bruno/scripts
export PATH=$PATH:$GOPATH/bin
if [ -e /home/bruno/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bruno/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

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

eval $(keychain --quiet --eval --noask --agents ssh)
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
