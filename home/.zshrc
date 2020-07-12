if ! type isgrml > /dev/null; then
  [[ -e /etc/zsh/zshrc ]] && source /etc/zsh/zshrc
fi

if [[ -z $DISPLAY  ]] && [[ $(tty) = /dev/tty1  ]]; then
  exec sway
fi

export PATH=$HOME/.local/bin:$PATH
fc -p $XDG_DATA_HOME/zsh_history

source $XDG_CONFIG_HOME/zsh-custom/aliases.zsh

# Disable software control flow
stty -ixon

if [[ -z "$TMUX" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi
if [[ -n ${INSIDE_EMACS} ]]; then
  # This shell runs inside an Emacs *shell*/*term* buffer.
  unsetopt zle
fi

autoload -Uz bracketed-paste-magic
autoload -Uz bracketed-paste-url-magic
autoload -Uz url-quote-magic
autoload -Uz select-word-style
zle -N bracketed-paste bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-url-magic
zle -N self-insert url-quote-magic
select-word-style bash

source_if_exists() {
  local file=$1

  if [[ -f "$file" ]]; then
    source "$file" &> /dev/null
  fi
}
source_if_exists /usr/share/fzf/completion.zsh
source_if_exists /usr/share/fzf/key-bindings.zsh
source_if_exists /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source_if_exists /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source_if_exists /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source_if_exists /usr/share/doc/find-the-command/ftc.zsh
source_if_exists /usr/share/autojump/autojump.zsh

autoload -U compinit && compinit

# Restore ALT-C binding overwritten by FZF
bindkey '\ec' capitalize-word

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' check-for-staged-changes true
zstyle ':vcs_info:*' stagedstr '%F{green}●'
zstyle ':vcs_info:*' unstagedstr '%F{yellow}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git svn
precmd () {
  if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
    zstyle ':vcs_info:*' formats '[%F{green}%b%c%u%F{white}]'
  } else {
    zstyle ':vcs_info:*' formats '[%F{green}%b%c%u%F{blue}●%F{white}]'
  }

  vcs_info
}

zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
autoload -Uz compinit
compinit

autoload -U bashcompinit
bashcompinit

complete -C aws_completer aws
export LS_COLORS=':ow=01;04;32'
