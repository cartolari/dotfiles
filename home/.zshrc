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

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/doc/find-the-command/ftc.zsh

# Restore ALT-C binding overwritten by FZF
bindkey '\ec' capitalize-word

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' check-for-staged-changes true
zstyle ':vcs_info:*' formats '%F{white}[%F{green}%b%F{white}]%F{yellow} %u%c%f ' 'zsh: %r'
zstyle ':vcs_info:*' stagedstr '✚'
zstyle ':vcs_info:*' unstagedstr '●'
zstyle ':prompt:grml:left:setup' items rc change-root path vcs percent
