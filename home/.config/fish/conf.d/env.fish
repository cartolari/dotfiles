set -gx XDG_BIN_HOME $HOME/.local/bin
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_LIB_HOME $HOME/.local/lib


if ! set -q XDG_RUNTIME_DIR
  if test (uname) = "Darwin"
    set -gx XDG_RUNTIME_DIR ~/Library/Caches
  else
    set -gx XDG_RUNTIME_DIR /run/user/1000
  end
end

set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
# if hash apt &> /dev/null && [[ "$(ls -di /)" != "2" ]]; then
#   # Inside a Chroot running Ubuntu, so we'll assume Chrome OS
#   export SSH_AUTH_SOCK /run/chrome/ssh-agent.socket
# end

set -gx EDITOR nvim

set -gx DOCKER_CONFIG $XDG_CONFIG_HOME/docker
set -gx GEMRC $XDG_CONFIG_HOME/gemrc
set -gx LESSHISTFILE $XDG_DATA_HOME/lesshst
set -gx VAGRANT_HOME $XDG_CONFIG_HOME/vagrant

set -gx FZF_DEFAULT_COMMAND 'ag --hidden -l -g ""'
set -gx FZF_CTRL_T_COMMAND 'ag --hidden -l -g ""'

set -gx LOCALE_ARCHIVE /usr/lib64/locale/locale-archive

set -gx GOPATH ~/go
set -gx NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/npm/config"
set -gx NVIM_TUI_ENABLE_TRUE_COLOR 1

fish_add_path -g $GOPATH/bin$file
fish_add_path -g $HOME/.local/share/gem/ruby/3.0.0/bin
fish_add_path -g $HOME/bin
fish_add_path -g $HOME/scripts
fish_add_path -g $XDG_DATA_HOME/npm/bin
fish_add_path -g /usr/bin/core_perl
fish_add_path -g $HOME/.dotnet/tools
fish_add_path -g ~/.cargo/bin

set -gx MOZ_ENABLE_WAYLAND 1
set -gx QT_QPA_PLATFORM wayland
set -gx _JAVA_AWT_WM_NONREPARENTING 1

set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")

set -gx VIMINIT 'let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

set -gx NODE_AUTH_TOKEN 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

set -gx JAVA_HOME /usr/lib/jvm/default
set -gx ANDROID_HOME ~/Android/Sdk

fish_add_path -g $ANDROID_HOME/emulator
fish_add_path -g $ANDROID_HOME/tools
fish_add_path -g $ANDROID_HOME/tools/bin
fish_add_path -g $ANDROID_HOME/platform-tools

set -gx AWS_DEFAULT_PROFILE onyx-production
