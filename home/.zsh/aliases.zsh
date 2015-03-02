alias ack='ack-grep'
alias cls='printf "\033c"'
alias clear='echo Use Ctrl-l to clear the screen'
alias cls='echo Use Ctrl-l to clear the screen'
alias ccat='pygmentize -g -O style=monokai -f console256 -g'
alias tmux='TERM=screen-256color-bce tmux'
alias vi='nvim'
alias vim='nvim'

# Apt
alias apt-install='sudo apt-get install'
alias apt-update='sudo apt-get update'
alias apt-ppa='sudo add-apt-repository'
alias apt-search='apt-cache search'
alias apt-show='apt-cache show'
alias apt-remove='sudo apt-get remove'
alias apt-purge='sudo apt-get purge'
alias apt-upgrade='sudo apt-get upgrade'
alias apt-dist-upgrade='sudo apt-get dist-upgrade'

# Docker
alias d='docker'
alias dc='docker-compose'
alias fig='echo "Use docker-compose"'
alias dup='docker-compose up -d'
alias drun='docker-compose run'
alias dstop='docker-compose stop'
alias druns='docker-compose run --service-ports'
alias dkill='docker-compose kill'
alias drm='docker-compose rm'
alias dps='docker-compose ps'
alias dlogs='docker-compose logs'
alias drunb='docker-compose run web bash'
alias drunsb='docker-compose run --service-ports web bash'
alias start_dns='source ~/start_dns.sh'

# ssh
alias s='ssh'

# Git
alias gcln='git clone'
