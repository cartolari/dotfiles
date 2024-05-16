function source_if_exists
  set file $argv[1]

  if test -f "$file"
    source "$file" &> /dev/null
  end
end

set will_run_specific_command 0
if test -f /proc/$fish_pid/cmdline
  awk 'BEGIN { RS="\0"; } /^-c$/ { found=1 } END { if (found) exit 1 }' /proc/$fish_pid/cmdline
  set will_run_specific_command $status
end

if status is-interactive; and test $will_run_specific_command -eq 0
  if ! set -q TMUX
    tmux attach-session -t default || tmux new-session -s default
  end

  source_if_exists /usr/share/fish/functions/autojump.fish
  source_if_exists /usr/share/autojump/autojump.fish
  source_if_exists /usr/share/fish/vendor_functions.d/fzf_key_bindings.fish
  source_if_exists /etc/fish/completions/rclone.fish
end

function fish_user_key_bindings
	fzf_key_bindings
end

set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_hide_untrackedfiles 1

set -g __fish_git_prompt_color_branch magenta
set -g __fish_git_prompt_showupstream "informative"
set -g __fish_git_prompt_char_upstream_ahead "↑"
set -g __fish_git_prompt_char_upstream_behind "↓"
set -g __fish_git_prompt_char_upstream_prefix ""

set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_dirtystate "✚"
set -g __fish_git_prompt_char_untrackedfiles "…"
set -g __fish_git_prompt_char_conflictedstate "✖"
set -g __fish_git_prompt_char_cleanstate "✔"

set -g __fish_git_prompt_color_dirtystate yellow
set -g __fish_git_prompt_color_stagedstate green
set -g __fish_git_prompt_color_invalidstate red
set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
set -g __fish_git_prompt_color_cleanstate green

set fish_greeting
