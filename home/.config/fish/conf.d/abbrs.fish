function last_history_item
  echo $history[1]
end

# Waiting for https://github.com/fish-shell/fish-shell/pull/9313
# abbr -a !! --position anywhere --function last_history_item --quiet
