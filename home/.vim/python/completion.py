from operator import itemgetter
import re
import vim

def words():
  p = re.compile('[a-zA-Z_]\w*')

  found = []
  for buf in vim.buffers:
    for line in buf:
      found += p.findall(line)
  return found

def unique_words():
  return list(set(words()))

def complete_local(search, items):
  search_lower = search.lower()
  p = re.compile('.*'.join(search_lower))

  matched = [x for x in items if p.match(x['word'].lower())]
  sorted_matches = sorted(matched, key=itemgetter('word'))
  return sorted_matches[:100]
