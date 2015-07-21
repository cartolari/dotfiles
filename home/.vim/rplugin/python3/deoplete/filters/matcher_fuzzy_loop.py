import re
from .base import Base

class Filter(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.name = 'matcher_fuzzy_loop'
        self.description = 'matcher_fuzzy_loop'

    def filter(self, context):
        complete_str = context['complete_str']
        if context['ignorecase']:
            complete_str = complete_str.lower()

        input_len = len(complete_str)
        get_word = lambda w: w.lower() if context['ignorecase'] else w

        return [x for x in context['candidates'] \
                if fuzzysearch(complete_str, get_word(x['word']))]

def fuzzysearch(needle, haystack):
  hlen = len(haystack)
  nlen = len(needle)

  if (nlen > hlen):
      return False

  i = 0
  j = 0
  while (i < nlen):
    found_match = False
    while (j < hlen and not found_match):
      if (haystack[j] == needle[i]):
        found_match = True
      j += 1

    if (not found_match): return False

    i += 1
  return True
