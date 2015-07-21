
import re
import operator
import functools
from .base import Base

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.buffers = {}
        self.mark = '[B]'
        self.matchers = ['matcher_fuzzy_loop']
        self.name = 'buffer'

    def gather_candidates(self, context):
        current_candidates = []
        p = re.compile(context['keyword_patterns'])

        for buf in self.vim.buffers:
            for l in buf:
                    current_candidates += p.findall(l)
        self.buffers[self.vim.current.buffer.number] = {
            'filetype': context['filetype'],
            'candidates': current_candidates,
        }

        return [{ 'word': x } for x in
                functools.reduce(operator.add, [
                     x['candidates'] for x in self.buffers.values()
                     if x['filetype'] == context['filetype']])]

