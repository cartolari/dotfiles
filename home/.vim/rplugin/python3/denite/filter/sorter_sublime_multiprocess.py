from functools import partial
from multiprocessing import cpu_count, Pool
from unicodedata import category

from denite.filter.sorter_sublime import get_score

from .base import Base

class Filter(Base):
    def __init__(self, vim):
        super().__init__(vim)

        self.name = 'sorter_sublime_multiprocess'
        self.description = \
            'sorter for fuzzy matching like sublime text based on lib_fts using multiprocessing'
        self.pool = Pool(processes=cpu_count())

    def filter(self, context):
        if len(context['input']) == 0:
            return context['candidates']

        all_chunks = chunks(context['candidates'], cpu_count())
        all_chunks = self.pool.map(partial(score, context['input']), all_chunks)
        context['candidates'] = []
        for c in all_chunks:
            context['candidates'] += c

        return sorted(
            context['candidates'],
            key=lambda candidate: -candidate['filter__rank']
        )

def score(pattern, candidates):
    for c in candidates:
        c['filter__rank'] = get_score(pattern, c['word'])
    return candidates

def chunks(l, n):
    n = max(1, n)
    return (l[i:i+n] for i in range(0, len(l), n))
