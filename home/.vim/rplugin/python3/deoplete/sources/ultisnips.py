from .base import Base

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.mark = '[U]'
        self.matchers = ['matcher_fuzzy_loop']
        self.name = 'Ultisnips'

        self.cache = {}

    def gather_candidates(self, context):
        snips = self.vim.eval('UltiSnips#SnippetsInCurrentScope()')
        return [{ 'word': key, 'menu': snips.get(key) + '[U]' } for key in snips.keys()]
