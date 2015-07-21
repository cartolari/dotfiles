from .base import Base

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.mark = '[S]'
        self.matchers = ['matcher_fuzzy_loop']
        self.name = 'Syntax'

        self.cache = {}

    def gather_candidates(self, context):
        syntax_words = self.vim.eval('syntaxcomplete#Complete(0, "")')
        return [{ 'word': key } for key in syntax_words]
