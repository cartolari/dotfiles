from .base import Base

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.mark = '[T]'
        self.matchers = ['matcher_fuzzy_loop']
        self.name = 'Tmux Complete'

        self.cache = {}

    def gather_candidates(self, context):
        tmux_completions = self.vim.eval('tmuxcomplete#complete(0, "")')
        return [{ 'word': x } for x in tmux_completions]
