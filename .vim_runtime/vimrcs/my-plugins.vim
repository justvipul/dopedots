" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
    " Any valid git URL is allowed
    "Plug 'https://github.com/junegunn/vim-github-dashboard.git'
    " On-demand loading
    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
    " Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
    Plug 'fatih/vim-go', { 'tag': '*' }
    "add icons
    Plug 'ryanoasis/vim-devicons'
    Plug 'francoiscabrol/ranger.vim'
    Plug 'vimwiki/vimwiki'
    Plug 'kien/rainbow_parentheses.vim'
    Plug 'mhinz/vim-startify'
    Plug 'flazz/vim-colorschemes'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'vim-utils/vim-man'
    Plug 'sheerun/vim-polyglot'
    Plug 'chun-yang/auto-pairs'
    Plug 'chrisbra/colorizer'
    Plug 'vitalk/vim-simple-todo'





" Initialize plugin system
call plug#end()
