
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
    "Plug 'francoiscabrol/ranger.vim'
    Plug 'vimwiki/vimwiki'
    Plug 'kien/rainbow_parentheses.vim'
    Plug 'mhinz/vim-startify'
    Plug 'flazz/vim-colorschemes'
  "  Plug 'vim-airline/vim-airline'
  "  Plug 'vim-airline/vim-airline-themes'
    Plug 'vim-utils/vim-man'
    "Plug 'sheerun/vim-polyglot'
    Plug 'chun-yang/auto-pairs'
    Plug 'chrisbra/colorizer'
    Plug 'vitalk/vim-simple-todo'
    Plug 'ajh17/vimcompletesme'
    Plug 'itchyny/lightline.vim'
    Plug 'jacoborus/tender.vim'
    Plug 'lilydjwg/colorizer'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'airblade/vim-gitgutter'
    Plug 'tpope/vim-markdown'
    Plug 'plasticboy/vim-markdown'
    Plug 'suan/vim-instant-markdown'
    Plug 'tpope/vim-vinegar'
    Plug 'sainnhe/everforest'
    Plug 'mcchrish/nnn.vim'
    Plug 'vim-latex/vim-latex'
    Plug 'brennier/quicktex'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'honza/vim-snippets'
    Plug 'chiel92/vim-autoformat'
    Plug 'ludovicchabant/vim-gutentags'



  "  Plug 'lervag/vimtex'
   " Plug 'sirver/ultisnips'
   " Plug 'KeitaNakamura/tex-conceal.vim'



" Initialize plugin system
call plug#end()

"colorscheme monokai-chris
colorscheme OceanicNext
