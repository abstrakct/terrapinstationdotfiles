syntax on

set t_Co=256

if &t_Co >= 256
"        colorscheme fu
"        colorscheme tango
        colorscheme wombat256
else
        colorscheme zenburn
endif

" let mapleader=","

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

set nocompatible
set laststatus=2
set statusline=
set statusline+=%-3.3n\                      " buffer number
set statusline+=%f\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " filetype
set statusline+=%=
set statusline+=%-14(%l,%c%V%)
set statusline+=%<%P
set history=1000
set undolevels=1000
set title

if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Blue
  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
endif

set hlsearch  " highlight search
set incsearch " incremental search, search as you type
set smartcase " Ignore case when searching lowercase

set expandtab
set tabstop=8
set shiftwidth=8
set smarttab
set backspace=indent,eol,start
set showcmd
set hidden
set autoindent
set copyindent

set nowrap
set linebreak " Wrap at word
set number
set vb t_vb=  " turn off bell!
set dir=~/.vim/tmp
highlight LineNr ctermfg=lightcyan

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

" imap jj <Esc>

filetype indent plugin on
" set cindent

"au BufWinLeave juleøltest.txt mkview
"au BufWinEnter juleøltest.txt silent loadview

" au BufWritePre 2009.nanowrimo let &bex = '-' . strftime("%F-%H.%M.%S") . '~'
" au BufWinEnter 2009.nanowrimo silent loadview
" au BufWinLeave 2009.nanowrimo mkview
" au BufRead,BufNewFile *.nanowrimo set filetype=nanowrimo
" au! Syntax nanowrimo source /home/rolf/.vim/syntax/nanowrimo.vim

nnoremap <space> za


map  <F2> :tabe %:p:s,.h$,.X123X,:s,.c$,.h,:s,.X123X$,.c,<CR>
"imap <F3> <C-R>=strftime("%Y-%m-%d %H:%M")<CR>
"nmap <F3> o<C-R>=strftime("%Y-%m-%d %H:%M")<CR><Esc>
nmap <F4> g<C-G>

map <F5> 5wiEurope '72 1972-05-
map <F3> A -><Esc>

map <left>  :tabprevious<CR>
map <right> :tabnext<CR>
map <down> :bprevious<CR>
map <up> :bnext<CR>

" Convenient go-to-end-of-line-key on norwegian keyboard.
map \ $
vmap \ $

" Put norwegian keys to use :)
map ø :
map æ @

map gr gT
map g> :%s/>/->/g<CR>

" going to the next search hit centers the line
map N Nzz
map n nzz

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

noremap <C-s> <C-a>
nmap <silent> ,l :nohlsearch<CR>

" Autocommands
" " Read-only .doc through antiword
autocmd BufReadPre *.doc silent set ro
autocmd BufReadPost *.doc silent %!antiword "%"
"
" " Read-only odt/odp through odt2txt
autocmd BufReadPre *.odt,*.odp silent set ro
autocmd BufReadPost *.odt,*.odp silent %!odt2txt "%"

au FileType help nnoremap <buffer><cr> <C-]> " Enter selects subject
au FileType help nnoremap <buffer><bs> <C-T> " Backspace to go back

autocmd FileType c set omnifunc=ccomplete#Complete

" Super Tab Completion stuff
function! SuperCleverTab()
        if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
                return "\<Tab>"
        else
                if &omnifunc != ''
                        if &filetype == "c" || &filetype == "cpp"
                            return "\<C-N>"
                        else
                            return "\<C-X>\<C-O>"
                        endif
                elseif &dictionary != ''
                        return "\<C-K>"
                else
                        return "\<C-N>"
                endif
        endif
endfunction


inoremap <Tab> <C-R>=SuperCleverTab()<CR>


" Changing CaSe!
"
function! TwiddleCase(str)
        if a:str ==# toupper(a:str)
                let result = tolower(a:str)
        elseif a:str ==# tolower(a:str)
                let result = substitute(a:str,'\(\<\w\+\>\)', '\u\1', 'g')
        else
                let result = toupper(a:str)
        endif
        return result
endfunction
vnoremap <F5> ygv"=TwiddleCase(@")<CR>Pgv

