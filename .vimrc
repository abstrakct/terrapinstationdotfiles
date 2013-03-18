syntax on

set t_Co=256

"if &t_Co >= 256
"        colorscheme fu
"        colorscheme tango
if has("gui_running")
        colorscheme candycode-custom
        "colorscheme nucolors
endif

if !has("gui_running")
        colorscheme candycode " wombat256
endif

"else
"        colorscheme zenburn
"endif

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
set novisualbell
set dir=~/.vim/tmp
highlight LineNr ctermfg=lightcyan

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

imap jj <Esc>

filetype indent plugin on
" set cindent

"au BufWinLeave juleøltest.txt mkview
"au BufWinEnter juleøltest.txt silent loadview

" au BufWritePre 2009.nanowrimo let &bex = '-' . strftime("%F-%H.%M.%S") . '~'
" au BufWinEnter 2009.nanowrimo silent loadview
" au BufWinLeave 2009.nanowrimo mkview
" au BufRead,BufNewFile *.nanowrimo set filetype=nanowrimo
" au! Syntax nanowrimo source /home/rolf/.vim/syntax/nanowrimo.vim


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
"function! SuperCleverTab()
"        if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
"                return "\<Tab>"
"        else
"                if &omnifunc != ''
"                        if &filetype == "c" || &filetype == "cpp"
"                            return "\<C-N>"
"                        else
"                            return "\<C-X>\<C-O>"
"                        endif
"                elseif &dictionary != ''
"                        return "\<C-K>"
"                else
"                        return "\<C-N>"
"                endif
"        endif
"endfunction

" inoremap <Tab> <C-R>=SuperCleverTab()<CR>


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

function FunctionHeading()
  let s:line=line(".")
  call setline(s:line,"/*********************************************")
  call append(s:line,"* Description - ")
  call append(s:line+1,"* Author - RK")
  call append(s:line+2,"* Date - ".strftime("%b %d %Y"))
  call append(s:line+3,"* *******************************************/")
  unlet s:line
endfunction


" **** KEYMAPPINGS ****
nnoremap <space> za

" Put norwegian keys to use :)
map ø :
map æ @

" tab navigation made easier
map gr gT

" for live show text files
map g> :%s/>/->/g<CR>

" going to the next search hit centers the line
map N Nzz
map n nzz

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" noremap <C-s> <C-a>
nmap <silent> ,l :nohlsearch<CR>


" *** FUNCTION KEYS ***

" not perfect, but functional way of opening corresponding .c/.h file in a new
" tab
map  <F2> :tabe %:p:s,.h$,.X123X,:s,.c$,.h,:s,.X123X$,.c,<CR>

imap <F4> <Esc>mz:execute FunctionHeading()<CR>`zjA
nmap <F4> mz:execute FunctionHeading()<CR>`zjA
map <F5> :make<CR>
nmap <F6> :make<CR>:!heritage<CR>
imap <F6> <ESC>:make<CR>:!heritage<CR>
vnoremap <F5> ygv"=TwiddleCase(@")<CR>Pgv
"map <F6> :Dox<CR>

" auto-fix charlie miller's text files for personal preference! ;)
nmap <F7> wwxxhxi<CR><Esc>/charliemiller<CR>J/Encore<CR>kdd

" Toggle taglist window
nnoremap <silent> <F8> :TlistToggle<CR>

" remember that F12 = toggle project window

map <left>  :tabprevious<CR>
map <right> :tabnext<CR>
map <down> :bprevious<CR>
map <up> :bnext<CR>

nmap <S-left> :tabm -1<CR>
nmap <S-right> :tabm +1<CR>
"map <F5> 5wiEurope '72 1972-05-
"map <F3> A -><Esc>j

" Convenient go-to-end-of-line-key on norwegian keyboard.
" map 0 $

" programming shortcuts/stuff
" inoremap { {<CR>}<ESC>O
" inoremap {<CR> {
" inoremap (<CR> ()<Left>
" taglist options
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 60
let Tlist_Use_SingleClick = 1
let Tlist_Show_Menu = 1

" settings for plugin "project"
let g:proj_flags="imstgS"

" Help delete character if it is 'empty space'
" stolen from Vim manual
" function! Eatchar()
"   let c = nr2char(getchar())
"   return (c =~ '\s') ? '' : c
" endfunction
" 
" " Replace abbreviation if we're not in comment or other unwanted places
" " stolen from Luc Hermitte's excellent http://hermitte.free.fr/vim/
" function! MapNoContext(key, seq)
"   let syn = synIDattr(synID(line('.'),col('.')-1,1),'name')
"   if syn =~? 'comment\|string\|character\|doxygen'
"     return a:key
"   else
"     exe 'return "' .
"     \ substitute( a:seq, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) . '"'
"   endif
" endfunction
" 
" " Create abbreviation suitable for MapNoContext
" function! Iab (ab, full)
"   exe "iab <silent> <buffer> ".a:ab." <C-R>=MapNoContext('".
"     \ a:ab."', '".escape (a:full.'<C-R>=Eatchar()<CR>', '<>\"').
"     \"')<CR>"
" endfunction
" 
" call Iab('#d', '#define ')
" call Iab('#i', '#include <><Left>')
" call Iab('#I', '#include ""<Left>')
" call Iab('printf', 'printf("\n");<C-O>?\<CR>')
" call Iab('if', 'if() {<CR>}<Left><C-O>?)<CR>')
" call Iab('for', 'for(;;) {<CR>}<C-O>?;;<CR>')
" call Iab('while', 'while() {<CR>}<C-O>?)<CR>')
" call Iab('else', 'else {<CR>x;<CR>}<C-O>?x;<CR><Del><Del>')
" call Iab('ifelse', 'if() {<CR>} else {<CR>}<C-O>?)<CR>')
" call Iab('intmain', 'int main (int argc, char **argv)<CR>'.
"  \ '{<CR>x;<CR>return 0;<CR>}<CR><C-O>?x;<CR><Del><Del>')

" nmap _if ofprintf(0<C-d>stderr, "DEBUG: %s:%d - \n", __FILE__, __LINE__);<Esc>F\i



" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
"
" Note: Must allow nesting of autocmds to enable any customizations for quickfix
" buffers.
" Note: Normally, :cwindow jumps to the quickfix window if the command opens it
" (but not if it's already open). However, as part of the autocmd, this doesn't
" seem to happen.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
