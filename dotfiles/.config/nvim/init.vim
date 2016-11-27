set backup             " keep a backup file (restore to previous version)
set undofile           " keep an undo file (undo changes after closing)
set ruler              " show the cursor position all the time
set showcmd            " display incomplete commands


" Don't use Ex mode, use Q for formatting
" noremap Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Switch syntax highlighting on
syntax on

" Enable file type detection.
" Use the default filetype settings, so that mail gets 'textwidth' set to 72,
" 'cindent' is on in C files, etc.
" Also load indent files, to automatically do language-dependent indenting.
filetype plugin indent on

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
  autocmd!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
 "  autocmd BufReadPost *
 "    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
 "    \   execute "normal! g`\"" |
 "    \ endif

augroup END

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set buftype=nofile | read ++edit # | 0d_ | diffthis
                 \ | wincmd p | diffthis
endif

let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\}

"KBINDINGS
let maplocalleader="\<space>"
let mapleader="\<space>"
let $FZF_DEFAULT_COMMAND= 'ag -l --hidden -g ""'

" FZF vs ctrlp"
" nnoremap - :FZF<CR>
nnoremap - :CtrlP<CR>
""""""""""""""""

nnoremap _ :Ack 

nnoremap <bs> <C-w>h
nnoremap <C-J> <C-w>j
nnoremap <C-K> <C-w>k
nnoremap <C-L> <C-w>l
nnoremap <C-Q> <C-w>q

nnoremap <C-U> 10k
nnoremap <C-D> 10j

"JS Keybindings
nnoremap <localleader>fd :TernDef<CR>
nnoremap <localleader>fdp :TernDefPreview<CR>
nnoremap <localleader>fds :TernDefSplit<CR>
nnoremap <localleader>fdt :TernDefTab<CR>

nnoremap <localleader>fd :TernDef<CR>
nnoremap <localleader>fdp :TernDefPreview<CR>
nnoremap <localleader>fds :TernDefSplit<CR>
nnoremap <localleader>fdt :TernDefTab<CR>

nnoremap <localleader>fdo :TernDoc<CR>
nnoremap <localleader>fdob :TernDocBrowse<CR>

nnoremap <localleader>fr :TernRefs<CR>
nnoremap <localleader>ft :TernTypes<CR>
nnoremap <localleader>frn :TernRename<CR>

inoremap <C-o> <C-x><C-o>

let g:notes_bold_cterm = 'reverse'
let g:notes_italic_cterm = 'underline'

"Plugins
call plug#begin('~/.config/nvim/plugged')


" Plug 'flazz/vim-colorschemes'
" Plug 'mtscout6/syntastic-local-eslint.vim'
" Plug 'scrooloose/syntastic'
Plug 'Shougo/vimproc.vim'
Plug 'Shougo/vimshell.vim'
Plug 'SirVer/ultisnips'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'jrosiek/vim-mark'
Plug 'junegunn/fzf', {'do': './install --all' }
Plug 'mattn/emmet-vim'
Plug 'mileszs/ack.vim'
Plug 'mustache/vim-mustache-handlebars'
Plug 'mxw/vim-jsx'
Plug 'neomake/neomake'
Plug 'ntpeters/vim-better-whitespace'
Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdtree'
Plug 'ternjs/tern_for_vim'
Plug 'tomasr/molokai'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/restore_view.vim'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-notes'

call plug#end()

"Colorz
color molokai

let g:jsx_ext_required = 0

"neomake
"
nmap <Leader>o :lopen<CR>      " open location window
nmap <Leader>c :lclose<CR>     " close location window
nmap <Leader>, :ll<CR>         " go to current error/warning
nmap <Leader>n :lnext<CR>      " next error/warning
nmap <Leader>p :lprev<CR>      " previous error/warning
let g:neomake_place_signs = 1
let g:neomake_javascript_enabled_markers = ['eslint']
let g:neomake_javascript_eslint_exe = $PWD . '/node_modules/.bin/eslint'
let g:neomake_javascript_eslint_args = ['-f', 'compact']
autocmd! BufWritePost * Neomake

let g:notes_directories = ['~/Google\ Drive/notes']

" The Silver Searcher in place of ack...
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

set history=999
set tabstop=4
set shiftwidth=4
set expandtab
set number
set ruler
set list
set nowrapscan
set scrolloff=10
set nowrap

set statusline=%f\ %m%r%y%=%c,%l/%L\ %P
set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn

set splitright
set splitbelow
set confirm
set hidden

set smartcase

" from tutorial
set shiftround

" change word under cursor to uppercase
" inoremap <c-?> <esc>viwUgi
" nnoremap <c-?> viwUe

let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\}

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" edit vimrc
nnoremap <leader>ek :vsp ~/Library/Application\ Support/Karabiner/private.xml<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>sv :so $MYVIMRC<cr>
nnoremap <leader>pi :PlugInstall<cr>
function! DiffUnified()
  let diffexpr="diff -Nuar"
  let bname=bufname("")
  let origtemp=0
  " Case 1: File has a filename and is not modified
  if !&modified && !empty(bname)
    let tempfile=0
    let origFile=bname.".orig"
  else
    " Case 2: File has a filename and is modified
    if &modified && !empty(bname)
      if !filereadable(bname.".orig")
        sp
        enew
        r #
        0d
        let tempfile2=tempname()
        exe ":sil w! " .tempfile2
        wincmd q
        let origtemp=1
        wincmd p
      endif
      let origFile=tempfile2
      " Case 2: File is new and is modified
    else
      if &modified
        let origFile=bname.".orig"
      else
        let origFile=""
      endif
    endif
    let bname=tempname()
    exe ":sil w! ".bname
    let tempfile=1
  endif
  try
    if !filereadable(origFile)
      let origFile=input("With which file to diff?: ","","file")
    endif
    if !filereadable(bname)
      exe ":sil w! ".bname
    endif
    if empty(origFile)
      throw "nofile"
    endif
    exe "sil sp"
    exe "enew"
    set bt=nofile
    exe "sil r!".diffexpr." ".origFile." ".bname
    exe "0d_"
    exe "set ft=diff"
    " Clean up temporary files
    if  tempfile == 1
      exe "sil :!rm -f ". bname
      let tempfile=0
    endif
    if origtemp == 1
      exe "sil :!rm -f ". origFile
      let origtemp=0
    endif
  catch
  endtry
endf

set errorformat+=%f

function! TryRemoveDiff()
    try | execute ':bd fugitive' | catch | | endtry
endf
command! -nargs=* Trd call TryRemoveDiff()

function! FileDiff(...)
    let branch='HEAD'
    if a:0
        echom a:1
        let branch=a:1
    endif
    cgetexpr system('git diff --name-only ' . branch)
    copen
    execute 'nnoremap <buffer> <c-d> :Trd<cr> <cr> :Gdiff ' . branch . '<cr>'
endf
command! -nargs=* Filediff call FileDiff(<f-args>)

function! FileDiffCommon(...)
    let branch='MASTER'
    if a:0
        echom a:1
        let branch=a:1
    endif
    echom 'Listing changed files from ' . branch . '...head'
    cgetexpr system('git diff --name-only ' . branch . '...head')
    copen
    execute 'nnoremap <buffer> <c-d> :Trd<cr> <cr> :Gdiff ' . branch . '<cr>'
endf
command! -nargs=* Fdc call FileDiffCommon(<f-args>)

function! BranchTodos(...)
    let branch='MASTER'
    if a:0
        echom a:1
        let branch=a:1
    endif
    execute ':Ack "TODO" ' . system("git diff --name-only " . branch . "...head | tr '\\n' ' '")
endf
command! -nargs=* Btodo call BranchTodos(<f-args>)

set foldmethod=indent
set foldlevelstart=1
set foldnestmax=10
