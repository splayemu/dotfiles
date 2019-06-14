syntax on
colorscheme desert
set number
set smartindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set paste

"responsible for crazy tab stuff
set path=.,,**

fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

autocmd FileType c,cpp,java,php,ruby,python,javascript autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()


