" Use g++ when no make file is available
if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=g++\ -Wall\ -Wextra\ -o\ %<\ %
endif
