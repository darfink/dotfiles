function! GetRunningOS()
  if has("win32")
    return "win"
  endif
  if has("unix")
    if system('uname')=~'Darwin'
      return "osx"
    else
      return "linux"
    endif
  endif
endfunction

function! BuildVimProc(info)
  let os = GetRunningOS()
  if os == "win"
    call system('tools\\update-dll-mingw')
  else if os == "osx"
    call system('make -f make_mac.mak')
  else
    call system('make -f make_unix.mak')
  endif
endfunction

function! InitializeDirectories()
  let directoryList = { 
  \   'backup': 'backupdir', 
  \   'view': 'viewdir', 
  \   'swap': 'directory', 
  \   'undo': 'undodir'
  \ }

  for [dirname, settingname] in items(directoryList)
    let directory = '~/.vim/' . dirname . '/'

    if exists("*mkdir")
      if !isdirectory(directory)
	call mkdir(directory)
      endif
    endif

    if !isdirectory(directory)
      echo "Warning: Unable to create backup directory: " . directory
      echo "Try: mkdir -p " . directory
    else
      " Notice that we append an additional slash
      " See: http://stackoverflow.com/a/15317146
      exec "set " . settingname . "=" . directory . "/"
    endif
  endfor
endfunction

function! Multiple_cursors_before()
  exe 'NeoCompleteLock'
  echo 'Disabled autocomplete'
endfunction

function! Multiple_cursors_after()
  exe 'NeoCompleteUnlock'
  echo 'Enabled autocomplete'
endfunction