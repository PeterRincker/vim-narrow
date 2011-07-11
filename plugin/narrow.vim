if (exists("g:loaded_narrow") && g:loaded_narrow) || &cp
    finish
endif
let g:loaded_narrow = 1

let s:region = {}
let s:buffers = {}

function! s:fnameescape(file) abort
  if exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file," \t\n*?[{`$\\%#'\"|!<")
  endif
endfunction

function! s:Narrow()
  let a = @a
  let at = getregtype("a")
  let ve = &ve
  let &ve = ""

  try
    sil norm! gv"ay

    let region = {}
    let region.firstline = min([line("'<"), line("'>")])
    let region.lastline = max([line("'<"), line("'>")])
    let region.firstcol = min([col("'<"), col("'>")])
    let region.lastcol = max([col("'<"), col("'>")])
    let region.vmode = visualmode()
    let region.ragged = region.vmode == "\<c-v>" && str2nr(getregtype("a")[1:]) != abs(region.firstcol - region.lastcol) + 1
    let region.bufnr = bufnr('%')

    if s:Overlap(region)
        echohl WarningMsg
        echo "Regions can not overlap"
        echohl None
        return
    endif

    if !has_key(s:buffers, region.bufnr)
      let s:buffers[region.bufnr] = {}
      let s:buffers[region.bufnr].regions = {}
      let s:buffers[region.bufnr].ma = getbufvar(region.bufnr, '&ma')
      let s:buffers[region.bufnr].ro = getbufvar(region.bufnr, '&ro')
      call setbufvar(region.bufnr, '&ma', 0)
      call setbufvar(region.bufnr, '&ro', 1)
    endif
    let region.number = max(map(copy(s:buffers[region.bufnr].regions), 's:region[+v:key].number')) + 1

    " create new scratch like buffer
    if bufname(region.bufnr) == ""
      let filename = 'buf:' . region.bufnr
    else
      let filename = fnamemodify(bufname(region.bufnr), ':p')
    endif
    let filename = 'narrow://' . filename . '//' . region.number
    exe "new " . s:fnameescape(filename)
    set noswapfile
    set buftype=acwrite
    set bufhidden=wipe
    set foldcolumn=0
    set nobuflisted

    sil pu a
    sil 0d _

    let nr = bufnr('%')
    let s:region[nr] = region

    set nomod

    " Store the original buffer to buffer variable.  Could be used in the
    " statusline
    let b:narrow_bufnr = region.bufnr
    let s:buffers[region.bufnr].regions[nr] = 1

    if s:buffers[region.bufnr].ro
      echohl WarningMsg
      echo "Buffer is protected, won't be able to write changes back!"
      echohl None
      aug Narrow
        au! * <buffer>
      aug END
    endif

  finally
    let &ve = ve
    call setreg("a", a, at)
  endtry
endfunction

function! s:WriteRegion(nr)
  let region = copy(s:region[a:nr])

  if s:buffers[region.bufnr].ro
    echohl WarningMsg
    echo "Original buffer is protected, can't write changes back!"
    echohl None
    return
  endif

  let wnr = bufwinnr(region.bufnr)
  if wnr == -1
    echohl WarningMsg
    echo "Original buffer no longer exist! Aborting!"
    echohl None
    return
  endif

  let a = @a
  let at = getregtype("a")
  let lz = &lz
  let &lz = 1

  try
    exe "noa " . bufwinnr(a:nr) . "wincmd w"
    sil %yank a

    " make original buffer writable
    call setbufvar(region.bufnr, '&ma', 1)
    call setbufvar(region.bufnr, '&ro', 0)

    " move to original buffer and visually select region
    exe "noa " . wnr . "wincmd w"
    exe "keepj norm! " . region.firstline . "G"
    exe "keepj norm! " . region.firstcol . "|"
    exe "keepj norm! " . region.vmode
    exe "keepj norm! " . region.lastline . "G"
    exe "keepj norm! " . region.lastcol . "|"
    if region.ragged
      exe "keepj norm! $"
    endif

    if region.vmode == "\<c-v>" && !region.ragged
      let ve = &ve
      let &ve = "all"
    endif

    try 
      " cast @a to the proper regtype and remove trailing line break in
      " characterwise mode
      call setreg("a", region.vmode ==# 'v' ? @a[:-2] : @a, region.vmode)

      sil norm! "ap

      if exists('g:narrow_debug') && g:narrow_debug
        echo string(region)
      endif

      " save new region positions via `[ and `] marks
      let s:region[a:nr].firstline = line("'[")
      let s:region[a:nr].lastline = line("']")
      let s:region[a:nr].firstcol = col("'[")
      let s:region[a:nr].lastcol = col("']")
    finally
      if region.vmode == "\<c-v>" && !region.ragged
        let &ve = ve
      endif
    endtry

    if region.vmode == "\<c-v>"
      let width = str2nr(getregtype("a")[1:]) 
      let s:region[a:nr].lastcol = s:region[a:nr].firstcol + width - 1
    endif

    exe "noa " . bufwinnr(a:nr) . "wincmd w"

    " make original buffer unwritable again
    call setbufvar(region.bufnr, '&ma', 0)
    call setbufvar(region.bufnr, '&ro', 1)

    " check differences in lines and columns and update other narrowed regions
    " in same buffer
    " TODO: need to check for other regions that are not linewise
    if s:region[a:nr].lastline != region.lastline
      let delta =  region.lastline - s:region[a:nr].lastline
      for k in keys(s:region)
        let r = s:region[k]
        if r.bufnr == region.bufnr && k != a:nr
          if r.firstline > region.lastline
            let s:region[k].firstline -= delta
            let s:region[k].lastline -= delta
          end
        endif
      endfor
    endif
    if s:region[a:nr].lastcol != region.lastcol
      for k in keys(s:region)
        let r = s:region[k]
        if r.bufnr == region.bufnr && k != a:nr && r.vmode !=# 'V'
          if r.firstline == region.lastline && r.firstcol > region.lastcol
            let s:region[k].firstcol -= col_delta
            let s:region[k].lastcol -= col_delta
          endif
        endif
      endfor
    endif

    " Set new region buffer to unmodified as this is to be a write
    call setbufvar(a:nr, '&mod', 0)
  finally
    call setreg("a", a, at)
    let &lz = lz
  endtry
endfunction

function! s:Overlap(region)
    for k in keys(s:region)
      if s:region[k].bufnr == a:region.bufnr
        let r = s:region[k]

        " Skip all regions that do not overlap linewise
        if a:region.lastline < r.firstline || a:region.firstline > r.lastline
          continue
        endif

        if a:region.vmode != 'V' && r.vmode != 'V'

          " Skip all chaterwise regions that do not overlap because they are on
          " a single line
          if ((a:region.vmode == "v" && a:region.firstline == a:region.lastline)
                \ || (r.vmode == "v" && r.firstline == r.lastline)
                \ )
                \ && (a:region.lastcol < r.firstcol || a:region.firstcol > r.lastcol)
            continue
          endif

          if a:region.vmode == "\<c->v" && r.vmode == "\<c-v>" && (a:region.lastcol < r.firstcol || (a:region.firstcol > r.lastcol && !r.ragged))
            continue
          endif
        endif
        return 1
      endif
    endfor
    return 0
endfunction

function! s:RemoveRegion(nr)
  if !has_key(s:region, a:nr)
    return
  endif
  let region = copy(s:region[a:nr])

  call remove(s:buffers[region.bufnr].regions, a:nr)
  if len(s:buffers[region.bufnr].regions) == 0
    " restore original 'modifiable' and 'readonly' options
    call setbufvar(region.bufnr, "&ma", s:buffers[region.bufnr].ma)
    call setbufvar(region.bufnr, "&ro", s:buffers[region.bufnr].ro)

    call remove(s:buffers, region.bufnr)
  endif

  call remove(s:region, a:nr)
  call setbufvar(a:nr, '&mod', 0)
  sil! exe a:nr . "bwipe!"
endfunction

aug Narrow
  au!
  au BufReadCmd narrow://* exe "silent doau BufReadPre ".s:fnameescape(expand("<amatch>"))|exe "silent doau BufReadPost ".s:fnameescape(expand("<amatch>"))
  au BufWriteCmd narrow://* call s:WriteRegion(+expand('<abuf>'))
  au BufWipeout,BufDelete narrow://* call s:RemoveRegion(+expand('<abuf>'))
aug END

map <script> <silent> <Plug>Narrow :<c-u>call <SID>Narrow()<cr>
if !hasmapto('<Plug>Narrow', 'v')
  vmap <silent> <leader>nr <Plug>Narrow
endif
