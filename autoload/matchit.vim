if exists('g:autoloaded_matchit')
    finish
endif
let g:autoloaded_matchit = 1

" FIXMES / TODOS {{{1

" FIXME:
"         let b:match_words .= ',\(foo\):end\1,\(bar\):end\1'
"         foo
"             bar
"                 hello
"                 world
"             endbar
"         endfoo
"
" Source the assignment.
" Position the cursor on `bar`    and hit `]%` → `endbar`    ✔
" Position the cursor on `endbar` and hit `]%` → ø           ✘ (should move onto `endfoo`)
"                         ^^^
" Actually, the problem only occurs when the cursor is on `end`, not `bar`.
"
" Also, source this:
"
"         let b:match_words = 'foo:endfoo,bar:endbar'
"
" … then hit `]%` on `foo`: the cursor doesn't move. It should go onto `endfoo`.
" The issue seems related to `searchpair()`. Type this while on `foo`:
"
"       echo searchpair('foo\|bar', '', 'endfoo\|endbar')

" FIXME:
" `[%` (&friends) should ignore triple curly braces
" They don't, probably because of 'mps' which contains `{:}`.
"
" To match a curly brace outside a folding marker, use these patterns:
"
"     \%({\@<={{\@=\)\@!\&\%({{\)\@<!{\%({{\)\@!
"     \%(}\@<=}}\@=\)\@!\&\%(}}\)\@<!}\%(}}\)\@!

" TODO:
"
" “How can i highlight matching names defined by matchit?“:
"
"         https://vi.stackexchange.com/q/8707/13370
"
" Implement a mapping, lhs = z%

" TODO:
" We should store the words in a list not in a string.
" This would allow us to get rid of things like `!empty(…) ? ',' : ''`

" TODO:
" Look at the comments in the original plugin. Search for `Autocomplete()`.
" The author wanted to implement a function which completes a word.
" If it's useful, try to finish what he began. Or re-implement it entirely.

" TODO:
" Add support for multi-line patterns for b:match_words.
" This would require an option:  how many lines to scan (default 1).
" This would be useful for Python, maybe also for *ML.
"
" TODO:  Allow:
"
"         let b:match_words = '\v((foo)(bar)):\3\2:end\1'
"
" TODO:  Make backrefs safer by using '\V' (very no-magic).
"
" TODO:
" make `multi()` pass the  middle part of a group of  words to `searchpair()` as
" its 2nd argument.


" Functions {{{1
fu! s:choose(patterns, string, comma, branch, prefix, suffix, ...) abort "{{{2
    " Assume a:comma = ",".  Then the format for a:patterns and a:1 is
    "   a:patterns = "<pat1>,<pat2>,..."
    "   a:1 = "<alt1>,<alt2>,..."
    " If <patn> is the first pattern that matches a:string then return <patn>
    " if no optional arguments are given; return <patn>,<altn> if a:1 is given.

    let tail = (a:patterns =~ a:comma.'$' ? a:patterns : a:patterns . a:comma)
    let i    = matchend(tail, s:even_backslash.a:comma)
    if a:0
        let alttail = (a:1 =~ a:comma.'$' ? a:1 : a:1 . a:comma)
        let j = matchend(alttail, s:even_backslash.a:comma)
    endif

    let current = strpart(tail, 0, i-1)
    if a:branch == ''
        let currpat = current
    else
        let currpat = substitute(current, s:even_backslash.a:branch, '\\|', 'g')
    endif

    while a:string !~ a:prefix . currpat . a:suffix
        let tail = strpart(tail, i)
        let i    = matchend(tail, s:even_backslash.a:comma)
        if i == -1
            return -1
        endif

        let current = strpart(tail, 0, i-1)
        if a:branch == ''
            let currpat = current
        else
            let currpat = substitute(current, s:even_backslash.a:branch, '\\|', 'g')
        endif

        if a:0
            let alttail = strpart(alttail, j)
            let j       = matchend(alttail, s:even_backslash.a:comma)
        endif
    endwhile

    if a:0
        let current = current.a:comma.strpart(alttail, 0, j-1)
    endif

    return current
endfu

fu! s:clean_up(old_ic, mode, ...) abort "{{{2
    " Restore options and do some special handling for Operator-pending mode.
    " The optional argument is the tail of the matching group.

    if a:0
        let [ startline, startcol, pat ] = a:000
        let [ cur_line, cur_col ] = getpos('.')[1:2]
    endif

    exe 'set '.(a:old_ic ?  'ic' : 'noic')

    " Open folds, if appropriate.
    if a:mode != 'o'
        if &foldopen =~ 'percent'
            norm! zv
        endif

    " In Operator-pending mode, we want to include the whole match
    " For example, if we're on the keyword `function` in a Vim file,
    " the function has a `return` statement, and we hit `y%`, we should yank
    " the whole keyword `return`, not just its first character `r`.
    "
    " This is only a problem if we end up moving in the forward direction.
    elseif a:0 && ( startline < cur_line
    \||             startline == cur_line && startcol < cur_col)

            let line = getline('.')

            " Check whether the match is a single character.
            " If not, move to the end of the match.
            "
            "                                  ┌ mid.'\|'.fin
            "                                  │ Ex:
            "                                  │     'elseif\|endif'
            "                                ┌─┤
            let regex   = s:wholematch(line, pat, cur_col-1)
            let end_col = matchend(line, regex)

            if end_col > cur_col
                call cursor(0, end_col)
            endif
    endif
endfu

fu! s:count(string, pattern, ...) abort "{{{2
    " Count the number of disjoint copies of pattern in string.
    " If the pattern is a literal string and contains no '0' or '1' characters
    " then s:count(string, pattern, '0', '1') should be faster than
    " s:count(string, pattern).

    let pat = escape(a:pattern, '\')
    if a:0 > 1
        let foo = substitute(a:string, '[^'.a:pattern.']', 'a:1', 'g')
        "                    ┌─ FIXME: in the original plugin, the author
        "                    │  wrote `a:string`; doesn't make sense, does it?
        let foo = substitute(foo, pat, a:2, 'g')
        let foo = substitute(foo, '[^'.a:2.']', '', 'g')
        return strlen(foo)
    endif

    let result = 0
    let foo    = a:string
    let index  = matchend(foo, pat)

    while index != -1
        let result = result + 1
        let foo    = strpart(foo, index)
        let index  = matchend(foo, pat)
    endwhile
    return result
endfu

fu! s:get_info() abort "{{{2
    " In s:clean_up(), we may need to check whether the cursor moved forward.
    return [ &l:ic, line('.'), col('.') ]
endfu

fu! s:insert_refs(groupBR, prefix, group, suffix, line) abort "{{{2
    " Example (simplified HTML patterns).
    "
    " If:
    "       a:groupBR   = '<\(\k\+\)>:</\1>'
    "       a:prefix    = '^.\{3}\('
    "       a:group     = '<\(\k\+\)>:</\(\k\+\)>'
    "       a:suffix    = '\).\{2}$'
    "       a:line      =  "123<tag>12" or "123</tag>12"
    "
    " … then extract "tag" from a:line and return "<tag>:</tag>".

    if a:line !~ a:prefix
              \. substitute(a:group, s:even_backslash.'\zs:', '\\|', 'g')
              \. a:suffix
        return a:group
    endif

    let i      = matchend(a:groupBR, s:even_backslash.':')
    let head   = strpart(a:groupBR, 0, i-1)
    let tailBR = strpart(a:groupBR, i)
    let word   = s:choose(a:group, a:line, ':', '', a:prefix, a:suffix, a:groupBR)
    let i      = matchend(word, s:even_backslash.':')
    let wordBR = strpart(word, i)
    let word   = strpart(word, 0, i-1)

    " Now, a:line =~ a:prefix . word . a:suffix
    if wordBR != head
        let table = s:resolve(head, wordBR, 'table')
    else
        let table = ''
        let d     = 0
        while d < 10
            if tailBR =~ s:even_backslash.'\\'.d
                let table = table.d
            else
                let table = table.'-'
            endif
            let d += 1
        endwhile
    endif
    let d = 9
    while d
        if table[d] != '-'
            let backref = substitute(a:line, a:prefix.word.a:suffix, '\'.table[d], '')
            " Are there any other characters that should be escaped?
            let backref = escape(backref, '*,:')
            exe s:ref(head, d, 'start', 'len')
            let head    = strpart(head, 0, start) . backref . strpart(head, start+len)
            let tailBR  = substitute(tailBR, s:even_backslash.'\zs\\'.d, escape(backref, '\\&'), 'g')
        endif
        let d -= 1
    endwhile
    return head.':'.tailBR
endfu

fu! matchit#next_word(fwd, mode) abort "{{{2
    let [ old_ic, startline, startcol ] = s:get_info()

    " Use default behavior if called with a count.
    " Ex:    42%  →  move cursor onto the line at 42% of the file
    if v:count
        exe 'norm! '.v:count.'%'
        call s:clean_up(old_ic, a:mode)
        return
    endif

    call s:set_ic()
    call s:set_pat()

    " Second step:  set the following variables:
    "
    "   ┌─────────┬───────────────────────────────────────────┐
    "   │ line    │ line on which the cursor started          │
    "   ├─────────┼───────────────────────────────────────────┤
    "   │ cur_col │ number of characters before match         │
    "   ├─────────┼───────────────────────────────────────────┤
    "   │ prefix  │ regex for start of line to start of match │
    "   ├─────────┼───────────────────────────────────────────┤
    "   │ suffix  │ regex for end of match to end of line     │
    "   └─────────┴───────────────────────────────────────────┘

    " Require match to end on or after the cursor and prefer it to
    " start on or before the cursor.
    let line = getline(startline)

    " Find the match that ends on or after the cursor and set cur_col.
    let regex   = s:wholematch(line, s:all_words, startcol-1)
    let cur_col = match(line, regex)
    " If there is no match, give up.
    if cur_col == -1
        call s:clean_up(old_ic, a:mode)
        return
    endif
    let end_col = matchend(line, regex)
    let suf     = strlen(line) - end_col
    let prefix  = cur_col
               \?     '^.*\%'.(cur_col + 1).'c\%('
               \:     '^\%('
    let suffix  = suf
               \?     '\)\%'.(end_col + 1).'c.*$'
               \:     '\)$'

    " Third step:  Find the group and single word that match, and the original
    " (backref) versions of these.  Then, resolve the backrefs.
    " Set the following local variable:
    "
    "         group = colon-separated list of patterns, one of which matches
    "               = start:middle:end or start:end
    "
    " Now, set group and groupBR to the matching group: 'if:endif' or
    " 'while:endwhile' or whatever.  A bit of a kluge:  s:choose() returns
    " group . "," . groupBR, and we pick it apart.

    let group   = s:choose(s:pat, line, ',', ':', prefix, suffix, s:pat_unresolved)
    let i       = matchend(group, s:even_backslash.',')
    let groupBR = strpart(group, i)
    let group   = strpart(group, 0, i-1)

    " Now, line =~ prefix . substitute(group,':','\|','g') . suffix
    if s:has_BR " Do the hard part:  resolve those backrefs!
        let group = s:insert_refs(groupBR, prefix, group, suffix, line)
    endif

    " Fourth step:  Set the arguments for searchpair().
    let i      = matchend(group, s:even_backslash.':')
    let j      = matchend(group, '.*'.s:even_backslash.':')
    let start  = strpart(group, 0, i-1)
    let middle = substitute(strpart(group, i,j-i-1), s:even_backslash.'\zs:', '\\|', 'g')
    let end    = strpart(group, j)

    "Un-escape the remaining , and : characters.
    let start  = substitute(start,  s:even_backslash.'\zs\\\(:\|,\)', '\1', 'g')
    let middle = substitute(middle, s:even_backslash.'\zs\\\(:\|,\)', '\1', 'g')
    let end    = substitute(end,    s:even_backslash.'\zs\\\(:\|,\)', '\1', 'g')

    " searchpair() requires that these patterns avoid \(\) groups.
    let start  = substitute(start,  s:even_backslash.'\zs\\(', '\\%(', 'g')
    let middle = substitute(middle, s:even_backslash.'\zs\\(', '\\%(', 'g')
    let end    = substitute(end,    s:even_backslash.'\zs\\(', '\\%(', 'g')

    if   a:fwd && line =~ prefix.end.suffix
    \|| !a:fwd && line =~ prefix.start.suffix
        let middle = ''
    endif

    let flags =  a:fwd && line =~ prefix.end.suffix
            \|| !a:fwd && line !~ prefix.start.suffix
            \?       'bW'
            \:       'W'

    let skip = get(b:, 'match_skip', 's:comment\|string')
    let skip = s:parse_skip(skip)

    " Fifth step:  actually start moving the cursor and call searchpair().
    " Later, :exe restore_cursor to get to the original screen.
    let view = winsaveview()
    call cursor(0, cur_col + 1)
    if skip =~ 'synID' && !exists('g:syntax_on')
        let skip = '0'
    else
        exe 'if '.skip."| let skip = '0' | endif"
    endif
    let sp_return = searchpair(start, middle, end, flags, skip)
    let final_position = 'call cursor('.line('.').','.col('.').')'
    " Restore cursor position and original screen.
    call winrestview(view)
    norm! m'
    if sp_return > 0
        exe final_position
    endif
    call s:clean_up(old_ic, a:mode, startline, startcol, middle.'\|'.end)
endfu

fu! matchit#next_unmatched(fwd, mode) abort "{{{2
    " Jump to the nearest unmatched:
    "
    "         • (
    "         • if
    "         • <tag>
    "
    "         • )
    "         • endif
    "         • </tag>

    if get(b:, 'match_words', '') == ''
        return
    endif
    let level = v:count1

    let [ old_ic, startline, startcol ] = s:get_info()
    call s:set_ic()
    call s:set_pat()

    " Second step:  figure out the patterns for searchpair()
    " and save the screen, cursor position, and 'ignorecase'.

    " '\<function\>:\<return\>:\<endfunction\>,\<if\>:\<elseif\>:\<endif\>'

    " '\<function\>\),\(\<if\>:\<elseif\>:\<endif\>'
    let start = substitute(s:pat,
              \            s:even_backslash.'\zs:.\{-}'.s:even_backslash.',',
              \            '\\),\\(',
              \            'g')

    " '\(\<function\>\),\(\<if\>\)'
    let start = '\('.substitute(start, s:even_backslash.'\zs:.*$', '\\)', '')



    " let middle = substitute(s:pat,
    "            \            s:even_backslash.'\zs,.\{-}'.s:even_backslash.':',
    "            \            '',
    "            \            'g')

    " let middle = substitute(middle,
    "            \            s:even_backslash.'\zs:.\{-}'.s:even_backslash.',',
    "            \            '\\),\\(',
    "            \            'g')

    " let middle = substitute(middle, '^.\{-}'.s:even_backslash.':', '\\(', '').'\)'



    " s:pat
    " '\<function\>:\<return\>:\<endfunction\>,\<if\>:\<elseif\>:\<endif\>'

    " '\<function\>:\<return\>:\<endfunction\>\),\(\<elseif\>:\<endif\>'
    let end = substitute(s:pat,
            \            s:even_backslash.'\zs,.\{-}'.s:even_backslash.':',
            \            '\\),\\(',
            \            'g')

    " '\(\<return\>:\<endfunction\>\),\(\<elseif\>:\<endif\>\)'
    let end = substitute(end, '^.\{-}'.s:even_backslash.':', '\\(', '').'\)'

    let skip = get(b:, 'match_skip', 's:comment\|string')

    let skip = s:parse_skip(skip)
    let view = winsaveview()

    " Third step: call searchpair().
    " Replace '\('--but not '\\('--with '\%(' and ',' with '\|'.
    let start = substitute(start, '\(\\\@<!\(\\\\\)*\)\@<=\\(', '\\%(', 'g')
    let start = substitute(start, ',', '\\|', 'g')

    let end = substitute(end, '\(\\\@<!\(\\\\\)*\)\@<=\\(', '\\%(', 'g')
    let end = substitute(end, '[:,]', '\\|', 'g')

    if skip =~ 'synID' && !exists('g:syntax_on')
        let skip = '0'
    else
        try
            exe 'if '.skip."| let skip = '0' | endif"
        catch /\v^Vim%(\(\a+\))?:E363/
            " We won't find anything, so skip searching, should keep Vim responsive.
            return
        endtry
    endif

    mark '
    while level
        " start
        " '\%(foo\)\|\%(bar\)\|\%((\)\|\%({\)\|\%(\[\)\|\%(\/\*\)\|\%(#\s*if\%(def\)\?\)'

        " end
        " '\%(endfoo\)\|\%(endbar\)\|\%()\)\|\%(}\)\|\%(\]\)\|\%(\*\/\)\|\%(#\s*else\>\|#\s*elif\>\|#\s*endif\>\)'

        " fwd
        " 1

        " skip
        " '0'

        " let g:start = '\%(foo\)\|\%(bar\)\|\%((\)\|\%({\)\|\%(\[\)\|\%(\/\*\)\|\%(#\s*if\%(def\)\?\)'
        " let g:end = '\%(endfoo\)\|\%(endbar\)\|\%()\)\|\%(}\)\|\%(\]\)\|\%(\*\/\)\|\%(#\s*else\>\|#\s*elif\>\|#\s*endif\>\)'
        " let g:fwd = 1
        " let g:skip = '0'

        if searchpair(start, '', end, (a:fwd ? 'W' : 'bW'), skip) < 1
            call s:clean_up(old_ic, a:mode)
            return
        endif
        let level -= 1
    endwhile
    call s:clean_up(old_ic, a:mode)
endfu

fu! s:parse_skip(str) abort "{{{2
    " Search backwards for "if" or "while" or "<tag>" or ...
    " and return "endif" or "endwhile" or "</tag>" or ... .
    " For now, this uses b:match_words and the same script variables
    " as `wrapper()`.  Later, it may get its own patterns,
    " either from a buffer variable or passed as arguments.

    " Parse special strings as typical skip arguments for searchpair():
    "
    "   s:foo becomes (current syntax item) =~ foo
    "   S:foo becomes (current syntax item) !~ foo
    "   r:foo becomes (text before cursor) =~ foo
    "   R:foo becomes (text before cursor) !~ foo

    let skip = a:str
    if skip[1] == ':'
        let skip = (skip[0] == 's'
                \?     "synIDattr(synID(line('.'),col('.'),1),'name') =~? "
                \: skip[0] == 'S'
                \?     "synIDattr(synID(line('.'),col('.'),1),'name') !~? "
                \: skip[0] == 'r'
                \?     "strpart(getline('.'),0,col('.'))=~"
                \: skip[0] == 'R'
                \?     "strpart(getline('.'),0,col('.'))!~"
                \: skip)
                \.string(strpart(skip,2))
    endif
    return skip
endfu

fu! s:parse_words(groups) abort "{{{2

    " Input: a comma-separated list of groups with backrefs, such as:
    "
    "       '\(foo\):end\1,\(bar\):end\1'
    "
    " Output: same thing but with backrefs replaced:
    "
    "       '\(foo\):end\(foo\),\(bar\):end\(bar\)'


    " , and : are special characters.
    " A sequence of them doesn't have much sense, and thus should be reduced.
    " There can be 2 kinds of sequences:
    "
    "       • only colons                          →   should be reduced to a single colon
    "       • commas mixed with possible colons    →   should be reduced to a single comma
    "
    " Why only these 2 kinds? What about “only commas“?
    " Already covered by “commas mixed with possible colons“. Pay attention to “possible“.

    let groups = substitute(a:groups, s:even_backslash.'\zs:\{2,}', ':', 'g')

    "                                                                  ┌ a sequence of colons and commas
    "                                                                  │ containing at least one comma
    "                                                        ┌─────────┤
    let groups = substitute(groups.',', s:even_backslash.'\zs[:,]*,[:,]*', ',', 'g')
    "                                                                         │
    "                                          replace it with a single comma ┘

    " We'll return this variable at the end of the function.
    " It should store the groups with all backrefs resolved.
    let parsed = ''
    " The rest of the code is an imbrication of while loops.
    " The purpose of the:
    "
    "         • outer loop is to process groups
    "         • inner loop is to process words in a group

    "                     ┌ go on until there's only colons and/or commas in `groups`
    "     ┌───────────────┤
    while groups =~ '[^:,]'
        " What's `i`(-1) and `j`(-1)? {{{
        "
        " Weights of the substrings from the beginning of `groups` up to a colon / comma.
        " When the `-1` offset is added, the colon / comma is excluded from the substring.
        "
        "                      i-1   j-1
        "                      v     v
        "     groups = '\(foo\):end\1,\(bar\):end\1'
        "                       ^     ^
        "                       i     j
"}}}
        " Why `matchend()`?{{{
        "
        " `match(str, pat)`  / `matchend(str,  pat)` returns  the weight  of the
        " substring up to the the 1st match of `pat`, EXCLUDING / INCLUDING it.
        "
        " We use `matchend()` instead of `match()` because of `s:even_backslash`.
        " We could also probably use `match()` if we suffixed `:` with `\zs`:
        "
        "         let i = match(groups, s:even_backslash.':\zs')
"}}}
        let i = matchend(groups, s:even_backslash.':')
        let j = matchend(groups, s:even_backslash.',')

        " ┌────────┬───────────────┐
        " │ head   │ \(foo\)       │
        " ├────────┼───────────────┤
        " │ tail   │ end\1:        │
        " ├────────┼───────────────┤
        " │ groups │ \(bar\):end\1 │
        " └────────┴───────────────┘
        "
        " head + tail = 1st group
        " groups      = subsequent groups

        "                               ┌ this is NOT a byte index,
        "                               │ this is the weight of the desired substring;
        "                             ┌─┤ the `-1` offset excludes the colon
        let head = strpart(groups, 0, i-1)
        let tail = strpart(groups, i, j-1-i).':'
        "                                     │
        "                                     └─ useful to make sure the next `while` loop
        "                                        parses `tail` fully

        " This assignment removes the first group from `groups`.
        " So, after each iteration of the main `while` loop, `groups` gets smaller.
        " The loop will stop when the only remaining characters are colons / commas.
        let groups = strpart(groups, j)

        " update `parsed` and `i` before entering the inner loop
        let parsed .= head
        let i       = matchend(tail, s:even_backslash.':')
        " go on until `tail` has been completely parsed
        " that is: there's no colon left
        while i != -1
            " In 'if:else:endif' :
            "
            "         • head = 'if'       assigned in                      the outer loop
            "         • word = 'else'     assigned in the 1st iteration of the inner loop
            "         • word = 'endif'    assigned in the 2nd iteration of the inner loop

            " next word in the group
            " (group currently processed in the main while loop)
            let word = strpart(tail, 0, i-1)

            " all the remaining words of this group
            let tail = strpart(tail, i)

            " resolve the possible backrefs in the word
            let parsed .= ':'.s:resolve(head, word, 'word')

            " move on to the next word
            let i = matchend(tail, s:even_backslash.':')
        endwhile

        " add a comma before parsing and adding another group
        let parsed .= ','
    endwhile

    " remove the last comma added during the last iteration of the main while loop
    return substitute(parsed, ',$', '', '')
endfu

fu! s:ref(string, d, ...) abort "{{{2
    " No extra arguments: s:ref(string, d) will find the d'th occurrence of '\('
    " and return  it, along  with everything  up to  and including  the matching
    " '\)'.
    "
    " One argument: s:ref(string, d, "start") returns  the index of the start of
    " the d'th '\(' and any other argument returns the length of the group.
    "
    " Two  arguments: s:ref(string,  d, "foo",  "bar")  returns a  string to  be
    " executed, having the effect of:
    "
    "       let foo = s:ref(string, d, "start")
    "       let bar = s:ref(string, d, "len")

    let len = strlen(a:string)
    if a:d == 0
        let start = 0
    else
        let cnt   = a:d
        let match = a:string
        while cnt
            let cnt -= 1
            let index = matchend(match, s:even_backslash.'\\(')
            if index == -1
                return ''
            endif
            let match = strpart(match, index)
        endwhile
        let start = len - strlen(match)
        if a:0 == 1 && a:1 == 'start'
            return start - 2
        endif
        let cnt = 1
        while cnt
            let index = matchend(match, s:even_backslash.'\\(\|\\)') - 1
            if index == -2
                return ''
            endif
            " Increment if an open, decrement if a ')':
            let cnt += match[index] == '(' ? 1 : -1   " ')'
            " let cnt = stridx('0(', match[index]) + cnt
            let match = strpart(match, index+1)
        endwhile
        let start -= 2
        let len -= start + strlen(match)
    endif

    return a:0 == 1
    \?         len
    \:     a:0 == 2
    \?         'let '.a:1.'='.start.'| let '.a:2.'='.len
    \:         strpart(a:string, start, len)
endfu

fu! s:resolve(head, word, output) abort "{{{2

    "                             ┌───────────────── head of group
    "                             │                ┌ a word of the same group
    "                    ┌────────┤    ┌───────────┤
    "         s:resolve('\(a\)\(b\)', '\(c\)\2\1\1\2')
    "
    " … should return table.word, where:
    "
    "         word  = '\(c\)\(b\)\(a\)\3\2'
    "         table = '-32-------'
    "
    " That  is, the  first '\1'  in  word is  replaced by  '\(a\)' from  word,
    " table[1]  = 3,  and this  indicates that  all other  instances of  '\1' in
    " word are to be replaced by '\3'.
    "
    " The hard part is dealing with nesting …
    "
    " Note that ":" is an illegal character  for head and word, unless it is
    " preceded by "\".

    let word  = a:word
    let i     = matchend(word, s:even_backslash.'\\\d') - 1
    let table = '----------'

    " As long as there are back references to be replaced.
    while i != -2
        let d = word[i]
        let backref = s:ref(a:head, d)

        " The idea is to replace `\d` with backref.
        "
        " Before we do this, replace any \(\) groups in backref with :1, :2, …
        " if they  correspond to the  first, second, … group  already inserted
        " into backref. Later, replace :1 with \1 and so on.
        "
        " The group number `w+b` within  backref corresponds to the group number
        " `s` within a:head.

        " ┌───┬───────────────────────────────────────────────┐
        " │ w │ number of '\(' in word before the current one │
        " ├───┼───────────────────────────────────────────────┤
        " │ b │ number of the current '\(' in backref         │
        " ├───┼───────────────────────────────────────────────┤
        " │ s │ number of the current '\(' in a:head          │
        " └───┴───────────────────────────────────────────────┘

        let w = s:count(substitute(strpart(word, 0, i-1), '\\\\', '', 'g'), '\(', '1')
        let b = 1
        let s = d

        while b <= s:count(substitute(backref, '\\\\', '', 'g'), '\(', '1')
        \&&   s < 10
            if table[s] == '-'
                if w + b < 10
                    " let table[s] = w + b
                    let table = strpart(table, 0, s).(w+b).strpart(table, s+1)
                endif
                let b += 1
                let s += 1
            else
                exe s:ref(backref, b, 'start', 'len')
                let ref = strpart(backref, start, len)
                let backref = strpart(backref, 0, start).':'.table[s]
                           \. strpart(backref, start+len)
                let s += s:count(substitute(ref, '\\\\', '', 'g'), '\(', '1')
            endif
        endwhile
        let word = strpart(word, 0, i-1).backref.strpart(word, i+1)
        let i    = matchend(word, s:even_backslash.'\\\d') - 1
    endwhile

    let word = substitute(word, s:even_backslash.'\zs:', '\\', 'g')

    return a:output == 'table'
    \?         table
    \:     a:output == 'word'
    \?         word
    \:     table.word
endfu

fu! s:set_ic() abort "{{{2
    " If we've set up `b:match_ignorecase` differently than `&ignorecase`,
    " then save the latter, before resetting it according to `b:match_ignorecase`.
    if exists('b:match_ignorecase') && b:match_ignorecase != &ic
        let &ic = b:match_ignorecase
    endif
endfu

fu! s:set_pat() abort "{{{2

    " if not already done, set the following script variables
    "
    "         ┌──────────────┬─────────────────────────────────────┐
    "         │ s:has_BR     │ flag for whether there are backrefs │
    "         │              │ in b:match_words                    │
    "         ├──────────────┼─────────────────────────────────────┤
    "         │ s:pat        │ parsed version of b:match_words     │
    "         │              │                 + def_words         │
    "         ├──────────────┼─────────────────────────────────────┤
    "         │ s:all_words  │ conversion of `s:pat` into a regex  │
    "         └──────────────┴─────────────────────────────────────┘

    let match_words = get(b:, 'match_words', '')

    if match_words != s:last_words || &l:mps != s:last_mps

        "             ┌─ default pairs stored in 'mps'
        "             │
        "             │         ┌─ C-style comment (to mimic `%`)
        "             │         │                                                  ┌ C preprocessor conditionals
        "             │         │                                                  │ (to mimic `%`)
        " ┌───────────┤ ┌───────┤ ┌────────────────────────────────────────────────┤
        " (:),{:},\[:\],\/\*:\*\/,#\s*if\%(def\)\?:#\s*else\>:#\s*elif\>:#\s*endif\>

        let def_words = escape(&l:mps, '[$^.*~\/?]')
                     \. (!empty(&l:mps) ? ',' : '')
                     \. '\/\*:\*\/,#\s*if\%(def\)\?:#\s*else\>:#\s*elif\>:#\s*endif\>'

        " We store the last used value of `b:match_words` and `&l:mps` in
        " script-local variables. Update them.
        let s:last_mps   = &l:mps
        " append `def_words` before saving in `s:last_words`
        let match_words  = match_words.(!empty(match_words) ? ',' : '').def_words
        let s:last_words = match_words

        " There's a backref in `match_words` IFF we can find an odd number of{{{
        " backslashes in front of a digit.
        " Watch:
        "         3  →          3    (NOT a backref)
        "       \\3  →      \ + 3    (")
        "     \\\\3  →  \ + \ + 3    (")
        "     …
        "
        " Explanation:
        " For a backref to appear, there needs to be a backslash in front of a digit.
        " But every pair of backslashes cancels out, because they match a real backslash.
        " So, inside `match_words`, there needs to be an ODD number of backslashes
        " in front of a digit.
        "
        " To check this pattern is present inside `match_words`, we need a regex.
        " An EVEN number of backslashes can be expressed with `s:even_backslash`.
        " So, an ODD number of backslashes can be expressed with:
        "
        "         s:even_backslash.'\\'
        "
        " And to check whether `match_words` contains a backref, we need this regex:
        "
        "         s:even_backslash.'\\\d'
"}}}
        if match_words =~ s:even_backslash.'\\\d'
            let s:has_BR = 1
            let s:pat    = s:parse_words(match_words)
        else
            let s:has_BR = 0
            let s:pat    = match_words
        endif
        let s:all_words = '\%('.substitute(s:pat, s:even_backslash.'\zs[,:]\+', '\\|', 'g').'\)'

        " Reconstruct the pattern with unresolved backrefs.
        let s:pat_unresolved = substitute(match_words.',',
                             \ s:even_backslash.'\zs[,:]*,[,:]*', ',', 'g')
        let s:pat_unresolved = substitute(s:pat_unresolved, s:even_backslash.'\zs:\{2,}', ':', 'g')
    endif
endfu

fu! s:wholematch(line, pat, start) abort "{{{2

    " NOTE:
    " I think that the purpose of this function is to tweak a regex so that
    " its first match is near the current cursor position.

    " TODO: What should I do if a:start is out of range?
    "
    " Return a regex that matches all of a:line, such that
    " matchstr(a:line, regex) represents the match for a:pat that starts
    " as close to a:start as possible, before being preferred to after, and
    " ends after a:start .

    " Usage:
    "         let pat    = s:wholematch(getline('.'), 'foo\|bar', col('.')-1)
    "         let i      = match(getline('.'), pat)
    "         let j      = matchend(getline('.'), pat)
    "         let match  = matchstr(getline('.'), pat)

    let len = strlen(a:line)

    let prefix = a:start
              \?     '\%<'.(a:start + 2).'c\zs'
              \:     '^'

    let group = '\%('.a:pat.'\)'

    let suffix = a:start+1 < len
              \?     '\ze\%>'.(a:start+1).'c'
              \:     '$'

    if a:line !~ prefix.group.suffix
        let prefix = ''
    endif
    return prefix.group.suffix
endfu

" Variables {{{1

let s:last_mps       = ''
let s:last_words     = ':'
let s:pat_unresolved = ''

" An even number of consecutive backslashes.
"
" How is this variable useful?{{{
" By design, the  plugin treats a colon and a  comma, inside `b:match_words`, as
" special characters.  They are delimiters between 2 consecutive words or groups
" of words:
"
"         :    →    delimiter between 2 words
"         ,    →    delimiter between 2 groups
"
" But how to include a literal colon or comma inside a word?
" By design, the plugin should consider a backslashed colon or comma as literal.
"
"         \:    →    literal colon
"         \,    →    literal comma
"
" But what about a colon or comma preceded by literal backslashes?
"
"         \\\:     →    literal backslash         +  literal colon
"         \\\,     →    literal backslash         +  literal comma
"         \\\\:    →    literal double backslash  +  delimiter between 2 words
"         \\\\,    →    literal double backslash  +  delimiter between 2 groups
"
" When a  colon/comma is  preceded by an  even number of  backslashes, it  has a
" special meaning. Otherwise, it's literal.
" That's  why `s:even_backslash`  is useful:  to make  the difference  between a
" literal and special colon/comma.
"}}}
"                            ┌ no slash before an even number of slashes
"                       ┌────┤
let s:even_backslash = '\\\@<!\%(\\\\\)*'
"                                └──┤
"                                   └ 2 slashes
