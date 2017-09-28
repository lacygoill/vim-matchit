if exists('g:autoloaded_matchit')
    finish
endif
let g:autoloaded_matchit = 1

" TODO:
" We should store the tokens in a list not in a string.
" This would allow us to get rid of things like `!empty(…) ? ',' : ''`

" TODO:
" Look at the comments in the original plugin. Search for `Autocomplete()`.
" The author wanted to implement a function which completes a token.
" If it's useful, try to finish what he began. Or re-implement it entirely.

" TODO:
" Add support for multi-line patterns for b:match_words.
" This would require an option:  how many lines to scan (default 1).
" This would be useful for Python, maybe also for *ML.
"
" TODO:  Eliminate the `multi()` function.
" Add yet another argument to `wrapper()` instead.
"
" TODO:  Allow:
"
"         let b:match_words = '\v((foo)(bar)):\3\2:end\1'
"
" TODO:  Make backrefs safer by using '\V' (very no-magic).

" Functions {{{1
fu! s:choose(patterns, string, comma, branch, prefix, suffix, ...) abort "{{{2
    " Assume a:comma = ",".  Then the format for a:patterns and a:1 is
    "   a:patterns = "<pat1>,<pat2>,..."
    "   a:1 = "<alt1>,<alt2>,..."
    " If <patn> is the first pattern that matches a:string then return <patn>
    " if no optional arguments are given; return <patn>,<altn> if a:1 is given.

    let tail = (a:patterns =~ a:comma."$" ? a:patterns : a:patterns . a:comma)
    let i = matchend(tail, s:even_backslash . a:comma)
    if a:0
        let alttail = (a:1 =~ a:comma."$" ? a:1 : a:1 . a:comma)
        let j = matchend(alttail, s:even_backslash . a:comma)
    endif
    let current = strpart(tail, 0, i-1)
    if a:branch == ""
        let currpat = current
    else
        let currpat = substitute(current, s:even_backslash . a:branch, '\\|', 'g')
    endif
    while a:string !~ a:prefix . currpat . a:suffix
        let tail = strpart(tail, i)
        let i = matchend(tail, s:even_backslash . a:comma)
        if i == -1
            return -1
        endif
        let current = strpart(tail, 0, i-1)
        if a:branch == ""
            let currpat = current
        else
            let currpat = substitute(current, s:even_backslash . a:branch, '\\|', 'g')
        endif
        if a:0
            let alttail = strpart(alttail, j)
            let j = matchend(alttail, s:even_backslash . a:comma)
        endif
    endwhile
    if a:0
        let current = current . a:comma . strpart(alttail, 0, j-1)
    endif
    return current
endfu

fu! s:clean_up(old_ic, mode, startline, startcol, ...) abort "{{{2
    " Restore options and do some special handling for Operator-pending mode.
    " The optional argument is the tail of the matching group.

    exe 'set '.(a:old_ic ?  'ic' : 'noic')

    " Open folds, if appropriate.
    if a:mode != 'o'
        if &foldopen =~ 'percent'
            norm! zv
        endif
        " In Operator-pending mode, we want to include the whole match
        " (for example, d%).
        " This is only a problem if we end up moving in the forward direction.

    elseif a:startline < line('.')
    \||    a:startline == line('.') && a:startcol < col('.')
        if a:0
            " Check whether the match is a single character.  If not, move to the
            " end of the match.
            let [ matchline, cur_col ] = getpos('.')[1:2]
            "                                       ┌ mid.'\|'.fin
            "                                       │ Ex:
            "                                       │     'elseif\|endif'
            "                                     ┌─┤
            let regexp  = s:wholematch(matchline, a:1, cur_col-1)
            let end_col = matchend(matchline, regexp)

            "  This is NOT off by one!
            if end_col > cur_col
                call cursor(0, end_col)
            endif
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

fu! s:insert_refs(groupBR, prefix, group, suffix, matchline) abort "{{{2
    " Example (simplified HTML patterns):  if
    "   a:groupBR   = '<\(\k\+\)>:</\1>'
    "   a:prefix    = '^.\{3}\('
    "   a:group     = '<\(\k\+\)>:</\(\k\+\)>'
    "   a:suffix    = '\).\{2}$'
    "   a:matchline =  "123<tag>12" or "123</tag>12"
    " then extract "tag" from a:matchline and return "<tag>:</tag>" .

    if a:matchline !~ a:prefix .
                \ substitute(a:group, s:even_backslash . '\zs:', '\\|', 'g') . a:suffix
        return a:group
    endif
    let i = matchend(a:groupBR, s:even_backslash . ':')
    let ini = strpart(a:groupBR, 0, i-1)
    let tailBR = strpart(a:groupBR, i)
    let word = s:choose(a:group, a:matchline, ":", "", a:prefix, a:suffix,
                \ a:groupBR)
    let i = matchend(word, s:even_backslash . ":")
    let wordBR = strpart(word, i)
    let word = strpart(word, 0, i-1)
    " Now, a:matchline =~ a:prefix . word . a:suffix
    if wordBR != ini
        let table = s:resolve(ini, wordBR, "table")
    else
        let table = ""
        let d = 0
        while d < 10
            if tailBR =~ s:even_backslash . '\\' . d
                let table = table . d
            else
                let table = table . "-"
            endif
            let d = d + 1
        endwhile
    endif
    let d = 9
    while d
        if table[d] != "-"
            let backref = substitute(a:matchline, a:prefix.word.a:suffix,
                        \ '\'.table[d], "")
            " Are there any other characters that should be escaped?
            let backref = escape(backref, '*,:')
            exe s:ref(ini, d, 'start', 'len')
            let ini = strpart(ini, 0, start) . backref . strpart(ini, start+len)
            let tailBR = substitute(tailBR, s:even_backslash . '\zs\\' . d,
                        \ escape(backref, '\\&'), 'g')
        endif
        let d = d-1
    endwhile
    return ini.':'.tailBR
endfu

fu! matchit#multi(fwd, mode) abort "{{{2
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

    let old_ic = s:save_and_set_ic()
    let [ startline, startcol ] = getpos('.')[1:2]

    call s:set_some_var()

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
        if searchpair(start, '', end, (a:fwd ? 'W' : 'bW'), skip) < 1
            call s:clean_up(old_ic, a:mode, startline, startcol)
            return
        endif
        let level -= 1
    endwhile
    call s:clean_up(old_ic, a:mode, startline, startcol)
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
    " Input a comma-separated list of groups with backrefs, such as
    "   a:groups = '\(foo\):end\1,\(bar\):end\1'
    " and return a comma-separated list of groups with backrefs replaced:
    "   return '\(foo\):end\(foo\),\(bar\):end\(bar\)'

    let groups = substitute(a:groups.',', s:even_backslash.'\zs[,:]*,[,:]*', ',', 'g')
    let groups = substitute(groups, s:even_backslash.'\zs:\{2,}', ':', 'g')
    let parsed = ''

    while groups =~ '[^,:]'
        let i = matchend(groups, s:even_backslash.':')
        let j = matchend(groups, s:even_backslash.',')
        let ini = strpart(groups, 0, i-1)
        let tail = strpart(groups, i, j-i-1).':'
        let groups = strpart(groups, j)
        let parsed = parsed.ini
        let i = matchend(tail, s:even_backslash.':')
        while i != -1
            " In 'if:else:endif', ini='if' and word='else' and then word='endif'.
            let word = strpart(tail, 0, i-1)
            let tail = strpart(tail, i)
            let i = matchend(tail, s:even_backslash.':')
            let parsed = parsed.':'.s:resolve(ini, word, 'word')
        endwhile " Now, tail has been used up.
        let parsed = parsed . ","
    endwhile " groups =~ '[^,:]'

    let parsed = substitute(parsed, ',$', '', '')
    return parsed
endfu

fu! s:ref(string, d, ...) abort "{{{2
    " No extra arguments:  s:ref(string, d) will
    " find the d'th occurrence of '\(' and return it, along with everything up
    " to and including the matching '\)'.
    " One argument:  s:ref(string, d, "start") returns the index of the start
    " of the d'th '\(' and any other argument returns the length of the group.
    " Two arguments:  s:ref(string, d, "foo", "bar") returns a string to be
    " executed, having the effect of
    "   :let foo = s:ref(string, d, "start")
    "   :let bar = s:ref(string, d, "len")

    let len = strlen(a:string)
    if a:d == 0
        let start = 0
    else
        let cnt = a:d
        let match = a:string
        while cnt
            let cnt = cnt - 1
            let index = matchend(match, s:even_backslash . '\\(')
            if index == -1
                return ""
            endif
            let match = strpart(match, index)
        endwhile
        let start = len - strlen(match)
        if a:0 == 1 && a:1 == "start"
            return start - 2
        endif
        let cnt = 1
        while cnt
            let index = matchend(match, s:even_backslash . '\\(\|\\)') - 1
            if index == -2
                return ""
            endif
            " Increment if an open, decrement if a ')':
            let cnt = cnt + (match[index]=="(" ? 1 : -1)  " ')'
            " let cnt = stridx('0(', match[index]) + cnt
            let match = strpart(match, index+1)
        endwhile
        let start = start - 2
        let len = len - start - strlen(match)
    endif
    if a:0 == 1
        return len
    elseif a:0 == 2
        return "let " . a:1 . "=" . start . "| let " . a:2 . "=" . len
    else
        return strpart(a:string, start, len)
    endif
endfu

fu! s:resolve(source, target, output) abort "{{{2
    " s:resolve('\(a\)\(b\)', '\(c\)\2\1\1\2') should return table.word, where
    " word = '\(c\)\(b\)\(a\)\3\2' and table = '-32-------'.  That is, the first
    " '\1' in target is replaced by '\(a\)' in word, table[1] = 3, and this
    " indicates that all other instances of '\1' in target are to be replaced
    " by '\3'.  The hard part is dealing with nesting...
    " Note that ":" is an illegal character for source and target,
    " unless it is preceded by "\".

    let word  = a:target
    let i     = matchend(word, s:even_backslash . '\\\d') - 1
    let table = "----------"

    while i != -2 " There are back references to be replaced.
        let d = word[i]
        let backref = s:ref(a:source, d)
        " The idea is to replace '\d' with backref.  Before we do this,
        " replace any \(\) groups in backref with :1, :2, ... if they
        " correspond to the first, second, ... group already inserted
        " into backref.  Later, replace :1 with \1 and so on.  The group
        " number w+b within backref corresponds to the group number
        " s within a:source.
        " w = number of '\(' in word before the current one
        let w = s:count(
                    \ substitute(strpart(word, 0, i-1), '\\\\', '', 'g'), '\(', '1')
        let b = 1 " number of the current '\(' in backref
        let s = d " number of the current '\(' in a:source
        while b <= s:count(substitute(backref, '\\\\', '', 'g'), '\(', '1')
                    \ && s < 10
            if table[s] == '-'
                if w + b < 10
                    " let table[s] = w + b
                    let table = strpart(table, 0, s).(w+b).strpart(table, s+1)
                endif
                let b += b
                let s += s
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
    if a:output == 'table'
        return table
    elseif a:output == 'word'
        return word
    else
        return table.word
    endif
endfu

fu! s:save_and_set_ic() abort "{{{2
    let old_ic = &l:ic

    " If we've set up `b:match_ignorecase` differently than `&ignorecase`,
    " then save the latter, before resetting it according to `b:match_ignorecase`.
    if exists('b:match_ignorecase') && b:match_ignorecase != &ic
        let &ic = b:match_ignorecase
    endif

    return old_ic
endfu

fu! s:set_some_var() abort "{{{2

    " if not already done, set the following script variables
    "
    "         ┌──────────┬──────────────────────────────────────────────┐
    "         │ s:has_BR │ flag for whether there are backrefs          │
    "         ├──────────┼──────────────────────────────────────────────┤
    "         │ s:pat    │ parsed version of b:match_words              │
    "         ├──────────┼──────────────────────────────────────────────┤
    "         │ s:all    │ regexp based on s:pat and the default groups │
    "         └──────────┴──────────────────────────────────────────────┘

    let match_words = get(b:, 'match_words', '')

    "             ┌─ default pairs stored in 'mps'
    "             │
    "             │         ┌─ C-style comment (to mimic `%`)
    "             │         │                                                  ┌ C preprocessor conditionals
    "             │         │                                                  │ (to mimic `%`)
    " ┌───────────┤ ┌───────┤ ┌────────────────────────────────────────────────┤
    " (:),{:},\[:\],\/\*:\*\/,#\s*if\%(def\)\?:#\s*else\>:#\s*elif\>:#\s*endif\>

    let def_words = escape(&l:mps, '[^$.*~\/?]').(!empty(&l:mps) ? ',' : '')
                 \. '\/\*:\*\/'
                 \. ',#\s*if\%(def\)\?:#\s*else\>:#\s*elif\>:#\s*endif\>'

    if match_words != s:last_words || &l:mps != s:last_mps
        " quote the special chars in 'matchpairs'
        " replace [,:] with \|
        " append the pairs: /*, */    #if:#ifdef,#else:#elif,#endif
        let def_words = escape(&l:mps, '[^$.*~\/?]')
                     \. (!empty(&l:mps) ? ',' : '')
                     \. '\/\*:\*\/,#\s*if\%(def\)\=:#\s*else\>:#\s*elif\>:#\s*endif\>'

        " We store the last used value of `b:match_words` and `&l:mps` in
        " script-local variables. Update them.
        let s:last_mps   = &l:mps
        " append `def_words` before saving in `s:last_words`
        let match_words  = match_words.(!empty(match_words) ? ',' : '').def_words
        let s:last_words = match_words

        if match_words =~ s:even_backslash.'\\\d'
            let s:has_BR = 1
            let s:pat    = s:parse_words(match_words)
        else
            let s:has_BR = 0
            let s:pat    = match_words
        endif
        let s:all = '\%('.substitute(s:pat, s:even_backslash.'\zs[,:]\+', '\\|', 'g').'\)'

        " FIXME:
        " The next lines were not present in `s:MultiMatch()`.
        " Now that we have extracted some code inside `s:set_some_var()`,
        " and call the latter in `MultiMatch()`, does it cause an issue
        " if we reset `s:patBR`?

        " Reconstruct the version with unresolved backrefs.
        let s:patBR = substitute(match_words.',',
                    \ s:even_backslash.'\zs[,:]*,[,:]*', ',', 'g')
        let s:patBR = substitute(s:patBR, s:even_backslash.'\zs:\{2,}', ':', 'g')
    endif
endfu

fu! s:wholematch(line, pat, start) abort "{{{2

    " NOTE:
    " I think that the purpose of this function is to tweak a regex so that
    " its first match is near the current cursor position.

    " TODO: What should I do if a:start is out of range?
    "
    " Return a regexp that matches all of a:line, such that
    " matchstr(a:line, regexp) represents the match for a:pat that starts
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

fu! matchit#wrapper(fwd, mode) abort range "{{{2
    let old_ic = s:save_and_set_ic()

    " In s:clean_up(), we may need to check whether the cursor moved forward.
    let [ startline, startcol ] = getpos('.')[1:2]

    " Use default behavior if called with a count.
    if v:count
        exe 'norm! '.v:count.'%'
        call s:clean_up(old_ic, a:mode, startline, startcol)
        return
    endif

    call s:set_some_var()

    " Second step:  set the following variables:
    "
    "     • matchline = line on which the cursor started
    "     • cur_col   = number of characters before match
    "     • prefix    = regexp for start of line to start of match
    "     • suffix    = regexp for end of match to end of line

    " Require match to end on or after the cursor and prefer it to
    " start on or before the cursor.
    let matchline = getline(startline)

    " Find the match that ends on or after the cursor and set cur_col.
    let regexp = s:wholematch(matchline, s:all, startcol-1)
    let cur_col = match(matchline, regexp)
    " If there is no match, give up.
    if cur_col == -1
        call s:clean_up(old_ic, a:mode, startline, startcol)
        return
    endif
    let end_col = matchend(matchline, regexp)
    let suf     = strlen(matchline) - end_col
    let prefix  = (cur_col ? '^.*\%'.(cur_col + 1).'c\%(' : '^\%(')
    let suffix  = (suf ? '\)\%'.(end_col + 1).'c.*$' : '\)$')

    " Third step:  Find the group and single word that match, and the original
    " (backref) versions of these.  Then, resolve the backrefs.
    " Set the following local variable:
    "
    "         group = colon-separated list of patterns, one of which matches
    "               = ini:mid:fin or ini:fin
    "
    " Now, set group and groupBR to the matching group: 'if:endif' or
    " 'while:endwhile' or whatever.  A bit of a kluge:  s:choose() returns
    " group . "," . groupBR, and we pick it apart.

    let group   = s:choose(s:pat, matchline, ',', ':', prefix, suffix, s:patBR)
    let i       = matchend(group, s:even_backslash.',')
    let groupBR = strpart(group, i)
    let group   = strpart(group, 0, i-1)

    " Now, matchline =~ prefix . substitute(group,':','\|','g') . suffix
    if s:has_BR " Do the hard part:  resolve those backrefs!
        let group = s:insert_refs(groupBR, prefix, group, suffix, matchline)
    endif

    " Fourth step:  Set the arguments for searchpair().
    let i   = matchend(group, s:even_backslash.':')
    let j   = matchend(group, '.*'.s:even_backslash.':')
    let ini = strpart(group, 0, i-1)
    let mid = substitute(strpart(group, i,j-i-1), s:even_backslash.'\zs:', '\\|', 'g')
    let fin = strpart(group, j)

    "Un-escape the remaining , and : characters.
    let ini = substitute(ini, s:even_backslash . '\zs\\\(:\|,\)', '\1', 'g')
    let mid = substitute(mid, s:even_backslash . '\zs\\\(:\|,\)', '\1', 'g')
    let fin = substitute(fin, s:even_backslash . '\zs\\\(:\|,\)', '\1', 'g')

    " searchpair() requires that these patterns avoid \(\) groups.
    let ini = substitute(ini, s:even_backslash . '\zs\\(', '\\%(', 'g')
    let mid = substitute(mid, s:even_backslash . '\zs\\(', '\\%(', 'g')
    let fin = substitute(fin, s:even_backslash . '\zs\\(', '\\%(', 'g')

    if   a:fwd && matchline =~ prefix.fin.suffix
    \|| !a:fwd && matchline =~ prefix.ini.suffix
        let mid = ''
    endif

    let flag =  a:fwd && matchline =~ prefix.fin.suffix
          \||  !a:fwd && matchline !~ prefix.ini.suffix
          \?        'bW'
          \:        'W'

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
    let sp_return = searchpair(ini, mid, fin, flag, skip)
    let final_position = 'call cursor('.line('.').','.col('.').')'
    " Restore cursor position and original screen.
    call winrestview(view)
    norm! m'
    if sp_return > 0
        exe final_position
    endif
    call s:clean_up(old_ic, a:mode, startline, startcol, mid.'\|'.fin)
endfu

" Variables {{{1

let s:last_mps = ''
let s:last_words = ':'
let s:patBR = ''

" an even number of consecutive backslashes

"                            ┌ no slash before an even number of slashes
"                       ┌────┤
let s:even_backslash = '\\\@<!\%(\\\\\)*'
"                                └──┤
"                                   └ 2 slashes
