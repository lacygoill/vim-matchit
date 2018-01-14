if exists('g:loaded_matchit')
    finish
endif
let g:loaded_matchit = 1

" Mappings {{{1

noremap  <expr><silent><unique>   %   matchit#percent_rhs(1)
noremap  <expr><silent><unique>  g%   matchit#percent_rhs(0)

nno  <silent>  <plug>(matchit-next-word-normal)   :<c-u>call matchit#next_word('n')<cr>
" TODO:
" Try to remove  m'gv``  and use `norm! gv` inside `matchit#next_word()` instead.
xno  <silent>  <plug>(matchit-next-word-visual)   :<c-u>call matchit#next_word('v')<cr>m'gv``
ono  <silent>  <plug>(matchit-next-word-op)      v:<c-u>call matchit#next_word('o')<cr>
"                                                │
"                                                └ by default, `%` and `g%` are inclusive motions:
"                                                  they include the last character (the one nearest
"                                                  from the end of the buffer)
"
"                                                  `matchit#next_word()` invokes `search()` which seems
"                                                  to be an exclusive motion (like `/`):
"                                                  the last character it finds isn't included in the object.
"                                                  We want it, so we add `v`, to make the motion
"                                                  inclusive like the original one.

" Analogues of [{ and ]} using matching patterns:

nno  <silent><unique>  ]%   :<c-u>call matchit#next_unmatched(1, 'n')<cr>
nno  <silent><unique>  [%   :<c-u>call matchit#next_unmatched(0, 'n')<cr>

xno  <silent><unique>  ]%   :<c-u>call matchit#next_unmatched(1, 'v')<cr>m'gv``
xno  <silent><unique>  [%   :<c-u>call matchit#next_unmatched(0, 'v')<cr>m'gv``

ono  <silent><unique>  ]%  v:<c-u>call matchit#next_unmatched(1, 'o')<cr>
ono  <silent><unique>  [%  v:<c-u>call matchit#next_unmatched(0, 'o')<cr>

" Text object:

xmap  <unique>  a%  o<esc>[%v%
ono   <unique>  a%  vo<esc>[%v%
