if exists('g:loaded_matchit')
    finish
endif
let g:loaded_matchit = 1

" Mappings {{{1

nno <silent><unique>  %    :<c-u>call matchit#next_word(1, 'n')<cr>
nno <silent><unique>  g%   :<c-u>call matchit#next_word(0, 'n')<cr>

xno <silent><unique>  %    :<c-u>call matchit#next_word(1, 'v')<cr>m'gv``
xno <silent><unique>  g%   :<c-u>call matchit#next_word(0, 'v')<cr>m'gv``

ono <silent><unique>  %   v:<c-u>call matchit#next_word(1, 'o')<cr>
ono <silent><unique>  g%  v:<c-u>call matchit#next_word(0, 'o')<cr>

" Analogues of [{ and ]} using matching patterns:

nno <silent><unique> ]%   :<c-u>call matchit#next_unmatched(1, 'n')<cr>
nno <silent><unique> [%   :<c-u>call matchit#next_unmatched(0, 'n')<cr>

xno <silent><unique> ]%   :<c-u>call matchit#next_unmatched(1, 'v')<cr>m'gv``
xno <silent><unique> [%   :<c-u>call matchit#next_unmatched(0, 'v')<cr>m'gv``

ono <silent><unique> ]%  v:<c-u>call matchit#next_unmatched(1, 'o')<cr>
ono <silent><unique> [%  v:<c-u>call matchit#next_unmatched(0, 'o')<cr>

" Text object:

xmap <unique>  a%  o<esc>[%v%
ono  <unique>  a%  vo<esc>[%v%
