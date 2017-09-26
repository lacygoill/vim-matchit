if exists('g:loaded_matchit')
    finish
endif
let g:loaded_matchit = 1

" Mappings {{{1

nno <silent> %      :<c-u>call <sid>match_wrapper('', 1, 'n')<cr>
nno <silent> g%     :<c-u>call <sid>match_wrapper('', 0, 'n')<cr>
xno <silent> %      :<c-u>call <sid>match_wrapper('', 1, 'v')<cr>m'gv``
xno <silent> g%     :<c-u>call <sid>match_wrapper('', 0, 'v')<cr>m'gv``
ono <silent> %     v:<c-u>call <sid>match_wrapper('', 1, 'o')<cr>
ono <silent> g%    v:<c-u>call <sid>match_wrapper('', 0, 'o')<cr>

" Analogues of [{ and ]} using matching patterns:

nno <silent> [%     :<c-u>call <sid>multi_match('bW', 'n')<cr>
nno <silent> ]%     :<c-u>call <sid>multi_match('W',  'n')<cr>
xmap         [%     <esc>[%m'gv``
xmap         ]%     <esc>]%m'gv``
" xno <silent> [%    :<c-u>call <sid>multi_match('bW', 'v')<cr>m'gv``
" xno <silent> ]%    :<c-u>call <sid>multi_match('W',  'v')<cr>m'gv``
ono <silent> [%    v:<c-u>call <sid>multi_match('bW', 'o')<cr>
ono <silent> ]%    v:<c-u>call <sid>multi_match('W',  'o')<cr>

" text object:
xmap a% o<esc>[%v%

