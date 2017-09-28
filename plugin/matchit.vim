if exists('g:loaded_matchit')
    finish
endif
let g:loaded_matchit = 1

" Mappings {{{1

nno <silent> %      :<c-u>let g:motion_to_repeat = '%'  <bar> call matchit#next_word(1, 'n')<cr>
nno <silent> g%     :<c-u>let g:motion_to_repeat = 'g%' <bar> call matchit#next_word(0, 'n')<cr>

xno <silent> %      :<c-u>call matchit#next_word(1, 'v')<cr>m'gv``
xno <silent> g%     :<c-u>call matchit#next_word(0, 'v')<cr>m'gv``

ono <silent> %     v:<c-u>call matchit#next_word(1, 'o')<cr>
ono <silent> g%    v:<c-u>call matchit#next_word(0, 'o')<cr>

" Analogues of [{ and ]} using matching patterns:

nno <silent> ]%     :<c-u>let g:motion_to_repeat = ']%' <bar> call matchit#next_unmatched(1, 'n')<cr>
nno <silent> [%     :<c-u>let g:motion_to_repeat = '[%' <bar> call matchit#next_unmatched(0, 'n')<cr>

xno <silent> ]%     :<c-u>call matchit#next_unmatched(1, 'v')<cr>m'gv``
xno <silent> [%     :<c-u>call matchit#next_unmatched(0, 'v')<cr>m'gv``

ono <silent> ]%    v:<c-u>call matchit#next_unmatched('W',  'o')<cr>
ono <silent> [%    v:<c-u>call matchit#next_unmatched('bW', 'o')<cr>

" Text object:

" TODO:
" get rid of `v_a%` in vimrc?
xmap a% o<esc>[%v%
