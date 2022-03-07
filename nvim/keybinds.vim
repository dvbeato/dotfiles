" Diogo Beato vimrc configurations
" diogobeato.com - @dvbeato

"TODO
"learn how to scroll down without new lines
"learn how to add new line and keep parentheses as first char

nnoremap <SPACE> <Nop>
let mapleader=" "

" panels - easier navigation
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h
"
" Delete without copy
nnoremap x "_x
nnoremap X "_X
vnoremap p "_dP

" exit term
tnoremap <ESC><ESC> <C-\><C-N>

" File Explorer
nmap <Leader>1  :NvimTreeToggle<CR>

" WhichKeys
call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
set timeoutlen=500

let g:which_key_map =  {}

" Finds
nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>fb :Buffers<CR>
nnoremap <silent> <leader>fe :Rg<CR>
nnoremap <silent> <leader>fc :Commands<CR>
let g:which_key_map.f = {
      \ 'name' : '+find',
      \ 'f' : 'find file',
      \ 'b' : 'find buffer',
      \ 'e' : 'find expression',
      \ 'c' : 'find command',
      \ }

" Terminal
nnoremap <silent> <Leader>to :VimuxOpenRunner<CR>
nnoremap <silent> <Leader>tr :VimuxPromptCommand<CR>
let g:which_key_map.t = {
      \ 'name' : '+terminal',
      \ 'o' : 'terminal open',
      \ 'r' : 'terminal run',
      \ }

" Utils
nmap <silent> <leader>urc :source $MYVIMRC<CR>
nmap <silent> <leader>uoc :edit! $MYVIMRC<CR>
let g:which_key_map.u = {
      \ 'name' : '+utils',
      \ 'oc' : 'open config',
      \ 'rc' : 'reload config',
      \ }

" Git
nnoremap <Leader>gs :Git<CR>
nnoremap <Leader>gb :Git blame<CR>
let g:which_key_map.g = {
      \ 'name' : '+git',
      \ 's' : 'git status',
      \ 'b' : 'git blame',
      \ }

" Clojure
nnoremap <silent> <Leader>ceb :ConjureEvalBuf<CR>
nnoremap <silent> <Leader>cef :ConjureEvalCurrentForm<CR>
let g:which_key_map.c = {
      \ 'name' : '+clojure',
      \ 'eb' : 'eval buffer',
      \ 'ef' : 'eval form',
      \ }

