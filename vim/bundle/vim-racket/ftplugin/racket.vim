" Language:     Racket
" Maintainer:   Will Langstroth <will@langstroth.com>
" URL:          http://github.com/wlangstroth/vim-racket
" Last Change:  2011-04-12

setl iskeyword+=#,%,^
setl lispwords+=module,module*,module+,parameterize,let-values,let*-values,letrec-values
setl lispwords+=define-values,opt-lambda,case-lambda,syntax-rules,with-syntax,syntax-case
setl lispwords+=define-signature,unit,unit/sig,compund-unit/sig,define-values/invoke-unit/sig
setl lispwords+=for
setl lisp

" Set mappings to execute and test the buffers.
" TODO: Change it so the mappings can be set in vimrc.
nmap <leader>t :!raco test %<CR>
nmap <leader>r :!racket %<CR>
