if exists("g:loaded_syntastic_swift_swiftc_checker")
    finish
endif
let g:loaded_syntastic_swift_swiftc_checker = 1

function! SyntaxCheckers_swift_swiftc_GetLocList() dict
    let makeprg = 'xcrun swiftc -parse ' . shellescape(expand("%"))
    let errorformat =
                \ '%E%f:%l:%c: error: %m,' .
                \ '%W%f:%l:%c: warning: %m,' .
                \ '%Z%\s%#^~%#,' .
                \ '%-G%.%#'

    return SyntasticMake({
                \ 'makeprg': makeprg,
                \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
            \ 'filetype': 'swift',
            \ 'name': 'swiftc'})
