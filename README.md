# Alfred-Hoogle

### A [Alfred](https://www.alfredapp.com) workflow for searching [Hoogle](https://hoogle.haskell.org)

This was made for my own use and to test my library for writing Alfred workflows in Haskell [alfred-footman](https://github.com/meck/alfred-footman). It can search locally and online simultaneously while filtering the results for duplicates. The default keyword is `ho`

![Demo](/docs/Screenshot.png)

### Possible actions on results
* <kbd>Enter</kbd> on a result opens the item docs in a browser.
* <kbd>Enter</kbd>+<kbd>Shift</kbd> opens the package docs of the item on Hackage or locally.
* <kbd>Enter</kbd>+<kbd>Ctrl</kbd> opens the module docs of the item if any on Hackage or locally.
* <kbd>Alt</kbd>+<kbd>Enter</kbd> opens the package docs of the item on Stackage.
* <kbd>Cmd</kbd> show the docs if any below the item. 
* <kbd>Cmd</kbd>+<kbd>Y</kbd> or a tap on <kbd>Shift</kbd> opens a quicklook window.
* <kbd>Cmd</kbd>+<kbd>C</kbd> copies the function name.

## Installation
Download binary from releases or check build section

## Settings
Settings are available via the `hoset` keyword

### Use a alternative server
Enabling this changes the server address from `hoogle.haskell.org` to `localhost:8080`,
for use with commands like `stack hoogle --server`.

### Use a local database
Local search is disabled by default, enable it from the settings. 
When searching next you will be prompted to build a database.
The indexing takes a while but reports back when its done.

The default source for the database is stackage.
It can be set to a local folder with a Alfred file action,
any packages in this folder will be indexed then.

It's possible to trigger a rebuild of the database anytime if needed.

If both online and local search is enabled results will be merged and filtered for duplicates,
by default the results from the local search take precedence, this changeable in the settings.

### Workflow variables
Some settings are available as workflow variables in Alfred:
- The alternate address, default `http://localhost:8080`
- The number of local search results, default `15`
- The number of web search results, default `15`

### External trigger
For triggering via applescript, as an example use it with Vim to lookup the word under the cursor:
``` vimscript
nnoremap <silent><Leader>ho :silent execute 
    \ ':!/usr/bin/osascript -e '
    \ . shellescape(
    \ 'tell application id "com.runningwithcrayons.Alfred"
    \ to run trigger "ext_trig"
    \ in workflow "se.meck.alfred-hoogle"
    \ with argument "' . expand('<cword>') . '"'
    \ )<CR>
``` 
### Building
1. Ensure `stack` is installed
2. Run `./build.hs` script, this makes `Hoggle.alfredworkflow`
3. Open it with Alfred
