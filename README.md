# Alfred-Hoogle

### A [Alfred](https://www.alfredapp.com) workflow for searching [Hoogle](https://hoogle.haskell.org)

This was made for my own use and to test my library for writing Alfred workflows in Haskell [alfred-footman](https://github.com/meck/alfred-footman). It can search locally and online simultaneously while filtering the results for duplicates. The default keyword is `ho`

![Demo](/docs/Screenshot.png)

### Possible actions on results
* <kbd>Enter</kbd> on a result opens the docs in a browser.
* <kbd>Shift</kbd> shows the package and links to docs.
* <kbd>Alt</kbd> shows the module and links to docs.
* <kbd>Ctrl</kbd> previews any available docs in the item subtitle.
* <kbd>Cmd</kbd>+<kbd>Y</kbd> or a tap on <kbd>Shift</kbd> opens a quicklook window.
* <kbd>Cmd</kbd>+<kbd>C</kbd> copies the function name.

## Settings
Settings are available via the `hoset` keyword

### Use a localhost server
Enabling this changes the server address from `hoogle.haskell.org` to `localhost:8080`,
for use with commands like `stack hoogle --server`.

The alternate adress is stored as
a workflow variable in aldfred and can be changed from there.

### Use a local database
Local search is disabled by default, enable it from the settings. 
When searching next you will be promted to build a database.
The indexing takes a while but reports back when its done.

The default source for the database is stackage.
It can be set to a local folder with a Alfred file action,
any packages in this folder will be indexed then.

It's possible to trigger a rebuild of the database anytime if needed.

If both online and local search is enabled results will be merged and filtered for duplicates,
by default the results from the local search take precedence, this changeable in the settings.

### Building
1. Ensure `stack` is installed
2. Run `./buildworkflow.sh` this makes `Hoggle.alfredworkflow`
3. Open it with Alfred
