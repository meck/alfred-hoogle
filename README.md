# Alfred-Hoogle

### A workflow for searching [hoogle](https://hoogle.haskell.org) with [Alfred](https://www.alfredapp.com)

This was made for my own use, and to test my library for writing Alfred workflows in Haskell [alfred-footman](https://github.com/meck/alfred-footman). As such its a bit over engineered for the task.

I will be adding support for searching a local hoogle database as well as online.

![Demo](/docs/Screenshot.png)


#### Some Features
* <kbd>Enter</kbd> on a result opens the docs in a browser
* <kbd>Shift</kbd> shows the package and links to docs
* <kbd>Alt</kbd> shows the module and links to docs
* <kbd>Ctrl</kbd> previews the docs in the subtitle

#### Building
1. Ensure ```stack``` is installed
2. Run ```./buildworkflow.sh``` this makes ```Hoggle.alfredworkflow```
3. Open it with Alfred
