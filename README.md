# emacs-conf

```sh
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-native-comp --with-cacodemon-icon --with-no-titlebar-and-round-corners
ln -s /usr/local/opt/emacs-plus@28/Emacs.app /Applications/Emacs.app
brew install ripgrep
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
fish_add_path ~/.config/emacs/bin
git clone git@github.com:ryukzak/emacs-config.git ~/.config/doom
doom sync
```

## Dependency

### Hasklig font
https://github.com/i-tu/Hasklig

### Spellcheck

``` console
> brew install ispell # for checking
> brew install aspell # for dictionary
```

### Markdown support
```
brew install markdown
npm install -g vmd
```

### Haskell support
```
stack install apply-refact hlint stylish-haskell hasktags hoogle
```

### JS support
```
npm install --global tern prettier
```

### Utils
```
brew install ripgrep
```
