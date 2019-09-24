# emacs-conf

## Install emacs on mac os
```
brew install emacs-plus --with-modern-icon
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications
```

## Install spacemacs & my config
```
git clone ssh://git@nitta.io:2222/penskoi/emacs-conf.git
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s Documents/emacs-conf/.spacemacs
```

## Dependency

### Markdown support
```
brew install markdown
npm install -g vmd
```

### Haskell support
```
stack install apply-refact hlint stylish-haskell hasktags hoogle
```

### Utils
```
brew install the_silver_searcher
```
