# emacs-conf

## Emacs on Mac OS
```sh
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-modern-icon
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
```

## Doom emacs
```sh
brew install coreutils git ripgrep fd clang
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

## Install my config
```sh
git clone ssh://git@nitta.io:2222/penskoi/emacs-conf.git ~/.doom.d
~/.emacs.d/bin/doom sync
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
brew install the_silver_searcher
```

