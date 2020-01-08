Русская текста У тебя есть всё — высокая должность, зарплата в несколько сотен
тысяч рублей, надёжность и стабильность государственной корпорации, ранговые
корпоративные игры. У тебя малиновые штаны — и подчинённые разве что не
приседают и не делают «Ку».


# emacs-conf

## Install emacs on mac os
```
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-modern-icon
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
```

## Install doom emacs
```
brew install coreutils git ripgrep fd clang
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

## Install spacemacs & my config
```
git clone ssh://git@nitta.io:2222/penskoi/emacs-conf.git
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s Documents/emacs-conf/.spacemacs
```

## Dependency

### Spellcheck

``` sh
brew install ispell aspell
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
Проверка русскоа текста
