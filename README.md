# haskellstein

[![Build Status](https://travis-ci.org/cmc-haskell-2018/haskellstein.svg?branch=master)](https://travis-ci.org/cmc-haskell-2018/haskellstein)

Игра в жанре шутер от первого лица.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec haskellstein tilemaps/testlvl.txt
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

