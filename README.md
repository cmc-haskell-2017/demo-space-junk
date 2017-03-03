# demo-space-junk

Моделирование космического мусора.

![Космический мусор.](images/demo-space-junk.gif)

## Сборка и запуск

Клонируйте репозиторий:

```
git clone https://github.com/cmc-haskell-2017/demo-space-junk.git
cd demo-space-junk
```

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать проект и запустить игру можно при помощи команды

```
stack build && stack exec demo-space-junk
```

## Задание

В качестве задания к [лекции «Классы типов»](https://youtu.be/efwK257k47o) требуется
определить законы, которым должен удовлетворять [класс `Physical`](https://github.com/cmc-haskell-2017/demo-space-junk/blob/master/src/SpaceJunk.hs#L46-L53).

Запишите законы в комментариях к классу в виде проверяемых свойств:

```haskell
-- prop> \(object :: Asteroid) -> <выражение типа Bool, которое использует object>
```

В качестве примера такой формулировки законов, посмотрите на формулировки для [функции `move`](https://github.com/cmc-haskell-2017/demo-space-junk/blob/master/src/SpaceJunk.hs#L55-L62):

```haskell
-- prop> \(object :: Asteroid) -> move 0 object == object
-- prop> \(object :: Asteroid) -> move x (move y object) ~= move (x + y) object
```

Чтобы проверить выполнение записанных вами законов, запустите тесты:

```
stack test
```

Свойства из комментариев будут автоматически проверяться на сгенерированных случайных объектах!
Если свойство не выполняется, на экран будут выведены значения `object` и всех свободных переменных (например, `x` и `y` во втором свойстве функции `move`), которые привели к нарушению свойства.
