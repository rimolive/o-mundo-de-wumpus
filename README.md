O Mundo de Wumpus
-----------------

Essa implementação foi baseada no [Hilios](https://github.com/hilios) usando Prolog.
Basicamente o que foi feito foi mudar os nomes para Português e mudar a heurística
dele de forma que se encaixasse na disposição clássica do mundo de Wumpus.

#### Pré-requisito:

Instalar o programa [SWI-Prolog](http://www.swi-prolog.org/).

#### Executando:

É possível compilar o código usando `swipl` e executar o binário:

```sh
$ swipl -o mundo-de-wumpus -c src/wumpus.pl src/heuristica.pl src/mundo.pl
$ ./mundo-de-wumpus
?- run.
```

Ou pode-se usar a interface do `swipl`:

```shell
$ swipl
?- [src/wumpus], [src/heuristica], [src/mundo].
true.
?- run.
```

#### Mundo aleatório

```shell
$ swipl
?- [src/wumpus], [src/naive].
true.
?- run(random).
```
