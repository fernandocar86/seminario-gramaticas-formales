# Gramáticas basadas en rasgos

## Requerimientos técnicos

- Python 3
- nltk


```python
import nltk 
import re 
import os, sys
import matplotlib 
```

Las gramáticas se pueden enriquecer con el uso de rasgos. Los rasgos son pares de atributo valor. En una gramática con rasgos, los rasgos se heredan de las entradas léxicas a los nodos superiores. Las reglas especifican los rasgos que sus nodos hijos deben compartir.


```python
nltk.data.show_cfg('gramaticas/GramaticaDeRasgos.fcfg')
```


```python
sentence = 'los chicas caminan'
tokens = sentence.split()
print(sentence)
print(type(sentence))
print(tokens)
print(type(tokens))
```


```python
from nltk import load_parser
cp = load_parser('gramaticas/GramaticaDeRasgos.fcfg', trace=2)
for tree in cp.parse(tokens):
     print(tree)
```

## Uso de rasgos para significado

Los rasgos pueden utilizarse a su vez para construir una representación semántica de las oraciones.

En semántica formal existe una función particular, que se conoce con el nombre de función interpretación, y que se anota con corchetes dobles. La función interpretación devuelve por cada expresión lingüística su denotación. Las denotaciones pueden ser de dos tipos: 

- elementos atómicos (típicamente objetos o proposiciones, pero también hay otras ontologías que incluyen mundos posibles, eventos y tiempos, entre otras cosas)
- funciones. 

El uso de rasgos para dar cuenta del significado consiste en que la función denotación sea el valor de un rasgo semántico.

En el [libro de NLTK](https://www.nltk.org/book/ch10.html) y en la [documentación de NLTK](http://nltk.sourceforge.net/doc/en/ch11.html) hay información sobre cómo implementar esto mediante una gramática de rasgos en NLTK.


Las funciones equivalen a conjuntos y se expresan en el llamado cálculo lambda. 

\x. x fuma

Esta es una función que toma un x y devuelve verdadero si x fuma y falso si x no fuma. En términos de conjuntos equivale al conjunto de todos los fumadores (ténicamente equivale al conjunto característico de todos los fumadores, que es el que devuelve verdadero si x fuma y falso si x no fuma).

Hay dos operaciones básicas de cálculo lambda que son particularmente relevantes (una tercera no tuvo tanta repercusión en la semántica formal):

- Conversión alpha (o reducción alpha): cambiar el nombre de una variable y, conjuntamente, el de todas las variables ligadas con ella.
`[\x. x fuma] = [\y. y fuma] = [\z. z fuma] = ...`
- Conversión lambda (o reducción beta): cuando combinamos una función con un argumento, eliminar el prefijo lambda y reemplazar todas las ocurrencias de la variable que introduce ese prefijo por el argumento.

- `[\x. x fuma](cata) = cata fuma`
- `[\f. f](\x. x fuma) = \x. x fuma`
- `[\f. [\g. [\x. g(x)=f(x)=1]]](\x. x fuma)(\x. x baila) = [\x. [\x. x fuma](x)=[\x. x baila](x)=1] = [\x x fuma y x baila]` 

Las interpretación semántica de las expresiones lingüísticas se da a partir del significado de sus partes y su combinación mediante reglas que dependen de la forma del árbol y de los tipos de las funciones. Las reglas más frecuentes son: 

- Aplicación funcional: Si un nodo A domina a dos nodos B y C tales que B es una función cuyo dominio incluye a C, entonces [[A]]=[[B]]([[C]])
- Modificación de predicados: Si un nodo A domina a dos nodos B y C tales que los dos nodos son funciones que van del dominio de los individuos al dominio de los valores de verdad, entonces [[A]] = \x. [[B]]=[[C]]=1

Las fcfg implementan la función intepretación como valor de un rasgo semántico y reemplazando las reglas que dependen de la forma del árbol directamente por restricciones en las reglas de reescritura. Estas restricciones consisten básicamente en la unificación mediante variables idénticas.


```python
nltk.data.show_cfg('pruebasemantica.fcfg')
```


```python
sents = ['Cata fuma']
grammar = 'pruebasemantica.fcfg'
for results in nltk.interpret_sents(sents, grammar):
    for (synrep, semrep) in results:
             print(synrep)
```

## Semántica eventiva

Un tipo de semántica formal que hoy ha ganado mucha popularidad es la semántica eventiva. La semántica eventiva concibe el significado de las oraciones como cuantificación sobre eventos: 

- Juan le dio ayer el libro a Pedro en la casa.
- Existe un e tal que Agente(e, Juan) & Tema(e, el libro) & Meta(e, Pedro) & en(e, la casa) & Pasado(e)

La semántica neodavidsoniana permite dispensar de grandes listas de subclases (verbos transitivos, verbos intransitivos, verbos ditransitivos, verbos bivalentes, verbos impersonales, verbos que toman distintos tipos de sintagmas preposicionales) y simplificar, en consecuencia, el léxico. Esto se hace al costo de dar una denotación más compleja a las entradas léxicas (que sin embargo, se puede automatizar si se tienen listas léxicas como las de freeling) y a las reglas de reescritura. Este tipo de enfoques propenden a sobregenerar, pero, naturalmente, este no es un problema si lo que nos interesa es parsear oraciones.


```python
sents = ['Cata dio Ficciones a Chafa']
grammar = 'semantica_eventiva_base.fcfg'
for results in nltk.interpret_sents(sents, grammar):
    for (synrep, semrep) in results:
             print(synrep)
```
