# Gramáticas categoriales

## Requerimientos


```python
import nltk 
import re 
from nltk.ccg import chart, lexicon
```

Las gramáticas categoriales están conformadas principalmente por un conjunto reducido de reglas y un léxico sumamente rico.
Las reglas que utiliza OpenCCG, que es el parser categorial que vamos a ver son las siguientes:

![reglas categoriales](reglascategorialesopenccg.png)

Construir una gramática categorial consiste principalmente en elaborar un léxico lo suficientemente rico, ya que las gramáticas categoriales son fuertemente lexicalistas. En ellas, la categoría a la que pertenece cada entrada léxica codifica sus posibilidades combinatorias.

## Combinatory Categorial Grammar


```python
#Combinatory Categorial Grammar

def combinatory_parser(sentence, grammar):   
    sentence = sentence.lower()                                     # convierte a minúscula
    if sentence.endswith('.'):                                      # si la oración termina con un punto
        sent = re.sub('\.',' ',sentence)                            # se lo quita
    else:                                                           # si no
        sent = sentence                                             # la toma como está
    sent = sent.split()                                             # divide la oración en palabras
    archivo = open(grammar, 'r')
    codigogram = archivo.read()
    lex = lexicon.fromstring(codigogram)
    print(lex)
    parser = chart.CCGChartParser(lex, chart.DefaultRuleSet)
    archivo.close()
    for parse in parser.parse(sent):  # doctest: +SKIP
         chart.printCCGDerivation(parse)
         #break       

```


```python
grammar = 'gramaticas/CG1.txt'
print('Escribí una oración')
oracion5 = input()
combinatory_parser(oracion5, grammar)
```


```python

```
