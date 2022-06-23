# Gramáticas categoriales con rasgos

## Requerimientos


```python
import nltk 
import re 
from nltk.ccg import lexicon, chart 
from nltk.ccg.chart import printCCGDerivation
```

Las gramáticas categoriales están conformadas principalmente por un conjunto reducido de reglas y un léxico sumamente rico.
Las reglas que utiliza OpenCCG, que es el parser categorial que vamos a ver son las siguientes:

![reglas categoriales](reglascategorialesopenccg.png)

Construir una gramática categorial consiste principalmente en elaborar un léxico lo suficientemente rico, ya que las gramáticas categoriales son fuertemente lexicalistas. En ellas, la categoría a la que pertenece cada entrada léxica codifica sus posibilidades combinatorias.

## Gramática categorial combinatoria con rasgos en NLTK


```python
#Combinatory Categorial Grammar with features
# Esta hay que revisarla y ver cómo se corrige para que funcione

def combinatory_parser(sentence, grammar):   
    sentence = sentence.lower()                                     # convierte a minúscula
    if sentence.endswith('.'):                                      # si la oración termina con un punto
        sent = re.sub('\.',' ',sentence)                            # se lo quita
    else:                                                           # si no
        sent = sentence                                             # la toma como está
    sent = sent.split()
    codigogram = nltk.data.load(grammar, cache=False)              # carga la gramática a nltk
    lex = lexicon.fromstring(codigogram)
    print(lex)
    parser = chart.CCGChartParser(lex, chart.DefaultRuleSet)
    parses = list(parser.parse(sent))
    quantparses = len(parses)
    print(str(quantparses) + " parses")
    print(sent)
    parseslist = 0
    def showccgparses(parseslist1, quantparses1):
        if parseslist1 < quantparses1:
            printCCGDerivation(parses[parseslist1])
            parseslist2 = parseslist1 + 1
            showccgparses(parseslist2, quantparses1)
        elif parseslist == quantparses: 
            printCCGDerivation(parses[parseslist])
    if quantparses == 0:
        print('No se encontraron estructuras para imprimir')
    elif quantparses > 0:
        showccgparses(parseslist, quantparses)
```


```python
grammar = 'gramaticas/FCG1.txt'
oracion5 = 'julia fuma'
combinatory_parser(oracion5, grammar)
```


```python
grammar = 'gramaticas/FCG2.txt'
oracion6 = 'ella fuma'
#oracion6 = 'la persona fuma'
#oracion6 = 'la persona fuma el cigarrillo' 
#oracion6 = 'la plaza fuma' # Pregunta a estudiantes: ¿por qué da agramatical?
#oracion6 = 'la persona fuma y yo hablo'
combinatory_parser(oracion6, grammar)
```

## Gramática categorial con anotación semántica


```python
def ccgsemparse(sentence, grammar):
    sentence = sentence.lower()                                     # convierte a minúscula
    if sentence.endswith('.'):                                      # si la oración termina con un punto
        sent = re.sub('\.',' ',sentence)                            # se lo quita
    else:                                                           # si no
        sent = sentence                                             # la toma como está
    sent = sent.split()   
    codigogram = nltk.data.load(grammar, cache=False)              # carga la gramática a nltk
    print(codigogram)
    print(type(codigogram))
    lex = lexicon.fromstring(codigogram, True)
    print(lex)
    parser = chart.CCGChartParser(lex, chart.DefaultRuleSet)
    parses = list(parser.parse(sent))
    quantparses = len(parses)
    print(str(quantparses) + " parses")
    print(sent)
    parseslist = 0
    def showccgparses(parseslist1, quantparses1):
        if parseslist1 < quantparses1:
            printCCGDerivation(parses[parseslist1])
            parseslist2 = parseslist1 + 1
            showccgparses(parseslist2, quantparses1)
        elif parseslist == quantparses: 
            printCCGDerivation(parses[parseslist])
    if quantparses == 0:
        print('No se encontraron estructuras para imprimir')
    elif quantparses > 0:
        showccgparses(parseslist, quantparses)
```


```python
oracion6= "pablo fuma"
#oracion6 = 'juan dio fernando a romi'
grammar3 = 'gramaticas/SCG1.txt'
ccgsemparse(oracion6, grammar3)
```


```python
oracion6 = 'I give them money'
grammar3 = 'gramaticas/SCG2.txt'
ccgsemparse(oracion6, grammar3)
```


```python

```
