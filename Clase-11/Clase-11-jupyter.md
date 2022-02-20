# Parsers de dependencias

## Requerimientos
- Python 3
- Spacy


```python
import spacy
import nltk
import re
from nltk import Tree
from spacy import displacy 
from nltk.parse import malt
```

No poseen distinción entre símbolos no terminales y terminales. Las estructuras representan relaciones de dependencia entre terminales.
Ejemplos de parsers de dependencias:
* Projective Dependency Parser de NLTK (https://www.nltk.org/_modules/nltk/parse/projectivedependencyparser.html)
* Maltparser (http://www.maltparser.org/)
* SyntaxNet (Estaba alojado en https://opensource.google.com/projects/syntaxnet, como parte de los recursos de la librería para Inteligencia Artificial TensorFlow de Google, pero en este momento no está disponible y se [rumorea](https://github.com/tensorflow/models/issues/8411) que se lo va a mover al github de [google-research](https://github.com/google-research/google-research))
* Dependency parser de Spacy (https://spacy.io/usage/linguistic-features#dependency-parse)

## Dependency Parser NLTK


```python
def dep_parser(sentence, grammar):                   # define una función llamada dep_parser con dos argumentos
    sentence = sentence.lower()                     # convierte a minúscula la oración
    if sentence.endswith('.'):                      # si la oración termina con un punto
        sent = re.sub('\.',' ',sentence)            # se lo quita
    else:                                           # si no
        sent = sentence                             # la toma como está
    sent = sent.split()                             # divide la oración en palabras
    dep_gram = nltk.data.load(grammar)              # carga la gramática a nltk
    dep_gram = nltk.DependencyGrammar.fromstring(dep_gram) # parsea la gramática como gramática de dependencias
    pdp = nltk.ProjectiveDependencyParser(dep_gram) # Carga la gramática en el parser
    print(dep_gram)                                  # imprime mi gramática
    for tree in pdp.parse(sent):              # para cada árbol posible en mi gramática para esa oración
        print(tree)                                 # lo imprimo
        return(tree)
```


```python
#Para correr el Proyective Dependency Parser

oracion1 = 'Pablo explotó el globo'  # Define la oración a analizar
grammar = 'gramaticas/DG1.txt'       # establece cuál va a ser mi gramática
dep_parser(oracion1, grammar)        # Para correr la función
```

## Spacy - Dependency parser

### Nota para quien no tenga la MV: 

Antes de correr hay que instalar spacy. Con pip3, eso se puede hacer con el comando 

`pip3 install spacy`

Hay que instalar también es_core_news_sm, un modelo entrenado mediante un corpus del español, con el comando

`python3 -m spacy download es_core_news_sm`

Alternativamente puede probarse de instalar es_core_news_md.

`python3 -m spacy download es_core_news_md`

En ese caso, para correrlo hay que cambiar en el código de abajo `es_core_news_sm` por `es_core_news_md`


```python
def gramaticadependencias(sentence):       #Define la función
    nlp = spacy.load('es_core_news_sm')    #Carga el modelo entrenado
    doc = nlp(sentence)                    #define una variable doc con la oración procesada por el modelo
    #for token in doc:               
        #print(token.text, token.dep_, token.head.text, token.head.pos_,
        #    [child for child in token.children])
    displacy.render(doc, style='dep', jupyter=True)

```


```python
print('Escribí una oración')
oracion5 = input()
gramaticadependencias(oracion5)
```

## Malt Parser

### Instrucciones

Crear la carpeta malt y adentro bajar los siguientes archivos:
- Malt Parser de [http://www.maltparser.org/download.html](http://www.maltparser.org/download.html)
- Bajar el modelo entrenado engmalt.poly-1.7 de [http://www.maltparser.org/mco/english_parser/engmalt.html](http://www.maltparser.org/mco/english_parser/engmalt.html)



```python
def maltparser(sentence, parserversion, langmodel):
    mp = malt.MaltParser(paserversion, langmodel)
    stemma = mp.parse_one(sentence.split()).tree()
    print(stemma)
    return(stemma)
```


```python
parserversion = 'malt/maltparser-1.9.2' # Define la versión del parser. 
langmodel = 'malt/engmalt.poly-1.7.mco'# Define el modelo entrenado poly
#lamgmodel = 'engmalt.linear-1.7.mco' # Defuine el modelo entrenado linear
print('Escribí una oración')
oracion8 = input()
maltparser(oracion8, parserversion, langmodel)
```
