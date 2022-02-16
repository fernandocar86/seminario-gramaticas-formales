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

## Gramática con slash y rasgo subcat


```python
nltk.data.show_cfg('gramaticas/GramaticaSlash.fcfg')
```


```python
sentence_slash_grammar = 'quién dice el chico que estornuda'
sentence = sentence_slash_grammar.split()
from nltk import load_parser
cp = load_parser('gramaticas/GramaticaSlash.fcfg', trace=2)
for tree in cp.parse(sentence):
     print(tree)
```


```python

```
