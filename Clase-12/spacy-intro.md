# Otras aplicaciones de spaCy

- Requerimientos: spaCy
- [Documentación](https://spacy.io/usage/linguistic-features)
- [Demo de _parser_ de dependencias](https://explosion.ai/demos/displacy)


```python
import spacy
```


```python
# Para ver los idiomas y modelos disponibles por idioma: https://spacy.io/usage/models
# Se puede entrar a cada modelo y ver en qué tareas y corpus fue entrenado

#!python -m spacy download en_core_web_md
#!python -m spacy download es_core_news_sm
#!python -m spacy download es_core_news_md
```


```python
# Para ver los idiomas que tenemos descargados
# Observar qué sucede con la versión de la librería y la de los modelos

spacy.cli.validate()
```


```python
nlp_sm = spacy.load("es_core_news_sm")
```


```python
# Me muestra qué tiene el pipeline del modelo cargado
nlp_sm.pipe_names
```


```python
# Me muestra a qué objeto en Python se asocian los nombres anteriores
nlp_sm.pipeline
```


```python
# Me devuelve un diccionario cuyas keys son los nombres de los componentes
# del pipe y sus values, las etiquetas que puede asignar ese componente
nlp_sm.pipe_labels['parser']
```


```python
sent = "Probemos el modelo."
doc_sm = nlp_sm(sent)
doc_sm
```


```python
# ¿Qué tipo de objeto es doc_sm?
#type(doc_sm)
```


```python
#for s in sent:
#    print(s)
```


```python
#for s in doc_sm:
#    print(s)
```


```python
print('{0:10}{1:8}{2:10}{3:8}{4:20}'.format('TOKEN','DEP TAG','NÚCLEO','POS TAG','DEPENDENDIENTES'))
for token in doc_sm:
    print('{0:10}{1:8}{2:10}{3:8}{4:20}'.format(
        token.text,
        token.dep_,
        token.head.text,
        token.head.pos_,
        str([child for child in token.children])
    ))
```


```python
spacy.displacy.render(doc_sm, style="dep")
```


```python
# Con el parámetro disable puedo desabilitar los componentes del modelo
# que no necesito
nlp_sm_smaller = spacy.load('es_core_news_sm', disable=['parser'])
nlp_sm_smaller.pipe_names
```

En [esta página](https://spacy.io/usage/processing-pipelines) pueden encontrar la documentación de spaCy sobre _pipelines_.


```python
# Muestra de qué tamaño son los vectores asignados al vocabulario del modelo
nlp_sm.vocab.vectors.shape
```


```python
nlp = spacy.load("es_core_news_md")
nlp.pipe_names
```


```python
nlp.vocab.vectors.shape
```


```python
doc = nlp("Probemos el modelo.")

for token in doc:
    print('{0:10}{1:20}'.format(token.text,str(token.vector[:5])))
```


```python
#nlp('modelo').vector
```


```python
spacy.displacy.render(doc, style="dep")
```


```python
doc1 = nlp("Probemos el modelo")
doc2 = nlp("Ah la flauta")
```


```python
print(doc1, "<->", doc2, ':', doc1.similarity(doc2))
```


```python
doc1_vec = doc1.vector
doc2_vec = doc2.vector
```


```python
type(doc1_vec)
```


```python
len(doc1_vec)
```


```python
print('{0:10}{1:8}{2:10}{3:8}{4:20}'.format('TOKEN','DEP TAG','NÚCLEO','POS TAG','DEPENDENDIENTES'))
for token in doc:
    print('{0:10}{1:8}{2:10}{3:8}{4:20}'.format(
        token.text,
        token.dep_,
        token.head.text,
        token.head.pos_,
        str([child for child in token.children])
    ))
```

{% include additional_content.html %}

{% include copybutton.html %}
