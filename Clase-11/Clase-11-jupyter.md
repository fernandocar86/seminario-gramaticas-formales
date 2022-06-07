# Parser de dependencias

## Requerimientos
- spaCy

## Documentación
- https://spacy.io/usage/linguistic-features

## Demo parser de dependencias
- https://explosion.ai/demos/displacy


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
nlp_sm = spacy.load("es_core_news_sm")
nlp_sm.pipeline
```




    [('tok2vec', <spacy.pipeline.tok2vec.Tok2Vec at 0x7f50b6d8f160>),
     ('morphologizer',
      <spacy.pipeline.morphologizer.Morphologizer at 0x7f50b6d8f460>),
     ('parser', <spacy.pipeline.dep_parser.DependencyParser at 0x7f50b6ce3e40>),
     ('attribute_ruler',
      <spacy.pipeline.attributeruler.AttributeRuler at 0x7f50b6c4f500>),
     ('lemmatizer',
      <spacy.lang.es.lemmatizer.SpanishLemmatizer at 0x7f50b6c529c0>),
     ('ner', <spacy.pipeline.ner.EntityRecognizer at 0x7f50b6ce3cf0>)]




```python
doc_sm = nlp_sm("Probemos el modelo.")
doc_sm.vocab.vectors.shape
```




    (0, 0)




```python
spacy.displacy.render(doc_sm, style="dep")
```


<span class="tex2jax_ignore"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:lang="es" id="2f551f0e2ceb4a229cc1ed5a4a4e0dfd-0" class="displacy" width="575" height="312.0" direction="ltr" style="max-width: none; height: 312.0px; color: #000000; background: #ffffff; font-family: Arial; direction: ltr">
<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="50">Probemos</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="50">VERB</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="225">el</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="225">DET</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="400">modelo.</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="400">NOUN</tspan>
</text>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-2f551f0e2ceb4a229cc1ed5a4a4e0dfd-0-0" stroke-width="2px" d="M245,177.0 C245,89.5 395.0,89.5 395.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-2f551f0e2ceb4a229cc1ed5a4a4e0dfd-0-0" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">det</textPath>
    </text>
    <path class="displacy-arrowhead" d="M245,179.0 L237,167.0 253,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-2f551f0e2ceb4a229cc1ed5a4a4e0dfd-0-1" stroke-width="2px" d="M70,177.0 C70,2.0 400.0,2.0 400.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-2f551f0e2ceb4a229cc1ed5a4a4e0dfd-0-1" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">obj</textPath>
    </text>
    <path class="displacy-arrowhead" d="M400.0,179.0 L408.0,167.0 392.0,167.0" fill="currentColor"/>
</g>
</svg></span>



```python
nlp = spacy.load("es_core_news_md")
nlp.pipeline
```


    ---------------------------------------------------------------------------

    OSError                                   Traceback (most recent call last)

    Input In [6], in <cell line: 1>()
    ----> 1 nlp = spacy.load("es_core_news_md")
          2 nlp.pipeline


    File ~/repos/seminario-gramaticas-formales/venv/lib/python3.8/site-packages/spacy/__init__.py:51, in load(name, vocab, disable, exclude, config)
         30 def load(
         31     name: Union[str, Path],
         32     *,
       (...)
         36     config: Union[Dict[str, Any], Config] = util.SimpleFrozenDict(),
         37 ) -> Language:
         38     """Load a spaCy model from an installed package or a local path.
         39 
         40     name (str): Package name or model path.
       (...)
         49     RETURNS (Language): The loaded nlp object.
         50     """
    ---> 51     return util.load_model(
         52         name, vocab=vocab, disable=disable, exclude=exclude, config=config
         53     )


    File ~/repos/seminario-gramaticas-formales/venv/lib/python3.8/site-packages/spacy/util.py:427, in load_model(name, vocab, disable, exclude, config)
        425 if name in OLD_MODEL_SHORTCUTS:
        426     raise IOError(Errors.E941.format(name=name, full=OLD_MODEL_SHORTCUTS[name]))  # type: ignore[index]
    --> 427 raise IOError(Errors.E050.format(name=name))


    OSError: [E050] Can't find model 'es_core_news_md'. It doesn't seem to be a Python package or a valid path to a data directory.



```python
doc = nlp("Probemos el modelo.")
doc.vocab.vectors.shape
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [7], in <cell line: 1>()
    ----> 1 doc = nlp("Probemos el modelo.")
          2 doc.vocab.vectors.shape


    NameError: name 'nlp' is not defined



```python
spacy.displacy.render(doc, style="dep")
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [8], in <cell line: 1>()
    ----> 1 spacy.displacy.render(doc, style="dep")


    NameError: name 'doc' is not defined



```python
for token in doc:
    print(token.text, token.vector)
    print("-----------------------")
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [9], in <cell line: 1>()
    ----> 1 for token in doc:
          2     print(token.text, token.vector)
          3     print("-----------------------")


    NameError: name 'doc' is not defined



```python
doc1 = nlp("Probemos el modelo")
doc2 = nlp("Ah la flauta")
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [10], in <cell line: 1>()
    ----> 1 doc1 = nlp("Probemos el modelo")
          2 doc2 = nlp("Ah la flauta")


    NameError: name 'nlp' is not defined



```python
print(doc1, "<->", doc2, doc1.similarity(doc2))
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [11], in <cell line: 1>()
    ----> 1 print(doc1, "<->", doc2, doc1.similarity(doc2))


    NameError: name 'doc1' is not defined



```python
doc1_vec = doc1.vector
doc2_vec = doc2.vector
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [12], in <cell line: 1>()
    ----> 1 doc1_vec = doc1.vector
          2 doc2_vec = doc2.vector


    NameError: name 'doc1' is not defined



```python
type(doc1_vec)
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [13], in <cell line: 1>()
    ----> 1 type(doc1_vec)


    NameError: name 'doc1_vec' is not defined



```python
len(doc1_vec)
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [14], in <cell line: 1>()
    ----> 1 len(doc1_vec)


    NameError: name 'doc1_vec' is not defined



```python
for token in doc:
    print(token.text, token.dep_, token.head.text, token.head.pos_,
            [child for child in token.children])
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [15], in <cell line: 1>()
    ----> 1 for token in doc:
          2     print(token.text, token.dep_, token.head.text, token.head.pos_,
          3             [child for child in token.children])


    NameError: name 'doc' is not defined



```python
doc = nlp("¿quiénes dijo Juan que estornudaron?")
spacy.displacy.render(doc, style="dep")
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [16], in <cell line: 1>()
    ----> 1 doc = nlp("¿quiénes dijo Juan que estornudaron?")
          2 spacy.displacy.render(doc, style="dep")


    NameError: name 'nlp' is not defined



```python
for token in doc:
    print(token.text, token.) 
```


      Input In [17]
        print(token.text, token.)
                                ^
    SyntaxError: invalid syntax




```python
spacy.pipeline.dep_parser.DependencyParser?
```

{% include additional_content.html %}

{% include copybutton.html %}


