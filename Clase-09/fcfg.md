# Gramáticas basadas en rasgos

Para esta clase, puede ser de utilidad la siguiente bibliografía:

- [NLTK Book: Chapter 9, Building Feature Based Grammars](https://www.nltk.org/book/ch09.html)
- [NLTK (documentación de librería): Sample usage for featstruct](https://www.nltk.org/howto/featstruct.html)


```python
# importa librería nltk
import nltk
```

## Estructuras de rasgos

<div style="text-align:center">
    <figure>
        <img src="./images/fs_ABC.png" width="60%">
    </figure>
</div>

### Representación con diccionarios

Los diccionarios son un tipo de objeto primitivo de Python en el que podemos agregar entradas, llamadas llaves (_keys_), y asignarles un valor o dato asociado (_value_). Un dato importante para la construcción de diccionarios es que estos no pueden tener _keys_ repetidas. En caso de querer insertar una _key_ que ya se encontraba en un diccionario, se sobreescribirá su valor.

Podemos aprovechar este objeto para representar una estructura de rasgos.


```python
fs_a_dict = {
    'SIGNIFICANTE':'torta',
    'LEXEMA':'torta',
    'CAT':'N',
    'GEN':'fem',
    'PLU':False
}
fs_a_dict
```




    {'SIGNIFICANTE': 'torta',
     'LEXEMA': 'torta',
     'CAT': 'N',
     'GEN': 'fem',
     'PLU': False}




```python
fs_a_dict['GEN']
```




    'fem'




```python
# sobreescribe un valor

fs_a_dict['LEXEMA']='tortas'
fs_a_dict['PLU']=True

fs_a_dict
```




    {'SIGNIFICANTE': 'torta',
     'LEXEMA': 'tortas',
     'CAT': 'N',
     'GEN': 'fem',
     'PLU': True}



`True` y `False` son objetos de tipo booleano, permiten indicar que algo es verdadero o falso, respectivamente. Aquí los usamos para indicar si el atributo _PLU_ está presente o no. También podríamos usar un `string` que indicase "+" o "-", pero este tipo de dato resulta más apropiado para lo que se desea representa.


```python
# arma un diccionario vacío

fs_b_dict = dict()
fs_b_dict
```




    {}




```python
# agrega rasgos de a uno

fs_b_dict['LEX'] = 'virus'
fs_b_dict
```




    {'LEX': 'virus'}




```python
fs_b_dict['CAT'] = 'N'
fs_b_dict
```




    {'LEX': 'virus', 'CAT': 'N'}




```python
# agrega un rasgo complejo

fs_b_dict['CONC'] = {
    'NUM':None,
    'GEN':'masc'
}

fs_b_dict
```




    {'LEX': 'virus', 'CAT': 'N', 'CONC': {'NUM': None, 'GEN': 'masc'}}



`None` es un valor que se utiliza para indicar "nulo". Aquí lo usamos para indiar que el atributo _NUM_ no se encuentra especificaco.


```python
# arma un diccionario vacío

fs_c_dict = dict()
fs_c_dict
```




    {}




```python
# actualiza el diccionario vacío
# con el contenido de otro diccionario

fs_c_dict.update(
    {
        'LEX': 'mutantes',
        'CAT': 'ADJ',
        'CONC': {
            'NUM': 'pl',
            'GEN': 'masc'}
    }
)

fs_c_dict
```




    {'LEX': 'mutantes', 'CAT': 'ADJ', 'CONC': {'NUM': 'pl', 'GEN': 'masc'}}



¿Podríamos utilizar el método `update` para realizar la unificación de dos estructuras así representadas? Probémoslo.


```python
fs_sn_dict = dict()
fs_sn_dict.update(fs_b_dict)
fs_sn_dict
```




    {'LEX': 'virus', 'CAT': 'N', 'CONC': {'NUM': None, 'GEN': 'masc'}}




```python
fs_sn_dict.update(fs_c_dict)
fs_sn_dict
```




    {'LEX': 'mutantes', 'CAT': 'ADJ', 'CONC': {'NUM': 'pl', 'GEN': 'masc'}}



El atributo _CONC_, cuyo atributo _NUM_ se encontraba subespecificado, fue reemplazado con la información más específica de la estructura del ítem léxico "mutantes". Sin embargo, este procedimiento también sobreescribió el valor del atributo _CAT_ y le asignó una categoría que no es la quisiéamos ver en ese rasgo.

Probemos qué sucede si lo hacemos de manera inversa, primero actualizando con los valores del adjetivo y, luego, del nombre:


```python
fs_sn_dict = dict()
fs_sn_dict.update(fs_c_dict)
fs_sn_dict
```




    {'LEX': 'mutantes', 'CAT': 'ADJ', 'CONC': {'NUM': 'pl', 'GEN': 'masc'}}




```python
fs_sn_dict.update(fs_b_dict)
fs_sn_dict
```




    {'LEX': 'virus', 'CAT': 'N', 'CONC': {'NUM': None, 'GEN': 'masc'}}



Ahora tenemos una categoría que se aproxima un poco más a lo que quisiéramos (sin ser el "SN" que nos gustaría), pero tenemos subespecificado el atributo _NUM_.

El método `update`, como recurso para unificar estrucuras, solo resulta útil (y no conlleva efectos indeseados) cuando las estructuras involucradas contienen exactamente los mismo valores asignados a los mismos atributos o tienen rasgos no compartidos (_i.e._ alguna o ambas tiene un par <atributo, valor> que la otra no). Recordemos las estructuras indicadas por Blevins(2011) para "él" (D, izq.) y "canta" (D, der.).

<div style="text-align:center">
    <figure>
        <img src="./images/fs_D.png" width="60%">
    </figure>
</div>

```python
fs_d_1 = {
    'PER':3,
    'NUM':'sg',
    'GEN':'masc',
    'CASO':'nom'
}
fs_d_2 = {
    'PER':3,
    'NUM':'sg',
    'CASO':'nom'
}
```


```python
fs_d_1
```




    {'PER': 3, 'NUM': 'sg', 'GEN': 'masc', 'CASO': 'nom'}




```python
fs_d_2
```




    {'PER': 3, 'NUM': 'sg', 'CASO': 'nom'}




```python
fs_d = fs_d_1
fs_d.update(fs_d_2)
fs_d
```




    {'PER': 3, 'NUM': 'sg', 'GEN': 'masc', 'CASO': 'nom'}



¿Y cómo podríamos evaluar la subsunción?

La comparación de objetos por igualdad nos permite ver si ambos objetos son exatamente iguales, pero no si uno de ellos se encuentra contenido en el otro.


```python
fs_d_1 == fs_d_1
```




    True




```python
fs_d_2 == fs_d
```




    False



Probemos la siguiente función, implementada para comparar la subsunción entre dos estructuras representadas en un diccionario:


```python
def subsumes_dict(general, specific):
    # atributos de esturctura general
    general_attr = set(general.keys())
    # atributos de esturctura específica
    specific_attr = set(specific.keys())
    # chequea que los atributos de e.general son un subconjunto de e.específica
    if general_attr.issubset(specific_attr):
        # chequea que los valores de esos atributos sean iguales
        # matches es una lista con valores booleanos
        # True si los valores son iguales y False si son distintos
        matches = [general[attr] == specific[attr] for attr in general_attr]
        # all() devuelve True si todos los booleanos de la lista son True
        # y False si alguno es False
        return all(matches)
    # si no, devuelve Falso
    # (hay atributos en e.general que no están en e.específica
    else:
        return False
```


```python
subsumes_dict(fs_d, fs_d_2)
```




    False




```python
subsumes_dict(fs_d_2, fs_d)
```




    True



Ventatas de la representación con diccionarios:

- brinda un método sencillo para realizar el proceso de unificación para estructuras atómicas

Limitaciones de la representación con diccionarios:

- no brinda un método sencillo para realizar el proceso de unificación para estructuras complejas
- no brinda un método sencillo para realizar el proceso de subsumción

### Representación como conjuntos

Del mismo modo que intentamos representar estructuras de rasgos mediantes diccionarios, lo intentaremos ahora usando conjuntos y tuplas. Veremos qué posibilidades nos ofrecen sus métodos y cuáles son sus limitaciones.

Los conjuntos (`set`) son colecciones no ordenadas de elementos y las tuplas (`tuple`), secuencias ordenadas. Ambos pueden tener tantos elementos como queramos. Nosotros usaremos ambos y propondremos una representación de las estructuras como conjuntos de rasgos (tuplas) <atributo, valor>.


```python
fs_a_set = {
    ('SIGNIFICANTE','torta'),
    ('LEXEMA','torta'),
    ('CAT','N'),
    ('GEN','fem'),
    ('PLU',False)
}
fs_a_set
```




    {('CAT', 'N'),
     ('GEN', 'fem'),
     ('LEXEMA', 'torta'),
     ('PLU', False),
     ('SIGNIFICANTE', 'torta')}



Notemos que la estructura así generada no respeta, cuando se la imprime, el mismo orden que aquel con el que fue generada. Esto es justamente porque los conjuntos no están ordenados, a pesar de que los elementos que contengan sí lo estén.

Otra forma de armar esto mismo puede ser primero armar un conjunto vacío y luego ir añadiendo las tuplas con el método `add`.


```python
fs_b_set = set()
fs_b_set
```




    set()




```python
lex_feature = ('LEX', 'virus')
lex_feature
```




    ('LEX', 'virus')




```python
fs_b_set.add(lex_feature)
fs_b_set
```




    {('LEX', 'virus')}



Para generar un estructura compleja, deberemos usar el tipo de objeto `frozenset` para la estructura interna (contenida) en lugar de `set`. Esto se debe a que los conjuntos del tipo `set`, en Python, no pueden contener objetos mutables. Dado que los objetos `set` son mutables (podemos agregarles y sacarles elementos), no se pueden contener a sí mismos.


```python
cat_feature = ('CAT', 'N')
conc_feature = frozenset([
    ('NUM',None),
    ('GEN','masc')
])

for feature in [cat_feature, conc_feature]:
    fs_b_set.add(feature)
    
fs_b_set
```




    {('CAT', 'N'), ('LEX', 'virus'), frozenset({('GEN', 'masc'), ('NUM', None)})}




```python
fs_c_set = {
    ('CAT', 'ADJ'),
    ('LEX', 'mutantes'),
    frozenset([
        ('GEN', 'masc'),
        ('NUM', 'PLU')
    ])
}
fs_c_set
```




    {('CAT', 'ADJ'),
     ('LEX', 'mutantes'),
     frozenset({('GEN', 'masc'), ('NUM', 'PLU')})}



Probemos cómo funcionaría la unificación. Para esto, podemos utilizar el método `union`, propio de los conjuntos en Python. Este método toma dos conjuntos y devuelve, como resultado, el conjunto formado con los elementos de ambos.


```python
fs_b_set.union(fs_c_set)
```




    {('CAT', 'ADJ'),
     ('CAT', 'N'),
     ('LEX', 'mutantes'),
     ('LEX', 'virus'),
     frozenset({('GEN', 'masc'), ('NUM', 'PLU')}),
     frozenset({('GEN', 'masc'), ('NUM', None)})}



¿Y si las estructuras son de tipo atómicas?


```python
fs_d_1_set = {
    ('PER',3),
    ('NUM','sg'),
    ('GEN','masc'),
    ('CASO','nom')
}
fs_d_2_set = {
    ('PER',3),
    ('NUM','sg'),
    ('CASO','nom')
}

fs_d_1_set.union(fs_d_1_set)
```




    {('CASO', 'nom'), ('GEN', 'masc'), ('NUM', 'sg'), ('PER', 3)}



En el caso de subsunción, podemos recurrir al método de `issubset`.


```python
fs_d_2_set.issubset(fs_d_1_set)
```




    True




```python
fs_d_1_set.issubset(fs_d_2_set)
```




    False



Ventajas por sobre la representación con diccionarios:
- brinda un método sencillo para realizar el proceso de subsumción

Limitaciones de la representación con conjuntos:

- no brinda un método sencillo para realizar el proceso de unificación para estructuras complejas

### Representación con NLTK

La librería `nltk`, por su parte, nos provee con el objeto `FeatStruct`, pensado especialmente para representar estructuras de rasgos. Veamos cómo funciona.


```python
fs_a_nltk = nltk.FeatStruct(SIGNIFICANTE='torta',LEXEMA='torta',CAT='N',GEN='fem',PLU=False)
fs_a_nltk
```




    [CAT='N', GEN='fem', LEXEMA='torta', -PLU, SIGNIFICANTE='torta']



Si queremos indicar que un atributo no tiene un valor específico, debemos usar la indicación `?n`.


```python
fs_b_nltk = nltk.FeatStruct('''[LEX=virus, CAT=N, CONC=[NUM=?n, GEN=masc]]''')
fs_b_nltk
```




    [CAT='N', CONC=[GEN='masc', NUM=?n], LEX='virus']




```python
fs_c_nltk = nltk.FeatStruct(LEX='mutantes',CAT='ADJ')
fs_c_nltk
```




    [CAT='ADJ', LEX='mutantes']




```python
fs_c_conc = nltk.FeatStruct(NUM='plu',GEN='masc')
fs_c_nltk['CONC'] = fs_c_conc
fs_c_nltk
```




    [CAT='ADJ', CONC=[GEN='masc', NUM='plu'], LEX='mutantes']



Para la unificación, NLTK nos provee el método `unify`.


```python
fs_b_nltk.unify(fs_c_nltk, trace=1)
```

    
    Unification trace:
       / [CAT='N', CONC=[GEN='masc', NUM=?n], LEX='virus']
      |\ [CAT='ADJ', CONC=[GEN='masc', NUM='plu'], LEX='mutantes']
      |
      | Unify feature: CAT
      |    / 'N'
      |   |\ 'ADJ'
      |   |
      X   X <-- FAIL



```python
fs_d_1_nltk = nltk.FeatStruct(GEN='masc', NUM='sg', PER=3, CASO='nom')
fs_d_2_nltk = nltk.FeatStruct(GEN='masc', NUM='sg', PER=3)

fs_d_1_nltk.unify(fs_d_2_nltk, trace=1)
```

    
    Unification trace:
       / [CASO='nom', GEN='masc', NUM='sg', PER=3]
      |\ [GEN='masc', NUM='sg', PER=3]
      |
      | Unify feature: GEN
      |    / 'masc'
      |   |\ 'masc'
      |   |
      |   +-->'masc'
      |
      | Unify feature: NUM
      |    / 'sg'
      |   |\ 'sg'
      |   |
      |   +-->'sg'
      |
      | Unify feature: PER
      |    / 3
      |   |\ 3
      |   |
      |   +-->3
      |
      +-->[CASO='nom', GEN='masc', NUM='sg', PER=3]





    [CASO='nom', GEN='masc', NUM='sg', PER=3]



Solo para ver qué sucede con los rasgos no especificados, volvamos a probar el caso fallido de "virus mutantes" pero eliminando los atributos `CAT` y `LEX`.


```python
_ = fs_b_nltk.pop('CAT')
_ = fs_b_nltk.pop('LEX')
fs_b_nltk
```




    [CONC=[GEN='masc', NUM=?n]]




```python
_ = fs_c_nltk.pop('CAT')
_ = fs_c_nltk.pop('LEX')
fs_c_nltk
```




    [CONC=[GEN='masc', NUM='plu']]




```python
binding = dict()
fs_b_nltk.unify(fs_c_nltk, bindings=binding, trace=1)
```

    
    Unification trace:
       / [CONC=[GEN='masc', NUM=?n]]
      |\ [CONC=[GEN='masc', NUM='plu']]
      |
      | Unify feature: CONC
      |    / [GEN='masc', NUM=?n]
      |   |\ [GEN='masc', NUM='plu']
      |   |
      |   | Unify feature: CONC.GEN
      |   |    / 'masc'
      |   |   |\ 'masc'
      |   |   |
      |   |   +-->'masc'
      |   |
      |   | Unify feature: CONC.NUM
      |   |    / ?n
      |   |   |\ 'plu'
      |   |   |
      |   |   +-->Variable('?n')
      |   |
      |   +-->[GEN='masc', NUM=?n]
      |       Bindings: {?n: 'plu'}
      |
      +-->[CONC=[GEN='masc', NUM='plu']]
          Bindings: {?n: 'plu'}





    [CONC=[GEN='masc', NUM='plu']]



También podemos usar el método `subsumes` para evaluar la subsunción de estructuras.


```python
fs_d_1_nltk.subsumes(fs_d_2_nltk)
```




    False




```python
fs_d_2_nltk.subsumes(fs_d_1_nltk)
```




    True



### Representación con un grafo


```python
#! pip install networkx
```


```python
from utilities import make_graph
```


```python
make_graph(fs_b_nltk)
```


    
![png](fcfg_files/fcfg_78_0.png)
    


## Estructuras de rasgos como funciones

> _These simple structures provide the base for a general subsumption relation “$\sqsubseteq$” which imposes a partial informativeness order on arbitrary feature structures S and T. In Shieber’s formulation, feature structures are treated as partial functions from features to values, so that the expression “S(f)” denotes the value that a structure S assigns to a feature f. Similarly,
dom(S) denotes the domain of features to which a structure S assigns a value. The expression “S(p)” denotes the value assigned a sequence or path of attributes. Applying a feature structure S to a path (fg) provides a convenient reference to the value obtained by applying S successively to f and g._
(Blevins, 2011: 306)

El objeto `FeatStruct` que nos ofrece NLTK nos permite obtener el valor asociado a un atributo fácilmente.


```python
fs_b_nltk['CAT']
```


    ---------------------------------------------------------------------------

    KeyError                                  Traceback (most recent call last)

    Input In [48], in <cell line: 1>()
    ----> 1 fs_b_nltk['CAT']


    File ~/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages/nltk/featstruct.py:646, in FeatDict.__getitem__(self, name_or_path)
        643 """If the feature with the given name or path exists, return
        644 its value; otherwise, raise ``KeyError``."""
        645 if isinstance(name_or_path, (str, Feature)):
    --> 646     return dict.__getitem__(self, name_or_path)
        647 elif isinstance(name_or_path, tuple):
        648     try:


    KeyError: 'CAT'



```python
fs_b_nltk['CONC']
```




    [GEN='masc', NUM=?n]




```python
fs_b_nltk['CONC']['GEN']
```




    'masc'



Sin embargo, esto no es una función. Veamos cómo sería implementar una función.


```python
def fstruct(attribute):
    value = fs_b_nltk[attribute]
    return value
```


```python
fstruct('CAT')
```


    ---------------------------------------------------------------------------

    KeyError                                  Traceback (most recent call last)

    Input In [52], in <cell line: 1>()
    ----> 1 fstruct('CAT')


    Input In [51], in fstruct(attribute)
          1 def fstruct(attribute):
    ----> 2     value = fs_b_nltk[attribute]
          3     return value


    File ~/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages/nltk/featstruct.py:646, in FeatDict.__getitem__(self, name_or_path)
        643 """If the feature with the given name or path exists, return
        644 its value; otherwise, raise ``KeyError``."""
        645 if isinstance(name_or_path, (str, Feature)):
    --> 646     return dict.__getitem__(self, name_or_path)
        647 elif isinstance(name_or_path, tuple):
        648     try:


    KeyError: 'CAT'



```python
fstruct('CONC')
```




    [GEN='masc', NUM=?n]



Esta función, no obstante, no nos permite "caminar" por la estructura. Solo toma un único atributo y devuelve su valor.


```python
fstruct('CONC','GEN')
```


    ---------------------------------------------------------------------------

    TypeError                                 Traceback (most recent call last)

    Input In [54], in <cell line: 1>()
    ----> 1 fstruct('CONC','GEN')


    TypeError: fstruct() takes 1 positional argument but 2 were given


Usando un asterisco (\*) antes del parámetro `attribute` podemos indicarle a la función que reciba más de un atributo. Todos los atributos que le indiquemos, serán guardados en una tupla sobre la que podremos iterar.


```python
def example(*args):
    return args
```


```python
example('a','b','c')
```




    ('a', 'b', 'c')




```python
def fstruct(*attributes):
    value = fs_b_nltk
    for attr in attributes:
        value = value[attr]
    return value
```


```python
fstruct('CONC')
```




    [GEN='masc', NUM=?n]




```python
fstruct('CONC','GEN')
```




    'masc'



En las funciones anteriores, la estructura de rasgos se encontraba _hardcodeada_. Es decir que estaba especificada dentro de la función y el usuario no tiene posibilidad de modificarla si, por ejemplo, quisiera conocer el valor de un atributo en otra estructura. Para evitar esto, es posible convertirla en un parámetro más.


```python
def fstruct(fs, *attributes):
    value = fs
    for attr in attributes:
        value = value[attr]
    return value
```


```python
fstruct(fs_b_nltk, 'CONC', 'NUM')
```




    Variable('?n')




```python
fstruct(fs_c_nltk, 'CONC', 'NUM')
```




    'plu'



## Feature Sharing

Podemos aprovechar y ver el paralelismo con la noción de herencia, que tiene una procedencia computacional.


```python
fs.cyclic()
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [63], in <cell line: 1>()
    ----> 1 fs.cyclic()


    NameError: name 'fs' is not defined



```python
# retomar feature sharing
# recordar que compartir rasgos no es lo mismo que tener duplicada la misma información
# mostrar con equal_values y hablar sobre reentrancia
```


```python
fs.equal_values(fs, check_reentrance=False)
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [65], in <cell line: 1>()
    ----> 1 fs.equal_values(fs, check_reentrance=False)


    NameError: name 'fs' is not defined


Acá también podríamos mostrar que una estructura que comparte rasgos es más específica que una que no los comparte aunque tiene esepecificados los mismos valores para los mismos atributos.

## Construcción de una gramática


```python
nltk.data.show_cfg('gramaticas/GramaticaDeRasgos.fcfg')
```

    % start S
    #Adaptado al español de la gramática elaborada por Klein para el libro de NLTK
    #
    # ###################
    # Reglas de la Gramática
    # ###################
    # Reescritura de la Raíz
    S -> NP[NUM=?n] VP[NUM=?n]
    # Reescritura de NP
    NP[NUM=?n] -> PropN[NUM=?n] 
    NP[NUM=?n,GEN=?g] -> Det[NUM=?n,GEN=?g] N[NUM=?n,GEN=?g]
    # Reescritura de VP
    VP[TENSE=?t, NUM=?n] -> V[TENSE=?t, NUM=?n]
    # ###################
    # Lexical Productions
    # ###################
    Det[NUM=sg,GEN=masc] -> 'este' | 'el'
    Det[NUM=pl,GEN=masc] -> 'estos' | 'los'
    Det[NUM=sg,GEN=fem] -> 'esta' | 'la'
    Det[NUM=pl,GEN=fem] -> 'estas' | 'las'
    PropN[NUM=sg]-> 'Cata' | 'Julia' | 'Fede' | 'Fer' | 'Martín' | 'Maca' | 'Vicky' | 'Pablo'
    N[NUM=sg,GEN=fem] -> 'chica' | 'mujer' | 'persona' | 'criatura'
    N[NUM=sg,GEN=masc] -> 'chico' | 'hombre' | 'sujeto' 
    N[NUM=pl,GEN=fem] -> 'chicas' | 'mujeres' | 'personas' | 'criaturas'
    N[NUM=pl,GEN=masc] -> 'chicos' | 'hombres' | 'sujetos' 
    V[TENSE=pres,NUM=sg] -> 'desaparece' | 'camina' | 'muerde' | 'llora' | 'aparece' | 'viene' | 'estornudan'
    V[TENSE=pres,NUM=pl] -> 'desaparecen' | 'caminan' | 'lloran' | 'muerden' | 'aparecen' | 'vienen' | 'estornudan'
    V[TENSE=pas,NUM=sg] -> 'desapareció' | 'caminó' | 'mordió' | 'lloraba' | 'apareció' | 'vino' | 'estornudó'
    V[TENSE=pas,NUM=pl] -> 'desaparecieron' | 'caminaron' | 'mordieron' | 'lloraban' | 'aparecieron' | 'vinieron' | 'estornudaron'



```python
sentence = 'las chicas caminan'
sentence
```




    'las chicas caminan'




```python
tokens = sentence.split()
tokens
```




    ['las', 'chicas', 'caminan']




```python
print(type(sentence))
```

    <class 'str'>



```python
print(type(tokens))
```

    <class 'list'>



```python
from nltk import load_parser

cp = load_parser('gramaticas/GramaticaDeRasgos.fcfg', trace=2)

for tree in cp.parse(tokens):
    print(tree)
```

    |.las .chic.cami.|
    Leaf Init Rule:
    |[----]    .    .| [0:1] 'las'
    |.    [----]    .| [1:2] 'chicas'
    |.    .    [----]| [2:3] 'caminan'
    Feature Bottom Up Predict Combine Rule:
    |[----]    .    .| [0:1] Det[GEN='fem', NUM='pl'] -> 'las' *
    Feature Bottom Up Predict Combine Rule:
    |[---->    .    .| [0:1] NP[GEN=?g, NUM=?n] -> Det[GEN=?g, NUM=?n] * N[GEN=?g, NUM=?n] {?g: 'fem', ?n: 'pl'}
    Feature Bottom Up Predict Combine Rule:
    |.    [----]    .| [1:2] N[GEN='fem', NUM='pl'] -> 'chicas' *
    Feature Single Edge Fundamental Rule:
    |[---------]    .| [0:2] NP[GEN='fem', NUM='pl'] -> Det[GEN='fem', NUM='pl'] N[GEN='fem', NUM='pl'] *
    Feature Bottom Up Predict Combine Rule:
    |[--------->    .| [0:2] S[] -> NP[NUM=?n] * VP[NUM=?n] {?n: 'pl'}
    Feature Bottom Up Predict Combine Rule:
    |.    .    [----]| [2:3] V[NUM='pl', TENSE='pres'] -> 'caminan' *
    Feature Bottom Up Predict Combine Rule:
    |.    .    [----]| [2:3] VP[NUM='pl', TENSE='pres'] -> V[NUM='pl', TENSE='pres'] *
    Feature Single Edge Fundamental Rule:
    |[==============]| [0:3] S[] -> NP[NUM='pl'] VP[NUM='pl'] *
    (S[]
      (NP[GEN='fem', NUM='pl']
        (Det[GEN='fem', NUM='pl'] las)
        (N[GEN='fem', NUM='pl'] chicas))
      (VP[NUM='pl', TENSE='pres'] (V[NUM='pl', TENSE='pres'] caminan)))



```python
sentence = 'los chicas caminan'
tokens = sentence.split()

for tree in cp.parse(tokens):
    print(tree)
    print('*'*50)
```

    |.los .chic.cami.|
    Leaf Init Rule:
    |[----]    .    .| [0:1] 'los'
    |.    [----]    .| [1:2] 'chicas'
    |.    .    [----]| [2:3] 'caminan'
    Feature Bottom Up Predict Combine Rule:
    |[----]    .    .| [0:1] Det[GEN='masc', NUM='pl'] -> 'los' *
    Feature Bottom Up Predict Combine Rule:
    |[---->    .    .| [0:1] NP[GEN=?g, NUM=?n] -> Det[GEN=?g, NUM=?n] * N[GEN=?g, NUM=?n] {?g: 'masc', ?n: 'pl'}
    Feature Bottom Up Predict Combine Rule:
    |.    [----]    .| [1:2] N[GEN='fem', NUM='pl'] -> 'chicas' *
    Feature Bottom Up Predict Combine Rule:
    |.    .    [----]| [2:3] V[NUM='pl', TENSE='pres'] -> 'caminan' *
    Feature Bottom Up Predict Combine Rule:
    |.    .    [----]| [2:3] VP[NUM='pl', TENSE='pres'] -> V[NUM='pl', TENSE='pres'] *


## Gramáticas con Slash


```python
nltk.data.show_cfg('gramaticas/GramaticaSlash.fcfg')
```

    % start S
    # Gramática para ilustrar rasgo SUBCAT y la categoría SLASH
    #
    # ###################
    # Reglas de la Gramática
    # ###################
    # Reescritura de la Raíz
    S -> NP[NUM=?n] VP[NUM=?n]
    S -> Wh[NUM=?n] VP/Wh[NUM=?n]
    # Reescritura de NP
    NP[NUM=?n] -> PropN[NUM=?n] 
    NP[NUM=?n,GEN=?g] -> Det[NUM=?n,GEN=?g] N[NUM=?n,GEN=?g]
    # Reescritura de VP
    VP[NUM=?n] -> V[SUBCAT='intrans', TENSE=?t, NUM=?n]
    VP[NUM=?n] -> V[SUBCAT='decir', TENSE=?t, NUM=?n] CP
    VP/?x[NUM=?n] -> V[SUBCAT='decir', TENSE=?t, NUM=?m] NP[NUM=?m] CP/?x[NUM=?n]
    # Reescritura de CP
    CP -> C IP
    CP/?x[NUM=?n] -> C IP/?x[NUM=?n]
    # Reescritura de C
    C -> 'que'
    # Reescritura de IP
    IP -> NP[NUM=?n] VP[NUM=?n]
    IP/?x[NUM=?n] -> N/?x[NUM=?n] VP[NUM=?n]
    # ###################
    # Lexical Productions
    # ###################
    # Reescritura de determinativos
    Det[NUM=sg,GEN=masc] -> 'este' | 'el'
    Det[NUM=pl,GEN=masc] -> 'estos' | 'los'
    Det[NUM=sg,GEN=fem] -> 'esta' | 'la'
    Det[NUM=pl,GEN=fem] -> 'estas' | 'las'
    # Reescritura de Nombres propios
    PropN[NUM=sg]-> 'Cata' | 'Julia' | 'Fede' | 'Fer' | 'Martín' | 'Maca' | 'Vicky' | 'Pablo'
    # Reescritura de N
    N[NUM=sg,GEN=fem] -> 'chica' | 'mujer' | 'persona' | 'criatura'
    N[NUM=sg,GEN=masc] -> 'chico' | 'hombre' | 'sujeto' 
    N[NUM=pl,GEN=fem] -> 'chicas' | 'mujeres' | 'personas' | 'criaturas'
    N[NUM=pl,GEN=masc] -> 'chicos' | 'hombres' | 'sujetos' 
    # Reescritura de N vacío
    N/Wh[NUM=sg] -> 
    N/Wh[NUM=pl] ->
    # Reescritura Wh
    Wh[NUM=sg] -> 'quién'
    Wh[NUM=pl] -> 'quiénes'
    # Reescritura de V
    # Verbos intransitivos
    V[SUBCAT='intrans', TENSE=pres,NUM=sg] -> 'desaparece' | 'camina' | 'muerde' | 'llora' | 'aparece' | 'viene' | 'estornuda'
    V[SUBCAT='intrans', TENSE=pres,NUM=pl] -> 'desaparecen' | 'caminan' | 'lloran' | 'muerden' | 'aparecen' | 'vienen' | 'estornudan'
    V[SUBCAT='intrans', TENSE=pas,NUM=sg] -> 'desapareció' | 'caminó' | 'mordió' | 'lloraba' | 'apareció' | 'vino' | 'estornudó'
    V[SUBCAT='intrans', TENSE=pas,NUM=pl] -> 'desaparecieron' | 'caminaron' | 'mordieron' | 'lloraban' | 'aparecieron' | 'vinieron' | 'estornudaron'
    # Verbos de decir
    V[SUBCAT='decir', TENSE=pres,NUM=sg] -> 'dice' | 'afirma' | 'defiende' | 'argumenta' | 'sostiene' 
    V[SUBCAT='decir', TENSE=pas,NUM=sg] -> 'dijo' | 'afirmó' | 'defendió' | 'argumentó' | 'sostuvo' 



```python
sentence_slash_grammar = 'quién dice el chico que estornuda'
sentence = sentence_slash_grammar.split()
from nltk import load_parser
cp = load_parser('gramaticas/GramaticaSlash.fcfg', trace=2)
for tree in cp.parse(sentence):
     print(tree)
```

    |.q.d.e.c.q.e.|
    Leaf Init Rule:
    |[-] . . . . .| [0:1] 'quién'
    |. [-] . . . .| [1:2] 'dice'
    |. . [-] . . .| [2:3] 'el'
    |. . . [-] . .| [3:4] 'chico'
    |. . . . [-] .| [4:5] 'que'
    |. . . . . [-]| [5:6] 'estornuda'
    Feature Empty Predict Rule:
    |# . . . . . .| [0:0] N[]/Wh[NUM='sg'] -> *
    |. # . . . . .| [1:1] N[]/Wh[NUM='sg'] -> *
    |. . # . . . .| [2:2] N[]/Wh[NUM='sg'] -> *
    |. . . # . . .| [3:3] N[]/Wh[NUM='sg'] -> *
    |. . . . # . .| [4:4] N[]/Wh[NUM='sg'] -> *
    |. . . . . # .| [5:5] N[]/Wh[NUM='sg'] -> *
    |. . . . . . #| [6:6] N[]/Wh[NUM='sg'] -> *
    |# . . . . . .| [0:0] N[]/Wh[NUM='pl'] -> *
    |. # . . . . .| [1:1] N[]/Wh[NUM='pl'] -> *
    |. . # . . . .| [2:2] N[]/Wh[NUM='pl'] -> *
    |. . . # . . .| [3:3] N[]/Wh[NUM='pl'] -> *
    |. . . . # . .| [4:4] N[]/Wh[NUM='pl'] -> *
    |. . . . . # .| [5:5] N[]/Wh[NUM='pl'] -> *
    |. . . . . . #| [6:6] N[]/Wh[NUM='pl'] -> *
    Feature Bottom Up Predict Combine Rule:
    |[-] . . . . .| [0:1] Wh[NUM='sg'] -> 'quién' *
    Feature Bottom Up Predict Combine Rule:
    |[-> . . . . .| [0:1] S[] -> Wh[NUM=?n] * VP[]/Wh[NUM=?n] {?n: 'sg'}
    Feature Bottom Up Predict Combine Rule:
    |. [-] . . . .| [1:2] V[NUM='sg', SUBCAT='decir', TENSE='pres'] -> 'dice' *
    Feature Bottom Up Predict Combine Rule:
    |. [-> . . . .| [1:2] VP[NUM=?n] -> V[NUM=?n, SUBCAT='decir', TENSE=?t] * CP[] {?n: 'sg', ?t: 'pres'}
    |. [-> . . . .| [1:2] VP[]/?x[NUM=?n] -> V[NUM=?m, SUBCAT='decir', TENSE=?t] * NP[NUM=?m] CP[]/?x[NUM=?n] {?m: 'sg', ?t: 'pres'}
    Feature Bottom Up Predict Combine Rule:
    |. . [-] . . .| [2:3] Det[GEN='masc', NUM='sg'] -> 'el' *
    Feature Bottom Up Predict Combine Rule:
    |. . [-> . . .| [2:3] NP[GEN=?g, NUM=?n] -> Det[GEN=?g, NUM=?n] * N[GEN=?g, NUM=?n] {?g: 'masc', ?n: 'sg'}
    Feature Bottom Up Predict Combine Rule:
    |. . . [-] . .| [3:4] N[GEN='masc', NUM='sg'] -> 'chico' *
    Feature Single Edge Fundamental Rule:
    |. . [---] . .| [2:4] NP[GEN='masc', NUM='sg'] -> Det[GEN='masc', NUM='sg'] N[GEN='masc', NUM='sg'] *
    Feature Bottom Up Predict Combine Rule:
    |. . [---> . .| [2:4] S[] -> NP[NUM=?n] * VP[NUM=?n] {?n: 'sg'}
    |. . [---> . .| [2:4] IP[] -> NP[NUM=?n] * VP[NUM=?n] {?n: 'sg'}
    Feature Single Edge Fundamental Rule:
    |. [-----> . .| [1:4] VP[]/?x[NUM=?n] -> V[NUM=?m, SUBCAT='decir', TENSE=?t] NP[NUM=?m] * CP[]/?x[NUM=?n] {?m: 'sg', ?t: 'pres'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . [-] .| [4:5] C[] -> 'que' *
    Feature Bottom Up Predict Combine Rule:
    |. . . . [-> .| [4:5] CP[] -> C[] * IP[] {}
    |. . . . [-> .| [4:5] CP[]/?x[NUM=?n] -> C[] * IP[]/?x[NUM=?n] {}
    Feature Bottom Up Predict Combine Rule:
    |. . . . . [-]| [5:6] V[NUM='sg', SUBCAT='intrans', TENSE='pres'] -> 'estornuda' *
    Feature Bottom Up Predict Combine Rule:
    |. . . . . [-]| [5:6] VP[NUM='sg'] -> V[NUM='sg', SUBCAT='intrans', TENSE='pres'] *
    Feature Bottom Up Predict Combine Rule:
    |> . . . . . .| [0:0] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. > . . . . .| [1:1] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . > . . . .| [2:2] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . > . . .| [3:3] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . > . .| [4:4] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . . > .| [5:5] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Single Edge Fundamental Rule:
    |. . . . . [-]| [5:6] IP[]/Wh[NUM='sg'] -> N[]/Wh[NUM='sg'] VP[NUM='sg'] *
    Feature Single Edge Fundamental Rule:
    |. . . . [---]| [4:6] CP[]/Wh[NUM='sg'] -> C[] IP[]/Wh[NUM='sg'] *
    Feature Single Edge Fundamental Rule:
    |. [---------]| [1:6] VP[]/Wh[NUM='sg'] -> V[NUM='sg', SUBCAT='decir', TENSE='pres'] NP[NUM='sg'] CP[]/Wh[NUM='sg'] *
    Feature Single Edge Fundamental Rule:
    |[===========]| [0:6] S[] -> Wh[NUM='sg'] VP[]/Wh[NUM='sg'] *
    Feature Bottom Up Predict Combine Rule:
    |. . . . . . >| [6:6] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'sg', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |> . . . . . .| [0:0] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. > . . . . .| [1:1] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . > . . . .| [2:2] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . > . . .| [3:3] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . > . .| [4:4] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . . > .| [5:5] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    Feature Bottom Up Predict Combine Rule:
    |. . . . . . >| [6:6] IP[]/?x[NUM=?n] -> N[]/?x[NUM=?n] * VP[NUM=?n] {?n: 'pl', ?x: 'Wh'}
    (S[]
      (Wh[NUM='sg'] quién)
      (VP[]/Wh[NUM='sg']
        (V[NUM='sg', SUBCAT='decir', TENSE='pres'] dice)
        (NP[GEN='masc', NUM='sg']
          (Det[GEN='masc', NUM='sg'] el)
          (N[GEN='masc', NUM='sg'] chico))
        (CP[]/Wh[NUM='sg']
          (C[] que)
          (IP[]/Wh[NUM='sg']
            (N[]/Wh[NUM='sg'] )
            (VP[NUM='sg']
              (V[NUM='sg', SUBCAT='intrans', TENSE='pres'] estornuda))))))


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
nltk.data.show_cfg('gramaticas/pruebasemantica.fcfg')
```

    % start S
    # Grammar Rules
    S[SEM = <?subj(?vp)>] -> NP[NUM=?n,GEN=?g,SEM=?subj] VP[NUM=?n,GEN=?g,SEM=?vp]
    NP[NUM=?n,GEN=?g,SEM=<?det(?nom)>] -> Det[NUM=?n,GEN=?g,SEM=?det]  Nom[NUM=?n,GEN=?g,SEM=?nom]
    NP[LOC=?l,NUM=?n,GEN=?g,SEM=?np] -> PropN[LOC=?l,NUM=?n,GEN=?g,SEM=?np]
    Nom[NUM=?n,GEN=?g,SEM=?nom] -> N[NUM=?n,GEN=?g,SEM=?nom]
    VP[NUM=?n,SEM=?v] -> IV[NUM=?n,SEM=?v]
    VP[NUM=?n,SEM=<?v(?obj)>] -> TV[NUM=?n,SEM=?v] NP[SEM=?obj]
    VP[NUM=?n,SEM=<?v(?obj,?pp)>] -> DTV[NUM=?n,SEM=?v] NP[SEM=?obj] PP[+A,SEM=?pp]
    VP[NUM=?n,GEN=?g,SEM=<?v(?a)>] -> VCOP[NUM=?n] A[NUM=?n,GEN=?g,SEM=?a]
    PP[+A, SEM=?np] -> P[+A] NP[SEM=?np]
    # Lexical Rules
    PropN[-LOC,NUM=sg,GEN=masc,SEM=<\P.P(martin)>] -> 'Martín'
    PropN[-LOC,NUM=sg,GEN=fem,SEM=<\P.P(cata)>] -> 'Cata'
    PropN[-LOC,NUM=sg,GEN=masc,SEM=<\P.P(fede)>] -> 'Fede'
    PropN[-LOC,NUM=sg,GEN=masc,SEM=<\P.P(pablo)>] -> 'Pablo'
    PropN[-LOC,NUM=sg,GEN=fem,SEM=<\P.P(julia)>] -> 'Julia'
    PropN[-LOC,NUM=sg,GEN=masc,SEM=<\P.P(fer)>] -> 'Fer'
    PropN[-LOC,NUM=sg,GEN=fem,SEM=<\P.P(maca)>] -> 'Maca'
    Det[NUM=sg,GEN=masc,SEM=<\P Q.exists x.(unico_ind_relevante_en_contexto(x) & P(x) & Q(x))>] -> 'el'
    Det[NUM=sg,GEN=fem,SEM=<\P Q.exists x.(unico_ind_relevante_en_contexto(x) & P(x) & Q(x))>] -> 'la'
    Det[NUM=sg,GEN=masc,SEM=<\P Q.exists x.(unico_ind_pl_relevante_en_contexto(x) & P(x) & Q(x))>] -> 'los'
    Det[NUM=sg,GEN=fem,SEM=<\P Q.exists x.(unico_ind_pl_relevante_en_contexto(x) & P(x) & Q(x))>] -> 'las'
    Det[NUM=sg,GEN=masc,SEM=<\P Q.exists x.(P(x) & Q(x))>] -> 'algún'
    Det[NUM=pl,GEN=masc,SEM=<\P Q.exists x.(ind_pl(x) & P(x) & Q(x))>] -> 'algunos'
    Det[NUM=sg,GEN=fem,SEM=<\P Q.exists x.(P(x) & Q(x))>] -> 'alguna'
    Det[NUM=pl,GEN=fem,SEM=<\P Q.exists x.(ind_pl(x) & P(x) & Q(x))>] -> 'algunas'
    Det[NUM=sg,GEN=masc,SEM=<\P Q.exists x.(P(x) & Q(x))>] -> 'un'
    Det[NUM=sg,GEN=fem,SEM=<\P Q.exists x.(P(x) & Q(x))>] -> 'una'
    Det[NUM=pl,GEN=masc,SEM=<\P Q.exists x.(ind_pl(x) & P(x) & Q(x))>] -> 'unos'
    Det[NUM=pl,GEN=fem,SEM=<\P Q.exists x.(ind_pl(x) & P(x) & Q(x))>] -> 'unas'
    N[NUM=sg,GEN=masc,SEM=<\x.globo(x)>] -> 'globo'
    N[NUM=pl,GEN=masc,SEM=<\x.globo(x)>] -> 'globos'
    N[NUM=sg,GEN=fem,SEM=<\x.chica(x)>] -> 'chica'
    N[NUM=sg,GEN=masc,SEM=<\x.chico(x)>] -> 'chico'
    N[NUM=sg,GEN=masc,SEM=<\x.hombre(x)>] -> 'hombre'
    N[NUM=pl,GEN=masc,SEM=<\x.hombre(x)>] -> 'hombres'
    N[NUM=sg,GEN=fem,SEM=<\x.mujer(x)>] -> 'mujer'
    N[NUM=pl,GEN=fem,SEM=<\x.mujer(x)>] -> 'mujeres'
    N[NUM=sg,GEN=masc,SEM=<\x.perro(x)>] -> 'perro'
    N[NUM=pl,GEN=masc,SEM=<\x.perro(x)>] -> 'perros'
    N[NUM=sg,GEN=masc,SEM=<\x.gato(x)>] -> 'gato'
    N[NUM=pl,GEN=masc,SEM=<\x.gato(x)>] -> 'gatos'
    N[NUM=sg,GEN=masc,SEM=<\x.regalo(x)>] -> 'regalo'
    N[NUM=pl,GEN=masc,SEM=<\x.regalo(x)>] -> 'regalos'
    N[NUM=sg,GEN=masc,SEM=<\x.paquete(x)>] -> 'paquete'
    N[NUM=pl,GEN=masc,SEM=<\x.paquete(x)>] -> 'paquetes'
    N[NUM=sg,GEN=masc,SEM=<\x.tabaco(x)>] -> 'tabaco'
    N[NUM=pl,GEN=masc,SEM=<\x.cigarrillo(x)>] -> 'cigarrillos'
    N[NUM=sg,GEN=masc,SEM=<\x.cigarrillo(x)>] -> 'cigarrillo'
    N[NUM=sg,GEN=masc,SEM=<\x.libro(x)>] -> 'libro'
    N[NUM=pl,GEN=masc,SEM=<\x.libro(x)>] -> 'libros'
    VCOP[NUM=sg] -> 'es'|'era'|'fue'
    VCOP[NUM=pl] -> 'son'|'eran'|'fueron'
    VCOP[NUM=sg] -> 'está'|'estaba'|'estuvo'
    VCOP[NUM=pl] -> 'están'|'estaban'|'estuvieron'
    IV[NUM=sg,SEM=<\x.fumar(x)>,TNS=pres] -> 'fuma'
    IV[NUM=pl,SEM=<\x.fumar(x)>,TNS=pres] -> 'fuman'
    IV[NUM=sg,SEM=<\x.caminar(x)>,TNS=pres] -> 'camina'
    IV[NUM=pl,SEM=<\x.caminar(x)>,TNS=pres] -> 'caminan'
    IV[NUM=sg,SEM=<\x.correr(x)>,TNS=pres] -> 'corre'
    IV[NUM=pl,SEM=<\x.correr(x)>,TNS=pres] -> 'corren'
    IV[NUM=sg,SEM=<\x.correr(x)>,TNS=pas] -> 'corrió'
    IV[NUM=pl,SEM=<\x.correr(x)>,TNS=pas] -> 'corrieron'
    IV[NUM=sg,SEM=<\x.explotar(x)>,TNS=pres] -> 'explota'
    IV[NUM=pl,SEM=<\x.explotar(x)>,TNS=pres] -> 'explotan'
    IV[NUM=sg,SEM=<\x.explotar(x)>,TNS=pas] -> 'explotó'
    IV[NUM=pl,SEM=<\x.explotar(x)>,TNS=pas] -> 'explotaron'
    TV[NUM=sg,SEM=<\X x.X(\y.mirar(x,y))>,TNS=pres] -> 'mira'
    TV[NUM=pl,SEM=<\X x.X(\y.mirar(x,y))>,TNS=pres] -> 'miran'
    TV[NUM=sg,SEM=<\X x.X(\y.romper(x,y))>,TNS=pres] -> 'rompe'
    TV[NUM=pl,SEM=<\X x.X(\y.romper(x,y))>,TNS=pres] -> 'rompen'
    TV[NUM=sg,SEM=<\X x.X(\y.morder(x,y))>,TNS=pres] -> 'muerde'
    TV[NUM=pl,SEM=<\X x.X(\y.morder(x,y))>,TNS=pres] -> 'muerden'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.dar(x,y,z)))>,TNS=pres] -> 'da'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.dar(x,y,z)))>,TNS=pres] -> 'dan'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.dar(x,y,z)))>,TNS=pas] -> 'dio'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.dar(x,y,z)))>,TNS=pas] -> 'dieron'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.entregar(x,y,z)))>,TNS=pres] -> 'entrega'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.entregar(x,y,z)))>,TNS=pres] -> 'entregan'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.entregar(x,y,z)))>,TNS=pas] -> 'entregó'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.entregar(x,y,z)))>,TNS=pas] -> 'entregaron'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.enviar(x,y,z)))>,TNS=pres] -> 'envía'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.enviar(x,y,z)))>,TNS=pres] -> 'envían'
    DTV[NUM=sg,SEM=<\Y X x.X(\z.Y(\y.enviar(x,y,z)))>,TNS=pas] -> 'envió'
    DTV[NUM=pl,SEM=<\Y X x.X(\z.Y(\y.enviar(x,y,z)))>,TNS=pas] -> 'enviaron'
    P[+a] -> 'a'
    A[NUM=sg,GEN=fem,SEM=<\x.ocupado(x)>] -> 'ocupada'
    A[NUM=pl,GEN=fem,SEM=<\x.ocupado(x)>] -> 'ocupadas'
    A[NUM=sg,GEN=masc,SEM=<\x.ocupado(x)>] -> 'ocupado'
    A[NUM=pl,GEN=masc,SEM=<\x.ocupado(x)>] -> 'ocupados'
    A[NUM=sg,GEN=fem,SEM=<\x.cansado(x)>] -> 'cansada'
    A[NUM=pl,GEN=fem,SEM=<\x.cansado(x)>] -> 'cansadas'
    A[NUM=sg,GEN=masc,SEM=<\x.cansado(x)>] -> 'cansado'
    A[NUM=pl,GEN=masc,SEM=<\x.cansado(x)>] -> 'cansados'
    A[NUM=sg,GEN=fem,SEM=<\x.ocupado(x)>] -> 'fumada'
    A[NUM=pl,GEN=fem,SEM=<\x.ocupado(x)>] -> 'fumadas'
    A[NUM=sg,GEN=masc,SEM=<\x.ocupado(x)>] -> 'fumado'
    A[NUM=pl,GEN=masc,SEM=<\x.ocupado(x)>] -> 'fumados'



```python
sents = ['Cata fuma']
grammar = 'gramaticas/pruebasemantica.fcfg'
for results in nltk.interpret_sents(sents, grammar):
    for (synrep, semrep) in results:
             print(synrep)
```

    (S[SEM=<fumar(cata)>]
      (NP[GEN='fem', -LOC, NUM='sg', SEM=<\P.P(cata)>]
        (PropN[GEN='fem', -LOC, NUM='sg', SEM=<\P.P(cata)>] Cata))
      (VP[NUM='sg', SEM=<\x.fumar(x)>]
        (IV[NUM='sg', SEM=<\x.fumar(x)>, TNS='pres'] fuma)))


## Semántica eventiva

Un tipo de semántica formal que hoy ha ganado mucha popularidad es la semántica eventiva. La semántica eventiva concibe el significado de las oraciones como cuantificación sobre eventos: 

- Juan le dio ayer el libro a Pedro en la casa.
- Existe un e tal que Agente(e, Juan) & Tema(e, el libro) & Meta(e, Pedro) & en(e, la casa) & Pasado(e)

La semántica neodavidsoniana permite dispensar de grandes listas de subclases (verbos transitivos, verbos intransitivos, verbos ditransitivos, verbos bivalentes, verbos impersonales, verbos que toman distintos tipos de sintagmas preposicionales) y simplificar, en consecuencia, el léxico. Esto se hace al costo de dar una denotación más compleja a las entradas léxicas (que sin embargo, se puede automatizar si se tienen listas léxicas como las de freeling) y a las reglas de reescritura. Este tipo de enfoques propenden a sobregenerar, pero, naturalmente, este no es un problema si lo que nos interesa es parsear oraciones.


```python
sents = ['Cata dio Ficciones a Chafa']
grammar = 'gramaticas/semantica_eventiva_base.fcfg'
for results in nltk.interpret_sents(sents, grammar):
    for (synrep, semrep) in results:
             print(synrep)
```

    (S[SEM=<(exists e.agente(e,cata) & dar(e) & pasado(e) & tema(e,ficciones) & meta(e,chafa))>]
      (NP[+ANIM, GEN='fem', NUM='sg', SEM=<\P.P(cata)>]
        (PropN[+ANIM, GEN='fem', NUM='sg', SEM=<\P.P(cata)>] Cata))
      (VP[NUM='sg', SEM=<\e.(dar(e) & pasado(e) & tema(e,ficciones) & meta(e,chafa))>]
        (V[NUM='sg', SEM=<\e.(dar(e) & pasado(e))>, TNS='pas'] dio)
        (NP[-ANIM, GEN='masc', NUM='sg', SEM=<\P.P(ficciones)>]
          (PropN[-ANIM, GEN='masc', NUM='sg', SEM=<\P.P(ficciones)>]
            Ficciones))
        (PP[+A, SEM=<\P.P(chafa)>]
          (P[+a] a)
          (NP[+ANIM, GEN='masc', NUM='sg', SEM=<\P.P(chafa)>]
            (PropN[+ANIM, GEN='masc', NUM='sg', SEM=<\P.P(chafa)>] Chafa)))))


## Referencias

- Bird, S., Klein, E., y Loper, E. (2009). _Natural language processing with Python: analyzing text with the natural language toolkit_. “O’Reilly Media, Inc.”. “Chapter 9: Building FeatureBased Grammars”, pp. 327–360.
- Blevins, J. P. (2011). Feature-based grammar. En _NonTransformational Syntax_, pp. 297–324. Wiley Blackwell, Massachusetts.
