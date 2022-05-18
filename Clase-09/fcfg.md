# Gramáticas basadas en rasgos

Para esta clase, puede ser de utilidad la siguiente bibliografía:

- [NLTK Book: Chapter 9, Building Feature Based Grammars](https://www.nltk.org/book/ch09.html)
- [NLTK (documentación de librería): Sample usage for featstruct](https://www.nltk.org/howto/featstruct.html)


```python
#Temas de Python que me parece que vamos a tocar en esta clase:
#
#- Diccionarios
#- Conjuntos
#- Funciones
#- Herencia
#- `*args`
#- Métodos de NLTK
#- Función `lambda`
```


```python
# importa librería nltk
import nltk
```


```python
# comentario
# no sé si al inicio porque me parece medio colgado, pero podríamos hacer esto que vos querías de ir a la carpeta
# donde está ubicada la librería y mostrarles que no es otra cosa que una carpeta con archivos y código en Python
# no sé si conviene hacerlo en esta instancia o más adelante, cuando importemos el FeatStruct, por ejemplo
# PD: voy a ir dejando comentarios así
```

## Estructuras de rasgos


```python
# pongo algunas estructuras para que nos sirvan
# de guía para ir armando con código
# elegí estas porque
# - la de torta nos permite mostrar el rasgo PLU como un bool
# - las otras nos permiten mostrar cómo armar rasgos complejos
# - el ejemplo de la unificación (es el mismo que estaba en clase)
# nos va a permitir pensar el tema de la implementación de la
# unificación (podemos representar la estructura como un diccionario
# pero después no tenemos cómo unificar eso a menos que lo
# implementemos o que ya alguien haya hecho algo similar por
# nosotros, en este caso, les muchaches de NLTK)
```

(A)N = \begin{equation}
\begin{bmatrix}
\text{SIGNIFICANTE torta}\\
\text{LEXEMA torta}\\
\text{CAT N}\\
\text{GEN fem}\\
\text{PLU -}\\
\end{bmatrix}
\end{equation}

(B)\begin{equation}
N = \begin{bmatrix}
\text{LEX virus}\\
\text{CAT N}\\
\text{CONC }\begin{bmatrix}
\text{NUM [ ]}\\
\text{GEN masc}
\end{bmatrix}
\end{bmatrix}
\end{equation}

(C)\begin{equation}
ADJ = \begin{bmatrix}
\text{LEX mutantes}\\
\text{CAT ADJ}\\
\text{CONC }\begin{bmatrix}
\text{NUM pl}\\
\text{GEN masc}
\end{bmatrix}
\end{bmatrix}
\end{equation}

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

(D)\begin{equation}
\begin{bmatrix}
\text{PER 3}\\
\text{NUM sg}\\
\text{GEN masc}\\
\text{CASO nom}
\end{bmatrix}
\sqcup
\begin{bmatrix}
\text{PER 3}\\
\text{NUM sg}\\
\text{CASO nom}
\end{bmatrix}
\end{equation}


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



Limitaciones de la representación con diccionarios:

- no brinda un método sencillo para realizar el proceso de unificación

### Representación como conjuntos

### Representación con NLTK


```python
fs1 = nltk.FeatStruct(TENSE='past', CHICHO=True)
```


```python
fs2 = nltk.FeatStruct(CAT='V')
```


```python
fs3 = nltk.FeatStruct(CAT='V', TENSE='present')
```


```python
fs3 = nltk.FeatStruct("""[CAT=V, TENSE=present]""")
```


```python
# explicar representación de inespecificidad
```

### Representación con un grafo


```python
# armar
```

## Estructuras de rasgos como funciones

Acá podemos mostrar:
- función que toma un atributo y devuelve un valor
- función que toma un path de atributos y devuelve el valor siguiendo ese path

Para esto -> armar una clase que herede de FeatStruct e implementar un método que recorra la estructura. Como argumento de la función usar `*args`.

Podemos aprovechar y ver el paralelismo con la noción de herencia, que tiene una procedencia computacional.

## Unificación


```python
print(fs1.unify(fs3, trace=1))
```

    
    Unification trace:
       / [+CHICHO, TENSE='past']
      |\ [CAT='V', TENSE='present']
      |
      | Unify feature: TENSE
      |    / 'past'
      |   |\ 'present'
      |   |
      X   X <-- FAIL
    None


Acá mostrar:

- unificación exitosa
- unificación fallida
- unificación vs. unión: antes vimos que las estructuras se pueden representar como conjuntos. Este es un buen momento para mostrar que la unificación y la unión funcionan distinto y que la única situación en la que funcionan igual es con rasgos atómicos.

## Subsunción


```python
fs2.subsumes(fs3)
```




    True



## Rasgos atómicos y complejos


```python
fs = nltk.FeatStruct(CAT='V', NUM='plu')
```


```python
fs
```




    [CAT='V', NUM='plu']




```python
fs['CONCORD'] = fs
```


```python
fs
```




    (1)[CAT='V', CONCORD->(1), NUM='plu']




```python
fs.cyclic()
```




    True




```python
# retomar feature sharing
# recordar que compartir rasgos no es lo mismo que tener duplicada la misma información
# mostrar con equal_values y hablar sobre reentrancia
```


```python
fs.equal_values(fs, check_reentrance=False)
```




    True



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
sentence = 'los chicas caminan'
tokens = sentence.split()
print(sentence)
print(type(sentence))
print(tokens)
print(type(tokens))
```

    los chicas caminan
    <class 'str'>
    ['los', 'chicas', 'caminan']
    <class 'list'>



```python
from nltk import load_parser
cp = load_parser('gramaticas/GramaticaDeRasgos.fcfg', trace=2)
for tree in cp.parse(tokens):
     print(tree)
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

