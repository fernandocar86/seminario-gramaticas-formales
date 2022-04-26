# Parsers para Gramáticas independientes de contexto


```python
import nltk
import re
```

## NLTK

Natural Language Toolkit es un conjunto de herramientas para crear programas en python que trabajen con lenguaje natural. nltk.org (la organización a cargo de crear y mantener esta librería) pone a disposición de manera online el [NLTK Book](https://www.nltk.org/book/), su libro especializado en el uso de la librería así como la explicación de conceptos generales de PLN.

Para estar al día con los cambios en el código, lo mejor es chequear este libro on-line en vez de se versión editada, que puede traer ejemplos deprecados ("nbest_parse" vs. "parse").

### Gramáticas para NLTK

Antes de pasar a los parsers, miremos las gramáticas que vamos a "parsear", construidas según lo pide el libro de Bird et al., y que tenemos guardada en la carpeta "gramaticas".

Notemos que la extensión del archivo es ".cfg", así le vamos a avisar a nltk que la gramática debe ser entendida como "Context Free Grammar".
Ahora podemos mirar por adentro que la grámatica cuenta con todos los elementos que, según lo que vimos en la clase, constituyen el formalismo de una CFG:

Un axioma: S

Símbolos no terminales: SN, PRO, NP, etc.

Símbolos terminales: martín, cata, etc.

Reglas de reescritura: cada una de las líneas de la gramática, que deben indicar que un elemento a la izquierda del signo -> se debe reescribir como los elementos a la derecha.

### Tokenización

"Sea cual fuere la extensión del texto considerado, es preciso segmentarlo primero en porciones cada vez más  reducidas hasta los elementos no descomponibles". (Émile Benveniste: Problemas de lingüística general (vol I). Siglo XXI, Madrid. 1991):

Antes de poder hacer cualquier procesamiento sobre una cadena de texto, es necesario poder segmentarla en las unidades que espera como input la herramientas de procesamiento. 

En el caso de los parsers, el análisis de la oración se aplica a la secuencia de palabras, por lo tanto, el input de los parsers será una **lista de palabras**. Claro que esta lista deberá corresponder en orden a la oración que queremos parsear. 

Ejemplo: si queremos parsear la oración "Hola mundo", debemos segmentarla hasta conseguir ["Hola", "mundo"].

Por eso, antes de pasar a los parsers, debemos revisar un concepto: la tokenización, es decir, el proceso de transformar una cadena de caracteres en unidades más pequeñas, en nuestro caso, palabras.

El proceso de tokenización es un proceso complejo que se puede servir de expresiones regulares o modelos de lenguaje y varía de lengua a lengua. Nosotros vamos a usar el método más simple confiando en que el español, por regla general, separa las palabra en la secuencia escrita mediante el uso de espacios en blanco. 

Muy rápidamente podemos ver que este método es simple pero no va a funcionar si usamos puntuación, ya que la lista de palabras resultantes de la secuencia "Yo, Claudio" no diríamos que es ["Yo,", "Claudio"]. 

Para eliminar la puntuación, vamos a usar un método de la librería "re" que nos permitirá substituir la puntuación por el símbolo vacío.

¿Qué otros problemas puede traer esta versión sobre simplificada de un tokenizador?



Pasemos a los parsers:

## Recursive Descent Parser

El primer parser que vamos a ver es el Recursive Descent Parser. Este parser es de tipo **top-down** (analiza de arriba hacia abajo). Es decir que parte del símbolo de inicio y aplica las reglas de la gramática para obtener los constituyentes inmediatos y armar el árbol hasta llegar a los símbolos terminales. 

Debe chequear que los símbolos terminales coincidan con la secuencia del input sin haberla visto de antemano. Si no hay coincidencia, tiene que retroceder y buscar diferentes alternativas de parseo.


```python
# Recursive Descent Parser

def rd_parser(oracion, gramatica):                  # Definimos una función llamada rd_parser con dos argumentos.
    oracion = oracion.lower()                       # Convertimos a minúscula la oración utilizando una función nativa de la cadena de caracteres: lower(). 
        
    if oracion.endswith('.'):                       # Otra función nativa de las strings nos ayuda a chequear si la cadena termina en x argumento.
        oracion = re.sub('\.',' ',oracion)          # En este caso, si la oración termina con un punto, se lo quita utilizando la librería de expresiones regulares "re".
    else:                                           # Si no termina con un punto, 
        oracion = oracion                           # toma la oración como estaba originalmente.
    lista_palabras = oracion.split()              # Dividimos la oración en palabras tomando como separador el espacio en blanco  con otra función nativa de las strings: split.
    print("- Esta es la lista de palabras resultante: ", lista_palabras) # Split nos devuelve una lista (ordenada) de strings.
      
    gramatica = nltk.data.load(gramatica)           # Usamos la función de la sub librería "data" que nos permite cargar una gramática para que pueda ser usada luego por el parser.    
    rd_parser = nltk.RecursiveDescentParser(gramatica) # Instanciamos la clase del parser que nos da NLTK pasandole un argumento obligatorio: la gramática.
    for arbol in rd_parser.parse(lista_palabras):    # Una vez que instanciamos la clase, podemos usar sus funciones mientras le pasemos los argumentos requeridos. En este caso, usamos la función "parser" a la que le pasaremos nuestra lista de palabras, y la función nos devolverá cada árbol posible en mi gramática para esa oración.
        print("- Este es el árbol resultante: ", arbol.draw()) # Imprimimos cada árbol en la consola.
```


```python
#Para correr el Recursive Descent Parser

print('Escribí una oración:')                          # Para que me pida que escriba una oración
oracion1 = input()                                     # Para que me abra un campo en el que escriba la oración
gramatica = 'gramaticas/CFG.cfg'                       # Indicamos el path a nuestra gramatica
rd_parser(oracion1, gramatica)                         # Llamamos a la función que creamos con los dos argumentos que establecimos como obligatorios.

# Oraciones que acepta la gramática: 
# Cata/Martín/Julia/Maca/Pablo fuma
# Cata/Martín/Julia/Maca/Pablo entregó/envió el/la/un/una plaza/facultad/regalo/globo/tabaco
```

### Algunas limitaciones del recursive descent parser

1. La recursión a la izquierda provoca un loop infinito. (SN -> PRO | SN NP)
2. El parser puede llegar a tomar demasiado tiempo en considerar opciones que mirando la oración ya sabemos que no son posibles. (Fernando fuma)
3. El movimiento de backtracking borra construcciones de consituyentes que podrían ser útiles para otras partes de la oración. (El cigarrillo fue fumado por la persona)

Veamos todo esto en la demo:

### **Demo del Recursive Descent Parser**


```python
#nltk.app.rdparser()
```

## Shift Reduce Parser

Este parser, en cambio, es **botom-up**, es decir que parte de la secuencia de palabras que conforman la oración a parsear y busca asignarle una estructura acorde con la gramática. Es decir que busca secuencias de palabras que coincidan con el lado derecho de las producciones de la gramática para reemplazarlas por el símbolo del lado izquierdo.

Por ejemplo, si encuentra la secuencia "fuma" y en la gramática posee la regla V -> "fuma", hará el reemplazo por el símbolo V.

Ahora bien, el parser intentará que la subsecuencia más larga posible coincida con los símbolos a la derecha, para ello usa un "stack" (una pila, donde se apilan cosas), una especie de memoria temporal donde acumula palabras de una secuencia, una a una, mientras intenta hacerlas coincidir con el lado derecho de una producción. Esta es la acción de "shift" (desplazamiento).

Una vez que la subsecuencia coincide con una de las producciones, la reemplaza por el símbolo del lado izquierdo. Esta es la acción de "reduce" (reducir).


El parser aplicará estos pasos hasta alcanzar el símbolo del axioma.


```python
# Shift Reduce Parser

def sr_parser(oracion, gramatica):                   # Definimos una función llamada sr_parser con dos argumentos.
    oracion = oracion.lower()
    if oracion.endswith('.'):
        oracion = re.sub('\.',' ',oracion)
    else:
        oracion = oracion
    lista_palabras = oracion.split()
    gramatica = nltk.data.load(gramatica)
    sr_parser = nltk.ShiftReduceParser(gramatica)    # Instanciamos otra clase de parser
    for arbol in sr_parser.parse(lista_palabras):
        print("- Este es el árbol resultante: ", arbol)
        #return(arbol)                                # Hacemos un retorno para la función, es decir que la función aquí se va a terminar, lo que nos cortara el loop, pero nos dibujará el árbol
```


```python
print('Escribí una oración:')
oracion2 = input()
gramatica = 'gramaticas/CFG.cfg'
sr_parser(oracion2, gramatica)   

# Oraciones que acepta la gramática: 
# Cata/Martín/Julia/Maca/Pablo fuma
# Cata/Martín/Julia/Maca/Pablo entregó/envió el/la/un/una plaza/facultad/regalo/globo/tabaco
```

### Algunas limitaciones del shift reduce parser

1. Solo puede devolver un árbol posible, aunque la oración sea ambigua y acepte más de una estructura.
2. En cada acción de reducir, debe seleccionar una, aunque haya más de una posible. Y si la posibilidad de hacer shift o reduce es ambivalente, deberá decidir por una de las dos acciones. Fallas en estas decisiones pueden resultar en una falla del parseo y, al no tener implementada una forma de backtracking, si siguió un camino que fue infructuoso, decidirá que esa oración no tiene solución. (Fernando fuma el cigarrillo en el parque)


Veamos todo esto en la demo:

### **Demo del Shift Reduce parser**


```python
#nltk.app.srparser()
```

## Chart Parser

Los parsers que vimos hasta acá tienen deficiencias, sea en eficiencia o completitud. Frente a estas limitaciones, el chart parser busca mejores resultados usando dynamic programming. Dynamic programming es una técnica para desarrollar algoritmos que tiende a solucionar un problema subdividiendolo en sub problemas. Consiste en guardar la solución a esos sub problemas para poder reusarla cada vez que se la necesita.

El chart parser aplica esta técnica. Por ejemplo, construirá el SP "con el telescopio" una vez y lo guardará en una tabla. Esta tabla se denomina WFST (tabla de subcadenas bien formadas).

Vamos a armar una:


```python
def init_wfst(tokens, grammar):    
    numtokens = len(tokens)    
    wfst = [[None for i in range(numtokens+1)] for j in range(numtokens+1)]    # Esta forma de escribir un loop se llama "list comprehension"
    for i in range(numtokens):        
        productions = grammar.productions(rhs=tokens[i])        
        wfst[i][i+1] = productions[0].lhs()    
    return wfst
```


```python
def complete_wfst(wfst, tokens, grammar, trace=False):    
    index = dict((p.rhs(), p.lhs()) for p in grammar.productions())    
    numtokens = len(tokens)    
    for span in range(2, numtokens+1):        
        for start in range(numtokens+1-span):           
            end = start + span            
            for mid in range(start+1, end):                
                nt1, nt2 = wfst[start][mid], wfst[mid][end]                
                if nt1 and nt2 and (nt1,nt2) in index:                    
                    wfst[start][end] = index[(nt1,nt2)]                    
                    if trace:                        
                        print("[%s] %3s [%s] %3s [%s] ==> [%s] %3s [%s]" % (start, nt1, mid, nt2, end, start, index[(nt1,nt2)], end))    
    return wfst
```


```python
def display(wfst, tokens):    
    print('\nWFST ' + ' '.join([("%-4d" % i) for i in range(1, len(wfst))]))    
    for i in range(len(wfst)-1):        
        print("%d   " % i, end=' ')        
        for j in range(1, len(wfst)):            
            print("%-4s" % (wfst[i][j] or '.'), end=' ')        
        print()
```


```python
oracion = "el balcon fuma en el balcon".split()
for indice in range(len(oracion)):
    print(indice, oracion[indice])
```

    0 el
    1 balcon
    2 fuma
    3 en
    4 el
    5 balcon
    


```python
chart_gramatica = nltk.CFG.fromstring( #Chomsky normal form
    """O -> SN SV
    SP -> P SN
    SN -> Det NC | 'fernando'
    SV -> V NP | V SP
    Det -> 'el'
    NC -> 'balcon'
    V -> 'fuma'
    P -> 'en'""")
```


```python
wfst0 = init_wfst(oracion, chart_gramatica)
display(wfst0, oracion)
```

    
    WFST 1    2    3    4    5    6   
    0    Det  .    .    .    .    .    
    1    .    NC   .    .    .    .    
    2    .    .    V    .    .    .    
    3    .    .    .    P    .    .    
    4    .    .    .    .    Det  .    
    5    .    .    .    .    .    NC   



```python
wfst1 = complete_wfst(wfst0, oracion, chart_gramatica, trace=True)
display(wfst1, oracion)
```

    [0] Det [1]  NC [2] ==> [0]  SN [2]
    [4] Det [5]  NC [6] ==> [4]  SN [6]
    [3]   P [4]  SN [6] ==> [3]  SP [6]
    [2]   V [3]  SP [6] ==> [2]  SV [6]
    [0]  SN [2]  SV [6] ==> [0]   O [6]
    
    WFST 1    2    3    4    5    6   
    0    Det  SN   .    .    .    O    
    1    .    NC   .    .    .    .    
    2    .    .    V    .    .    SV   
    3    .    .    .    P    .    SP   
    4    .    .    .    .    Det  SN   
    5    .    .    .    .    .    NC   


Veamos el resultado de correr el Chart Parser por una oración con nuestra gramática original:


```python
gramatica = 'gramaticas/CFG.cfg'
gramatica = nltk.data.load(gramatica)
print(gramatica)
```

    Grammar with 47 productions (start state = S)
        S -> SN SV
        SN -> PRO
        SN -> NP
        SN -> D NC
        SN -> D NC SP
        NP -> 'martín'
        NP -> 'cata'
        NP -> 'fernando'
        NP -> 'fede'
        NP -> 'maca'
        NP -> 'pablo'
        NC -> 'plaza'
        NC -> 'facultad'
        NC -> 'regalo'
        NC -> 'globo'
        NC -> 'tabaco'
        NC -> 'persona'
        NC -> 'cigarrillo'
        NC -> 'telescopio'
        D -> 'el'
        D -> 'la'
        D -> 'un'
        D -> 'una'
        PRO -> 'ella'
        PRO -> 'él'
        SV -> FV SN SP
        SV -> FV SP SP
        SV -> FV SP
        SV -> FV
        SV -> FV SN
        FV -> AUX PART
        FV -> V
        AUX -> 'fue'
        PART -> 'entregado'
        PART -> 'enviado'
        PART -> 'fumado'
        PART -> 'explotado'
        V -> 'entregó'
        V -> 'envió'
        V -> 'explotó'
        V -> 'fuma'
        V -> 've'
        SP -> P SN
        P -> 'por'
        P -> 'en'
        P -> 'a'
        P -> 'con'



```python
parser = nltk.ChartParser(gramatica)
for tree in parser.parse(['fernando', 'fuma']):
    #print(tree.draw()) # La notación de punto nos permite acceder a métodos del objeto. Descomenten las lineas con "print" y miren qué hace cada método.
    #print(tree.flatten()) 
    #print(tree.productions())
    #print(tree.) #Descomenten la línea y usen la tecla "tab" para ver qué otros métodos ofrece el objeto.
    for st in tree.subtrees():
        print(st)
```

    (S (SN (NP fernando)) (SV (FV (V fuma))))
    (SN (NP fernando))
    (NP fernando)
    (SV (FV (V fuma)))
    (FV (V fuma))
    (V fuma)
    


```python
#Demo para el Chart Parser
#nltk.app.chartparser()
```

## BLLIP Parser

Brown Laboratory for Linguistic Information Processing


Bllip parser es un "reranking parser", es decir, un parser que va a devolver una serie de posibles árboles para una oración, ordenados según su probabilidad del más probable al menos probable; y una vez que obtuvo los 50 mejores árboles, va a aplicar otra estrategia de ranking para reordenar (rerank) este subset de resultados consiguiendo incluso mayor precisión.

La probabilidad de un determinado resultado viene dada por el modelo de lenguaje que el parser usa. El modelo provisto por BLLIP fue entrenado con un corpus de árboles en inglés (El Penn TreeBank) pero sus resultados podrían variar dependiendo de los datos usados en el entrenamiento, así como del método utilizado para entrenar. 

[Eugene Charniak. "A maximum-entropy-inspired parser." Proceedings of the 1st North American chapter of the Association for Computational Linguistics conference. Association for Computational Linguistics, 2000.](https://aclanthology.org/A00-2018.pdf)


[Penn TreeBank](https://catalog.ldc.upenn.edu/LDC99T42)


[TreeBank Wikipedia](https://es.wikipedia.org/wiki/TreeBank)


```python
#!pip3 install --user bllipparser
```


```python
#import bllipparser
from bllipparser import RerankingParser                             #Importa el parser
from bllipparser.ModelFetcher import download_and_install_model     # Descarga e instala el "modelo"

model_dir = download_and_install_model('WSJ', 'tmp/models')         #Crea una variable con el "modelo"
rrp = RerankingParser.from_unified_model_dir(model_dir)
```


```python
oracion2 = "john runs through the hill"
rrp.simple_parse(oracion2)
```

```python
for parse in rrp.parse(oracion2):
    print(parse) #Probar los métodos de parse usando notación de . y "tab"
```

```python
rrp.tag(oracion2)
```


```python
print('Escribí una oración en inglés')
oracion4 = input()
rrp.simple_parse(oracion4)
```


### Método tree - árboles con el formato del Penn TreeBank


```python
oracion3 = "No one saw him disembark in the unanimous night, no one saw the bamboo canoe sink into the sacred mud, but in a few days there was no one who did not know that the taciturn man came from the South"
structure = rrp.simple_parse(oracion3)
```


```python
import bllipparser
tree = bllipparser.Tree(structure)
prettytree = tree.pretty_string()
sentenceroot = tree.label
sentencespan = tree.span()
print(tree)
print(prettytree)
print(sentenceroot)
print(sentencespan)
```


### Ejemplo de una gramática con pesos de probabilidad


```python
gramatica_pesos = nltk.PCFG.fromstring("""    
    S    -> SN SV              [1.0]    
    SV   -> V  SN              [1.0]    
    SN   -> Det N              [0.8]    
    SN   -> NP                 [0.2]    
    NP   -> 'Raúl'             [1.0]    
    Det  -> 'el'               [1.0]    
    N    -> 'perro'            [0.5]    
    N    -> 'gato'             [0.5]    
    V    -> 'comió'            [1.0]    
    """)

print(gramatica_pesos)
```


## Treebank en NLTK


```python
from nltk.corpus import treebank
#nltk.download('treebank')
```


```python
t = treebank.parsed_sents('wsj_0001.mrg')[0] # Wall Street Journal
print(t)
```

{% include additional_content.html %}

{% include copybutton.html %}
