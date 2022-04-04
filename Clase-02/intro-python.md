# Nociones básicas de Python

## Ejecución

En este seminario utilizaremos, entre otros recursos, códigos de programación escritos en Python. Existen dos grandes versiones de Python: Python 2 y Python 3. Nosotros usaremos esta última. Más específicamente, usaremos Python 3.8.10.

Existen distintas formas de ejecutar código escrito en Python. Una de ellas es en _notebooks_, como esta que están leyendo. Las notebooks son un entorno computacional interactivo que permiten escribir y ejecutar código de Python, entre otros lenguajes, y combinarlo con fragmentos de texto plano o, incluso, con imágenes.

Dos de las interfaces más utilizadas para abrir notebooks son [Jupyter Notebook](https://jupyter-notebook.readthedocs.io/en/latest/user-documentation.html) y [Jupyter Lab](https://jupyterlab.readthedocs.io/en/stable/). En la máquina virtual que les brindamos, tienen instalada la primera y, al final de esta clase, instalaremos la segunda.

Si no están usando la VM, pueden consultar la sección de [Recursos requeridos](https://fernandocar86.github.io/seminario-gramaticas-formales/Instructivos/recursos.html) para una breve explicación de cómo instalar ambas opciones.

Para ejecutarl Jupyter Notebook simplmente deben abrir una consola o terminal (`ctrl+alt+t`) y allí escribir `jupyer notebook`. Eso les abrirá una pestaña en su navegador por defecto y podrán ver las carpetas y archivos en su computadora. Si lo que desean es ejecutar Juyter Lab, deben hacer lo mismo pero escribir `jupyter lab` y se les abrirá una pestaña similar en el navegador.

<div style="text-align:center">
    <img src="./images/terminal.png" width="400px"/>
</div>


También es posible ejecutar Python directamente en la consola. Aquí lo que se hace es invocar lo que se llama un _intérprete_, un programa que lee y ejecuta código escitro en determinado lenguaje. Para esto, una vez abierta la consola, deben escribir "python" y apretar _enter_. Hecho esto, podrán leer algo como lo siguiente:

```{python}
Python 3.8.10 (default, Mar 27 2022, 23:42:37) 
[GCC 9.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
```

Allí se les indica que el intérprete se inició con éxito y que están utilizando la versión 3.8.10. Luego del indicador `>>>` pueden escribir sus comandos para ser ejecutados.

En caso de necesitar escribir comentarios al código (muy útiles para poder entenderlo más fácilmente), se puede utilizar el numeral (#). El intérprete (ya sea en consola, ya en un entorno interactivo como una notebook) ignorará las líneas que comiencen con ese símbolo.

## Operaciones aritméticas

En tanto lenguaje, Python tiene elementos que cumplen funciones específicas. Entre ellos, podemos mencionar los números enteros ($\mathbf{Z}$) y racionales ($\mathbf{Q}$) y las operaciones aritméticas como suma, resta y multiplicación entre otras.


```python
1 + 10    # suma
```




    11




```python
3 - 2     # resta
```




    1




```python
2 / 0.5   # división (es lo mismo que escribir 2 / .5)
```




    4.0




```python
3 * 4    # multiplicación
```




    12




```python
100 % 4    # resto de división (0 si la división no tiene resto, 1 si sí lo tiene)
```




    0




```python
100 % 33
```




    1




```python
3 ** 2    # potenciación
```




    9



## Variables

Los **valores** son representaciones de objetos que pueden ser manipulados por un programa de computación. Cada valor tiene un tipo determinado (ahondaremos en esto en el [siguiente apartado](#Tipos-de-objetos)). Los números utilizados anteriormente (enteros o racionales) son ejemplos de valores.

Estos valores pueden usarse directamente, como hicimos al ejecutar cuentas como `1+10`, donde `1` es el objeto entero 1 en sí mismo y lo mismo sucede con `10`, o bien pueden almacenarse en variables. Una **variable** es un nombre que refiere a cierto valor. Para asignar un valor en particular a determinada variable se utiliza el signo `=`. Esto se conoce como **asignación** o sentencia de asignación


```python
mensaje = 'hola mundo'
mensaje
```




    'hola mundo'



Una vez que asignamos un valor a nuestra variable, podemos usarla en lugar del valor que contiene. Y también podemos reasignarle otro valor.


```python
mensaje = 'el mensaje anterior cambió'
mensaje
```




    'el mensaje anterior cambió'



Si queremos usar una variable que no ha sido definida anteriormente, Python nos dirá que no la conoce:


```python
variable
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    Input In [10], in <cell line: 1>()
    ----> 1 variable


    NameError: name 'variable' is not defined


Vale aclarar que el lenguaje de programación no requiere que el nombre de la variable tenga alguna relación con el valor al que refiere (nada nos impide que una variable llamada `número`contenga un mensaje, por ejemplo). Sin embargo, es una buena práctica poner nombres descriptivos a las variables de modo que el código sea fácil de leer para un humano.

No obstante sí existen algunas reglas que se deben seguir a la hora de definir variables:

- el nombre de la variable puede ser tan largo como se desee y contener tanto letras como números, pero **no puede empezar con un número**
- es posible usar tanto mayúsculas como minúsculas, pero **por convención en Python se usan solo minúsculas** para los nombres de variables (notar que, si bien es posible usar ambas tipografías, no es indistinto: si nombramos una variable con mayúsculas, Python no la reconocerá si lueg la invocamos con su nombre en minúsculas)
- cuando el nombre de una variable tiene varias palabras, **es posible usar guines bajos** para delimitarlas, por ejemplo: `mi_primera_variable`
- si nombramos a una variable de una forma no permitida, Python nos devolverá un error


```python
1ravariable = 'hola'
```


      Input In [11]
        1ravariable = 'hola'
         ^
    SyntaxError: invalid syntax




```python
variable1 = 'hola'
variable1
```




    'hola'




```python
otra_variable = 1
otra_variable
```




    1




```python
v@riable = 'variable'
```


      Input In [14]
        v@riable = 'variable'
        ^
    SyntaxError: cannot assign to operator



Por último, existen una serie de **keywords** reservadas en Python para controlar la estructura de los programas escritos en este lenguaje. Estas no pueden ser utilizadas como nombres de variables. Un ejemplo es la palabra `class`:


```python
class = 'ilegal assigment'
```


      Input In [15]
        class = 'ilegal assigment'
              ^
    SyntaxError: invalid syntax



La lista de keywords reservadas es la siguiente:


<table>
  <tbody>
    <tr>
      <td>False</td>
      <td>for</td>
      <td>if</td>
      <td>break</td>
      <td>try</td>
      <td>assert</td>
      <td>import</td>
      <td>and</td>
      <td>in</td>
      <td>class</td>
      <td>nonlocal</td>
    </tr>
    <tr>
      <td>True</td>
      <td>while</td>
      <td>elif</td>
      <td>raise</td>
      <td>except</td>
      <td>yield</td>
      <td>from</td>
      <td>or</td>
      <td>not</td>
      <td>def</td>
      <td>global</td>
    </tr>
    <tr>
      <td>None</td>
      <td>continue</td>
      <td>else</td>
      <td>pass</td>
      <td>return</td>
      <td>finally</td>
      <td>as</td>
      <td>is</td>
      <td>with</td>
      <td>lambda</td>
      <td>del</td>
    </tr>
  </tbody>
</table>

## Tipos de objetos

Los valores pueden ser de distinto tipo: un (número) entero, un (número con punto) flotante, una cadena (de letras), etc. Cada uno de estos tipos de valores admite ciertas operaciones que veremos a continuación.

Para conocer qué tipo de objeto es determinado valor, podemos usar la función `type()`. Esta puede ser aplicada sobre el valor mismo o sobre una variables que contiene a un valor.

### Enteros y números de punto flotante

El tipo de objeto `int` permite representar los números enteros ($\mathbf{Z}$) en python.


```python
num = 35
type(num)
```




    int



Esto solo vale para valores que efetivamente son un número, no así para los que están dentro de un texto o cadena.


```python
num_string = '35'
type(num_string)
```




    str



Sin embargo, si el texto contiene solamente un número, es posible convertirlo a entero utilizando la función `int()`.


```python
num_int = int(num_string)
type(num_int)
```




    int



Por otro lado, si lo que queremos es representar un número racional ($\mathbf{Q}$), debemos usar el tipo de objeto `float`.


```python
rac = 3.6
type(rac)
```




    float



Y aquí sucede lo mismo con el número en formato texto o cadena, solo que debemos utilizar la función `float()` para la conversión.


```python
rac_str = '4.0'
type(rac_str)
```




    str




```python
rac_float = float(rac_str)
type(rac_float)
```




    float



También es posible convertir un número de entero a float y viceversa. En el primer caso perderemos los decimales y en el segundo, se agregará el punto flotante y un cero luego.


```python
rac
```




    3.6




```python
int(rac)
```




    3




```python
num_int
```




    35




```python
float(num_int)
```




    35.0



### Booleanos

Los booleanos son un tipo de valor que nos indica si algo es cierto o falso.


```python
True
```




    True




```python
False
```




    False




```python
type(True)
```




    bool



Podemos obtener este tipo de valores como resultado de expresiones booleanas:


```python
3 > 1       # mayor (para mayor o igual usar: >=)
```




    True




```python
10 <= 90    # menor o igual (para menor usar <)
```




    True




```python
0 >= 9
```




    False




```python
3 == (6/2)  # igualdad
```




    True



#### Operadores relacionales

```
x != y      # x es distinta de y
x > y       # x es más grande que y
x < y       # x es más chica que y
x >= y      # x es es más grande o igual que y
x <= y      # x es más chica o igual que y
```

#### Operadores lógicos

```
x and y     # CONJUNCIÓN: devuelve True si tanto x como y son verdaderas
x or y      # DISYUNCIÓN INCLUSIVA: devuelve True si x es verdadera y/o y también lo es
not x       # NEGACIÓN: devuelve True si x es falsa
```


```python
x = 3 > 2
x
```




    True




```python
y = type(3) == int
y
```




    True




```python
z = len([]) == 4
z
```




    False




```python
x and y
```




    True




```python
x and z
```




    False




```python
x or y
```




    True




```python
y or z
```




    True




```python
z or z
```




    False




```python
not x
```




    False




```python
not z
```




    True



### Listas

Una lista es **una secuencia de valores ordenados**, donde los valores pueden ser de cualquier tipo. Estos valores suelen ser llamados _elementos_ o _items_ de la lista.

Para definir las listas usamos corchetes (`[]`).


```python
lista = [3, 10, 'hola']
lista
```




    [3, 10, 'hola']




```python
type(lista)
```




    list



También es posible crear listas vacías.


```python
lista_vacia = []     # esto es lo mismo que lista_vacia = list()
lista_vacia
```




    []



Una lista puede asimismo contener otra lista.


```python
lista_con_listas = [1,2,3, ['soy','una','lista'], ['otra','lista']]
lista_con_listas
```




    [1, 2, 3, ['soy', 'una', 'lista'], ['otra', 'lista']]



Para conocer la longitud de una lista, puedo utilizar la función `len()`.


```python
len(lista)
```




    3




```python
len(lista_vacia)
```




    0




```python
len(lista_con_listas)
```




    5



Además, como es una secuencia ordenada, puedo ver qué elemento se encuentra en cada posición de la lista. Basta indicar la posición cuyo elemento quiero averiguar entre corchetes. Sí es importante notar que la primera posición se indica con 0 y no con 1.


```python
lista[0]    # me permite acceder al primer elemento
```




    3




```python
lista_con_listas[1]    # me permite acceder al segundo elemento
```




    2




```python
lista_con_listas[-1]    # me permite acceder al último elemento
```




    ['otra', 'lista']



Y, del mismo modo, si yo ya sé que un elemento se encuentra en una lista, puedo averiguar cuál es su posición utilizando el método `index()`. Si el elemento en cuestión aparece más de una vez en la lista (nada impide que esto suceda), este método solo nos indicará la primera posición en la cual podemos encontrarlo.


```python
lista.index('hola')
```




    2




```python
lista_con_elementos_repetidos = [1,2,3,2,4,2]
lista_con_elementos_repetidos.index(2)
```




    1



Del mismo modo que seleccionamos elementos en posiciones específicas, podemos seleccionar una porción (o _slice_, en inglés) de la lista. Para eso debemos indicar entre paréntesis la posición de inicio y la de finalización de la porción que deseamos (la posición de inicio será incluida en el la porción pero la de finalización será excluida).


```python
lista_con_elementos_repetidos[2:4]
```




    [3, 2]




```python
lista[1:]     # toma desde la segunda posición hasta el final
```




    [10, 'hola']




```python
lista[:2]     # toma desde el inicio hasta el tercer elemento (excl)
```




    [3, 10]



Algo a destacar es que, si intentamos buscar qué elemento se encuentra en una posición que no existe en la lista (porque es más corta), Python nos devolverá un error.


```python
lista[4]
```


    ---------------------------------------------------------------------------

    IndexError                                Traceback (most recent call last)

    Input In [58], in <cell line: 1>()
    ----> 1 lista[4]


    IndexError: list index out of range


#### Operaciones con listas

El operador `+` permite concatenar listas.


```python
a = [1,3,5,7]
b = [2,4,6,8]
a+b
```




    [1, 3, 5, 7, 2, 4, 6, 8]



Dada una lista y un entero, el operador `*` repite los elementos de la lista tantas veces como indique el entero.


```python
c = ['a','b','c']
c * 3
```




    ['a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c']




```python
d = [1]
d * 5
```




    [1, 1, 1, 1, 1]



#### Métodos de las listas

En tanto objetos, las listas tienen métodos específicos que nos permiten manipularlas.

El método `append()` posibilita agregar un elemento a una lista.


```python
lista_a_modificar = []
```


```python
lista_a_modificar.append('primer elemento')
```


```python
lista_a_modificar
```




    ['primer elemento']



El método `extend()` permite agregar los elementos de una lista a otra.


```python
nuevos_elementos = ['segundo elemento', 'tercer elemento']
```


```python
lista_a_modificar.extend(nuevos_elementos)
```


```python
lista_a_modificar
```




    ['primer elemento', 'segundo elemento', 'tercer elemento']



Dicho método no modifica sin embrgo la lista cuyos elementos fueron agregados.


```python
nuevos_elementos
```




    ['segundo elemento', 'tercer elemento']



El método `sort()` ordena los elementos de una lista. Si sus elementos son números, los ordena en forma creciente. Si son letras o cadenas, en orden alfabético.


```python
lista_a_ordenar = [10,4,2,9,3]
```


```python
lista_a_ordenar.sort()
```


```python
lista_a_ordenar
```




    [2, 3, 4, 9, 10]



Con los métodos `pop()` y `remove()` podemos quitar elementos de una lista. El primer método toma la posición del elemento que se quiere quitar y, el segundo, el elemento en sí.


```python
lista_con_elementos_a_quitar = ['h','o','l','a','!']
lista_con_elementos_a_quitar
```




    ['h', 'o', 'l', 'a', '!']




```python
lista_con_elementos_a_quitar.pop(2)
```




    'l'




```python
lista_con_elementos_a_quitar
```




    ['h', 'o', 'a', '!']




```python
lista_con_elementos_a_quitar.remove('a')
```


```python
lista_con_elementos_a_quitar
```




    ['h', 'o', '!']



Si lo que se desea es eliminar elementos de varias posiciones contiguas a la vez, se puede usar `del`.


```python
lista_con_elementos_a_quitar = ['h','o','l','a','!']
lista_con_elementos_a_quitar
```




    ['h', 'o', 'l', 'a', '!']




```python
del lista_con_elementos_a_quitar[2:4]
```


```python
lista_con_elementos_a_quitar
```




    ['h', 'o', '!']



#### Mutabilidad de las listas

A diferencia de otros tipos de objetos, las listas son mutables. Esto significa que métodos como los antes vistos los pueden modificar y que también es posible reasignar elementos a sus posiciones recurriendo a _slices_.


```python
lista = ['esto', 'es', 'python']
```


```python
lista[1:2] = ['una','lista','de']
```


```python
lista
```




    ['esto', 'una', 'lista', 'de', 'python']




```python
lista[2] = 'notebook'
```


```python
lista
```




    ['esto', 'una', 'notebook', 'de', 'python']



### Tuplas

Las tuplas son secuencias de valores muy similares a las listas, su gran diferencia es que son inmutables. Podemos definir una tupla con paréntesis (_()_) o simplemente ordenando sus elementos y separándolos por una coma.


```python
tupla_1 = 'h','o','l','a'
tupla_1
```




    ('h', 'o', 'l', 'a')




```python
tupla_2 = ('m','u','n','d','o')
tupla_2
```




    ('m', 'u', 'n', 'd', 'o')



También, si ya contamos con una lista o conjunto al que queremos convertir en tupla, podemos utilizar la función `tuple()`.


```python
lista = [1,2,3]
tupla = tuple(lista)
tupla
```




    (1, 2, 3)



O podemos genrar una tupla vacía:


```python
tuple()
```




    ()




```python
type(tupla)
```




    tuple



Del mismo modo que accedíamos a elementos de una lista encerrando la posición (o posiciones) entre corchetes, podemos hacerlo con las tuplas:


```python
tupla_1[2]
```




    'l'




```python
tupla_1[1:3]
```




    ('o', 'l')



Pero, al no ser mutables, no podemos cambiar los elementos que contiene:


```python
tupla
```




    (1, 2, 3)




```python
tupla[0] = 3
```


    ---------------------------------------------------------------------------

    TypeError                                 Traceback (most recent call last)

    Input In [93], in <cell line: 1>()
    ----> 1 tupla[0] = 3


    TypeError: 'tuple' object does not support item assignment


Sin embargo, podemos generar una nueva tupla y asignársela a la variable que ya existía:


```python
tupla = (3,) + tupla[1:]
tupla
```




    (3, 2, 3)



### Conjuntos

Como vimos en la sección teórica, los conjuntos son una colección de objetos o elementos.

Para instanciar un conjunto en Python, podemos:

- encerrar los elementos que queremos ubicar dentro del conjunto entre corchetes
- armar una lista o tupla y utilizarla como valor para la función `set()`


```python
lista = [1,2,3,4]
conjunto = set(lista)
conjunto
```




    {1, 2, 3, 4}




```python
type(conjunto)
```




    set



Tal y como habíamos visto: los conjuntos no tienen orden ni elementos repetidos.


```python
conjunto = {3, 'foo', (1, 2, 3), 3.14159, 3}
conjunto
```




    {(1, 2, 3), 3, 3.14159, 'foo'}



Si lo que queremos es generar un conjunto vacío, podemos hacer lo mismo pero utilizar una lista vacía o, lo que es más sencillo y prolijo, usar la función `set()` sin ningún valor.


```python
conjunto_vacio = set()
conjunto_vacio
```




    set()



Al igual que las listas, puedo contar cuántos elementos tiene un conjunto con la función `len()`.


```python
len(conjunto)
```




    4




```python
len(conjunto_vacio)
```




    0



#### Relaciones entre conjuntos

La función `subset()` nos permite evaluar si un conjunto es subconjunto de otro.


```python
a = {'a','b','c'}
b = {'a','b','c','g','h','z'}
```


```python
a.issubset(b)
```




    True




```python
a.issubset(a)
```




    True



También podemos evaluar si un conjunto contiene a otro:


```python
b.issuperset(a)
```




    True




```python
b.issuperset(b)
```




    True




```python
a.issuperset(b)
```




    False



Si lo que queremos es ver si un subconjunto es subconjunto propio, debemos usar el operador `>` y colocar a izquierda el conjunto que queremos evaluar si es subconjutno propio del que ubiquemos a derecha (también podemos usar `<` e invertir las posiciones).


```python
a < b
```




    True




```python
a < a
```




    False



Con `in` podemos evaluar la pertenencia ($\in$) de un elemento es miembro de un conjunto (esto también funciona con listas).


```python
'c' in a
```




    True




```python
'z' in a
```




    False



#### Operacines entre conjuntos


```python
x = {'m','n','o'}
```


```python
y = {'m','w','z'}
```


```python
x.union(y)            # elementos que están en x o en y
```




    {'m', 'n', 'o', 'w', 'z'}




```python
x.intersection(y)     # elementos que están en x y en y
```




    {'m'}




```python
x.difference(y)       # elementos que están en x pero no en y (x-y)
```




    {'n', 'o'}




```python
y.difference(x)       # elementos que están en y pero no en x (y-x)
```




    {'w', 'z'}




```python
from itertools import product  # vamos a volver a esto en un rato

list(product(x,y))             # con product puedo ver el producto cartesiano
```




    [('o', 'w'),
     ('o', 'z'),
     ('o', 'm'),
     ('n', 'w'),
     ('n', 'z'),
     ('n', 'm'),
     ('m', 'w'),
     ('m', 'z'),
     ('m', 'm')]




```python
list(product(y,x))
```




    [('w', 'o'),
     ('w', 'n'),
     ('w', 'm'),
     ('z', 'o'),
     ('z', 'n'),
     ('z', 'm'),
     ('m', 'o'),
     ('m', 'n'),
     ('m', 'm')]



### Diccionarios

Los diccionarios son una suerte de colección sin orden, cuyos índices son llamados _keys_ y los valores en esos índices, _values_. Es decir, así como en una lista podíamos acceder a los distintos elementos recurriendo a las posiciones (números enteros), para los diccionarios usamos keys. ¿Y qué tipo de objeto puede ser una key? ¡Muchos! Enteros, flotantes, cadenas, por nombrar algunos. Una restricción importante es que, para que estos elementos puedan funcionar como índices, cada key debe ser única. No puedo tener dos keys iguales. Y cada key está asociada a un único valor (_value_), que puede ser desde un entero, hasta una lista o una tupla, pero a un único valor.

Para crear un diccionario podemos usar tanto la función `dict()` como las llaves.


```python
diccionario_1 = dict()
diccionario_1
```




    {}




```python
diccionario_2 = dict([('a',1),('b',2),('c',3)])
diccionario_2
```




    {'a': 1, 'b': 2, 'c': 3}




```python
diccionario_3 = {
    100:[1,0,0],
    101:[1,0,1],
    102:[1,0,2]
}
diccionario_3
```




    {100: [1, 0, 0], 101: [1, 0, 1], 102: [1, 0, 2]}



Dado que no tienen orden, no podemos acceder a sus valores indicando una posición, pero sí usando las keys:


```python
diccionario_2[0]
```


    ---------------------------------------------------------------------------

    KeyError                                  Traceback (most recent call last)

    Input In [122], in <cell line: 1>()
    ----> 1 diccionario_2[0]


    KeyError: 0



```python
diccionario_2['a']
```




    1



Si el diccionario no tiene la key indicada, python nos devolerá un error.


```python
diccionario_2['z']
```


    ---------------------------------------------------------------------------

    KeyError                                  Traceback (most recent call last)

    Input In [124], in <cell line: 1>()
    ----> 1 diccionario_2['z']


    KeyError: 'z'


También podemos agregar nuevos pares key-value:


```python
diccionario_2['z'] = 'elemento nuevo'
diccionario_2
```




    {'a': 1, 'b': 2, 'c': 3, 'z': 'elemento nuevo'}



Si no conocemos cuáles son las keys de un dicionario, podemos usar el método `keys()` para acceder a esta información. Los metodos `values()` e `items()` nos dirán los valores y los pares key-value respectivamente.


```python
diccionario_3.keys()
```




    dict_keys([100, 101, 102])




```python
diccionario_3.values()
```




    dict_values([[1, 0, 0], [1, 0, 1], [1, 0, 2]])




```python
diccionario_3.items()
```




    dict_items([(100, [1, 0, 0]), (101, [1, 0, 1]), (102, [1, 0, 2])])



### Cadenas

Las cadenas o _strings_ son secuencias ordenadas de caracteres. Un número, una letra, un signo de puntuación o un espacio pueden ser un caracter. Para definir una cadena debemos encerrar los caracteres entre comillas.


```python
cadena_1 = "hola mundo!!"
cadena_1
```




    'hola mundo!!'




```python
cadena_2 = 'hola mundo con comillas simples'
cadena_2
```




    'hola mundo con comillas simples'



También es posible usar la función `str()` y convertir otro tipo de valor en una cadena.


```python
numero = 2
numero_str = str(numero)
numero_str
```




    '2'




```python
type(numero_str)
```




    str




```python
type(numero)
```




    int



De la misma forma que sucedía con las listas, al ser secuencias ordenadas, podemos acceder a los distintos caracteres de una cadena utilizando sus posiciones:


```python
lorem_ipsum = '''Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'''
lorem_ipsum
```




    'Lorem ipsum dolor sit amet, consectetur adipiscing elit,\nsed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.\nExcepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'




```python
lorem_ipsum[3]
```




    'e'




```python
lorem_ipsum[0:20]
```




    'Lorem ipsum dolor si'




```python
lorem_ipsum[-8:]
```




    'laborum.'



La función `print()` nos permite imprimir una cadena interpretando los caracteres en ella. El salto de línea, por ejemplo está representado por el caracter `\n`, Si usamos `print()`, en lugar de ver dicho caracter, veremos el salto de línea.


```python
print(lorem_ipsum)
```

    Lorem ipsum dolor sit amet, consectetur adipiscing elit,
    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.


Las cadenas no son objetos mutables, por lo cual, al igual que lo que sucedía con las tuplas, no podemos modificar sus elementos (con las listas, sí podíamos). De todos modos, sí podemos asignar una nueva cadena a una variable ya existente.


```python
cadena_1
```




    'hola mundo!!'




```python
cadena_1[0] = 'H'
```


    ---------------------------------------------------------------------------

    TypeError                                 Traceback (most recent call last)

    Input In [140], in <cell line: 1>()
    ----> 1 cadena_1[0] = 'H'


    TypeError: 'str' object does not support item assignment



```python
cadena_1 = '¡¡' + cadena_1 + '!!'
cadena_1
```




    '¡¡hola mundo!!!!'



A continuación, listamos algunas operaciones útiles que se pueden realizar sobre las cadenas:


```python
cadena_1.upper()     # pasa la cadena a mayúscula (no modifica la variable)
```




    '¡¡HOLA MUNDO!!!!'




```python
cadena_1
```




    '¡¡hola mundo!!!!'




```python
cadena_1_upper = cadena_1.upper() # pasa la cadena a minúscula (no modifica la variable)
cadena_1_upper.lower()
```




    '¡¡hola mundo!!!!'




```python
cadena_2.capitalize()     # pasa la primera letra a mayúscula (no modifica la variable)
```




    'Hola mundo con comillas simples'




```python
cadena_2.title()          # pasa la primera letra de cada palabra a mayúscula (no modifica la variable)
```




    'Hola Mundo Con Comillas Simples'




```python
cadena_2.split()          # convierte la cadena en una lista separando los caracteres por espacios
```




    ['hola', 'mundo', 'con', 'comillas', 'simples']




```python
cadena_4 = "esta|es|otra|cadena"
cadena_4.split()
```




    ['esta|es|otra|cadena']




```python
cadena_4.split('|')      # convierte la cadena en una lista separando los caracteres por pleca
```




    ['esta', 'es', 'otra', 'cadena']



## Funciones

Dentro del contexto de programación, una función es una secuencia de procedimientos que ejecutan determinado cómputo.


```python
type('hola')
```




    str



`type()` es una función que toma un argumento y devuelve una cadena que indica a qué tipo de objeto de Python pertence ese argumento. Este valor que devuelve la función se llama _valor de retorno_.

Para definir una función en Python, usamor la sentencia `def`y a continuación escribimos el nombre la función y su definición:


```python
def saludar(nombre):
    saludo = 'Hola, '+nombre
    return saludo
```


```python
saludar('Don Pepito')
```




    'Hola, Don Pepito'



Dentro de una función, los valores se asignan a variables llamadas _parámetro_. La función `saludar()`, por ejemplo, toma un argumento, y asigna ese argumento al parámetro `nombre`. Cuando se utilice ese parámetro dentro de la función, hará referencia al valor que se le asignó.

Algunas funciones tienen valores de retorno y otras no. Las primeras son llamadas _funciones fructíferas_ (_fruitful functions_) y las segunas, _nulas_ (_void functions_). Estas últimas devuelven un valor especial conocido como `None`.


```python
def imprimir_saludo(nombre):
    saludo = 'Hola, '+nombre
    print(saludo)
```


```python
imprimir_saludo('Don José')
```

    Hola, Don José



```python
valor_1 = saludar('Don Pepito')
valor_2 = imprimir_saludo('Don José')
```

    Hola, Don José



```python
valor_1
```




    'Hola, Don Pepito'




```python
valor_2
```


```python
print(valor_2)
```

    None


## Librerías

Si bien podemos definir todas las funciones que necesitemos, al ser un lenguaje de código abierto, Python cuenta con muchas funciones ya implementadas por la comunidad que contribuye a su desarrollo. En muchos casos, estas funciones se encuentran en _módulos_ o _librerías_.

Un módulo consiste en uno o más archivos que contienen una serie de funciones relacionadas. Para poder usar un módulo, debemos importarlo usando la sentencia `import`.


```python
import os         # importa el módulo os
```


```python
os.listdir('.')   # utiliza la función listdir (de os)
                  # para listar los archivos y carpetas
                  # en este directorio
```




    ['intro-python.md',
     '.ipynb_checkpoints',
     'index.md',
     'git-basics.md',
     'handout.pdf',
     'intro-python.ipynb',
     'git-basics-images',
     'images',
     'git-cheat-sheet.pdf']




```python
from os import listdir
```


```python
listdir('.')
```




    ['intro-python.md',
     '.ipynb_checkpoints',
     'index.md',
     'git-basics.md',
     'handout.pdf',
     'intro-python.ipynb',
     'git-basics-images',
     'images',
     'git-cheat-sheet.pdf']




```python
#import antigravity
```

Mientras que algunas librerías ya vienen instaladas con Python por defecto, otras deben ser instaladas en caso de necesitarlas. Para ello, podemos correr el siguiente comando desde la consola:

```
pip3 install <librería>
```

O también podemos instalarla desde Jupyter anteponiendo un signo de exclamación cerrado (_!_).


```python
! pip install art
```

    Collecting art
      Downloading art-5.5-py2.py3-none-any.whl (583 kB)
    [2K     [38;2;114;156;31m━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━[0m [32m583.5/583.5 KB[0m [31m4.7 MB/s[0m eta [36m0:00:00[0m
    [?25hInstalling collected packages: art
    Successfully installed art-5.5



```python
from art import *    # importa todo desde art
```


```python
print(art('happy'))
```

     ۜ\(סּںסּَ` )/ۜ 


Para instalar jupyter lab, de hecho, podemos hacerlo del mismo modo:


```python
! pip install jupyter lab
```

    Requirement already satisfied: jupyter in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (1.0.0)
    Collecting lab
      Downloading lab-7.0-py2.py3-none-any.whl (91 kB)
    [2K     [38;2;114;156;31m━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━[0m [32m91.9/91.9 KB[0m [31m1.8 MB/s[0m eta [36m0:00:00[0m
    [?25hRequirement already satisfied: notebook in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (6.4.8)
    Requirement already satisfied: jupyter-console in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (6.4.3)
    Requirement already satisfied: nbconvert in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (6.4.5)
    Requirement already satisfied: qtconsole in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (5.2.2)
    Requirement already satisfied: ipykernel in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (6.10.0)
    Requirement already satisfied: ipywidgets in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter) (7.7.0)
    Collecting simplejson
      Downloading simplejson-3.17.6-cp38-cp38-manylinux_2_5_x86_64.manylinux1_x86_64.manylinux_2_12_x86_64.manylinux2010_x86_64.whl (139 kB)
    [2K     [38;2;114;156;31m━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━[0m [32m139.5/139.5 KB[0m [31m4.8 MB/s[0m eta [36m0:00:00[0m
    [?25hCollecting txt2tags>=3.6
      Using cached txt2tags-3.7-py2.py3-none-any.whl (51 kB)
    Requirement already satisfied: matplotlib in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from lab) (3.5.1)
    Requirement already satisfied: matplotlib-inline<0.2.0,>=0.1.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (0.1.3)
    Requirement already satisfied: traitlets<6.0,>=5.1.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (5.1.1)
    Requirement already satisfied: nest-asyncio in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (1.5.4)
    Requirement already satisfied: ipython>=7.23.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (8.2.0)
    Requirement already satisfied: debugpy<2.0,>=1.0.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (1.6.0)
    Requirement already satisfied: jupyter-client<8.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (7.1.2)
    Requirement already satisfied: tornado<7.0,>=5.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (6.1)
    Requirement already satisfied: psutil in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipykernel->jupyter) (5.9.0)
    Requirement already satisfied: nbformat>=4.2.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipywidgets->jupyter) (5.2.0)
    Requirement already satisfied: widgetsnbextension~=3.6.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipywidgets->jupyter) (3.6.0)
    Requirement already satisfied: ipython-genutils~=0.2.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipywidgets->jupyter) (0.2.0)
    Requirement already satisfied: jupyterlab-widgets>=1.0.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipywidgets->jupyter) (1.1.0)
    Requirement already satisfied: pygments in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter-console->jupyter) (2.11.2)
    Requirement already satisfied: prompt-toolkit!=3.0.0,!=3.0.1,<3.1.0,>=2.0.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jupyter-console->jupyter) (3.0.28)
    Requirement already satisfied: pillow>=6.2.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (9.0.1)
    Requirement already satisfied: cycler>=0.10 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (0.11.0)
    Requirement already satisfied: pyparsing>=2.2.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (3.0.7)
    Requirement already satisfied: packaging>=20.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (21.3)
    Requirement already satisfied: fonttools>=4.22.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (4.31.2)
    Requirement already satisfied: kiwisolver>=1.0.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (1.4.1)
    Requirement already satisfied: numpy>=1.17 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (1.22.3)
    Requirement already satisfied: python-dateutil>=2.7 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from matplotlib->lab) (2.8.2)
    Requirement already satisfied: beautifulsoup4 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (4.10.0)
    Requirement already satisfied: bleach in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (4.1.0)
    Requirement already satisfied: jupyterlab-pygments in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.1.2)
    Requirement already satisfied: pandocfilters>=1.4.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (1.5.0)
    Requirement already satisfied: MarkupSafe>=2.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (2.1.1)
    Requirement already satisfied: jinja2>=2.4 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (3.1.1)
    Requirement already satisfied: jupyter-core in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (4.9.2)
    Requirement already satisfied: entrypoints>=0.2.2 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.4)
    Requirement already satisfied: defusedxml in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.7.1)
    Requirement already satisfied: testpath in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.6.0)
    Requirement already satisfied: nbclient<0.6.0,>=0.5.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.5.13)
    Requirement already satisfied: mistune<2,>=0.8.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbconvert->jupyter) (0.8.4)
    Requirement already satisfied: argon2-cffi in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from notebook->jupyter) (21.3.0)
    Requirement already satisfied: prometheus-client in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from notebook->jupyter) (0.13.1)
    Requirement already satisfied: terminado>=0.8.3 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from notebook->jupyter) (0.13.3)
    Requirement already satisfied: pyzmq>=17 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from notebook->jupyter) (22.3.0)
    Requirement already satisfied: Send2Trash>=1.8.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from notebook->jupyter) (1.8.0)
    Requirement already satisfied: qtpy in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from qtconsole->jupyter) (2.0.1)
    Requirement already satisfied: decorator in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (5.1.1)
    Requirement already satisfied: setuptools>=18.5 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (56.0.0)
    Requirement already satisfied: stack-data in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (0.2.0)
    Requirement already satisfied: pickleshare in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (0.7.5)
    Requirement already satisfied: jedi>=0.16 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (0.18.1)
    Requirement already satisfied: backcall in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (0.2.0)
    Requirement already satisfied: pexpect>4.3 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from ipython>=7.23.1->ipykernel->jupyter) (4.8.0)
    Requirement already satisfied: jsonschema!=2.5.0,>=2.4 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from nbformat>=4.2.0->ipywidgets->jupyter) (4.4.0)
    Requirement already satisfied: wcwidth in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from prompt-toolkit!=3.0.0,!=3.0.1,<3.1.0,>=2.0.0->jupyter-console->jupyter) (0.2.5)
    Requirement already satisfied: six>=1.5 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from python-dateutil>=2.7->matplotlib->lab) (1.16.0)
    Requirement already satisfied: ptyprocess in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from terminado>=0.8.3->notebook->jupyter) (0.7.0)
    Requirement already satisfied: argon2-cffi-bindings in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from argon2-cffi->notebook->jupyter) (21.2.0)
    Requirement already satisfied: soupsieve>1.2 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from beautifulsoup4->nbconvert->jupyter) (2.3.1)
    Requirement already satisfied: webencodings in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from bleach->nbconvert->jupyter) (0.5.1)
    Requirement already satisfied: parso<0.9.0,>=0.8.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jedi>=0.16->ipython>=7.23.1->ipykernel->jupyter) (0.8.3)
    Requirement already satisfied: importlib-resources>=1.4.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jsonschema!=2.5.0,>=2.4->nbformat>=4.2.0->ipywidgets->jupyter) (5.6.0)
    Requirement already satisfied: attrs>=17.4.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jsonschema!=2.5.0,>=2.4->nbformat>=4.2.0->ipywidgets->jupyter) (21.4.0)
    Requirement already satisfied: pyrsistent!=0.17.0,!=0.17.1,!=0.17.2,>=0.14.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from jsonschema!=2.5.0,>=2.4->nbformat>=4.2.0->ipywidgets->jupyter) (0.18.1)
    Requirement already satisfied: cffi>=1.0.1 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from argon2-cffi-bindings->argon2-cffi->notebook->jupyter) (1.15.0)
    Requirement already satisfied: asttokens in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from stack-data->ipython>=7.23.1->ipykernel->jupyter) (2.0.5)
    Requirement already satisfied: pure-eval in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from stack-data->ipython>=7.23.1->ipykernel->jupyter) (0.2.2)
    Requirement already satisfied: executing in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from stack-data->ipython>=7.23.1->ipykernel->jupyter) (0.8.3)
    Requirement already satisfied: pycparser in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from cffi>=1.0.1->argon2-cffi-bindings->argon2-cffi->notebook->jupyter) (2.21)
    Requirement already satisfied: zipp>=3.1.0 in /home/maca/personal/seminario-gramaticas-formales/venv/lib/python3.8/site-packages (from importlib-resources>=1.4.0->jsonschema!=2.5.0,>=2.4->nbformat>=4.2.0->ipywidgets->jupyter) (3.7.0)
    Installing collected packages: txt2tags, simplejson, lab
    Successfully installed lab-7.0 simplejson-3.17.6 txt2tags-3.7


## Ejercicios

1. Definir una variable llamada _laborales_ y asignarle los cinco días hábiles de la semana. Generar una nueva variable llamada _finde_ y asignarle los días del fin de semana. A partir de ambas variables, generar una nueva llamada _semana_ que contenga la lista de todos los días. Las anteriores variables deben permanecer sin modificaciones.

2. Definir una función llamda *working_days* que tome el nombre de un día de la semana y devuelva un valor booleano que indique si es un día laborable. Por ejemplo, al tomar "lunes" debe devolver "True".

3. (Ejercicio tomado de Downey et. al, 2012) Escribir una función llamada _middle_ que tome una lista y devuelva una nueva lista que contenga todos sus elementos menos el primero y el último. Por ejemplo:

```
>>> t = [1,2,3,4]
>>> middle(t)
[2,3]
```

4. (Ejercicio tomado de Downey et. al, 2012) Escribir una función llamada _chop_ que tome una lista y la modifique. La función debe quitar el primero y el último elemento de la lista y devolver `None`. Por ejemplo:

```
>>> t = [1, 2, 3, 4]
>>> chop(t)
>>> t
[2, 3]
```

5. (Ejercicio tomado de Downey et. al, 2012) Escribir una función llamada *has_duplicates* que tome una lista y devuelva `True`si la lista tiene elementos repetidos. Esta función no debe modificar la lista original.

**Referencias**

- [Downey, A., Brooks Jr, F. P., Peek, J., Todino, G., Strang, J., Robbins, A., & Rosenblatt, B. (2012). Think python. 2.0. Green Tea Press Supplemental Material:.](https://greenteapress.com/thinkpython2/thinkpython2.pdf)

{% include additional_content.html %}

{% include copybutton.html %}
