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

    <ipython-input-10-1748287bc46a> in <module>
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


      File "<ipython-input-11-bd5ff5b2efeb>", line 1
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


      File "<ipython-input-14-f732e74b984e>", line 1
        v@riable = 'variable'
        ^
    SyntaxError: cannot assign to operator



Por último, existen una serie de **keywords** reservadas en Python para controlar la estructura de los programas escritos en este lenguaje. Estas no pueden ser utilizadas como nombres de variables. Un ejemplo es la palabra `class`:


```python
class = 'ilegal assigment'
```


      File "<ipython-input-15-e19c9b8244b3>", line 1
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

    <ipython-input-58-09bfed834fa2> in <module>
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




    [('n', 'w'),
     ('n', 'z'),
     ('n', 'm'),
     ('m', 'w'),
     ('m', 'z'),
     ('m', 'm'),
     ('o', 'w'),
     ('o', 'z'),
     ('o', 'm')]




```python
list(product(y,x))
```




    [('w', 'n'),
     ('w', 'm'),
     ('w', 'o'),
     ('z', 'n'),
     ('z', 'm'),
     ('z', 'o'),
     ('m', 'n'),
     ('m', 'm'),
     ('m', 'o')]




```python

```

### Diccionarios

### Cadenas

convertir cadenas a listas


```python
list('hola')
```




    ['h', 'o', 'l', 'a']




```python
'hola mundo'.split()
```




    ['hola', 'mundo']




```python
bla
```


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    <ipython-input-111-047595d0fae9> in <module>
    ----> 1 bla
    

    NameError: name 'bla' is not defined


## Funciones

## Librerías

import

## Ejercicios


```python

```

**Referencias**

- [Downey, A., Brooks Jr, F. P., Peek, J., Todino, G., Strang, J., Robbins, A., & Rosenblatt, B. (2012). Think python. 2.0. Green Tea Press Supplemental Material:.](https://greenteapress.com/thinkpython2/thinkpython2.pdf)

{% include additional_content.html %}

{% include copybutton.html %}
