# Nociones básicas de Python

## Ejecución

En este seminario utilizaremos, entre otros recursos, códigos de programación escritos en Python. Existen dos grandes versiones de Python: Python 2 y Python 3. Nosotros usaremos esta última. Más específicamente, usaremos Python 3.8.10.

Existen distintas formas de ejecutar código escrito en Python. Una de ellas es en _notebooks_, como esta que están leyendo. Las notebooks son un entorno computacional interactivo que permiten escribir y ejecutar código de Python, entre otros lenguajes, y combinarlo con fragmentos de texto plano o, incluso, con imágenes.

Dos de las interfaces más utilizadas para abrir notebooks son [Jupyter Notebook](https://jupyter-notebook.readthedocs.io/en/latest/user-documentation.html) y [Jupyter Lab](https://jupyterlab.readthedocs.io/en/stable/). En este seminario utilizaremos la segunda, pero ambas son muy similares.

Si están usando la VM, ya tienen Jupyter Lab instalado en la máquina. Si no la están usando, deberán instalarse el programa(consultar la sección de [Recursos requeridos](https://fernandocar86.github.io/seminario-gramaticas-formales/Instructivos/recursos.html) para una breve explicación).

Una vez que lo tengan instalado, para ejecutarlo simplmente deben abrir una consola o terminal (`ctrl+alt+t`) y allí escribir `jupyer lab`. Eso les abrirá una pestaña en su navegador por defecto y podrán ver las carpetas y archivos en su computadora.

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




```python
3 > 1    # mayor (para mayor o igual usar: >=)
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
3 == (6/2)    # igualdad
```




    True



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

    Input In [14], in <cell line: 1>()
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


      Input In [15]
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


      Input In [18]
        v@riable = 'variable'
        ^
    SyntaxError: cannot assign to operator



Por último, existen una serie de **keywords** reservadas en Python para controlar la estructura de los programas escritos en este lenguaje. Estas no pueden ser utilizadas como nombres de variables. Un ejemplo es la palabra `class`:


```python
class = 'ilegal assigment'
```


      Input In [19]
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


```python
type(3)
```




    int




```python
numero = 5
type(5)
```




    int




```python
cadena = 'soy un texto'
type(cadena)
```




    str




```python
type(3.5)
```




    float




```python
valor = '42'
type(valor)
```




    str



### Listas

Una lista es **una secuencia de valores ordenados**, donde los valores pueden ser de cualquier tipo. Estos valores suelen ser llamados _elementos_ o _items_ de la lista.

Para definir las listas usamos corchetes (`[]`).


```python
lista = [3, 10, 'hola']
lista
```




    [3, 10, 'hola']



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

    Input In [39], in <cell line: 1>()
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



### Conjuntos

### Tuplas

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



### Booleanos

### Enteros

### Números de punto flotante

## Funciones

## Iteraciones

## Condicionales

## Librerías

import

## Ejercicios


```python

```

**Referencias**

- [Downey, A., Brooks Jr, F. P., Peek, J., Todino, G., Strang, J., Robbins, A., & Rosenblatt, B. (2012). Think python. 2.0. Green Tea Press Supplemental Material:.](https://greenteapress.com/thinkpython2/thinkpython2.pdf)
