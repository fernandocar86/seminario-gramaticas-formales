Python como lenguaje de programación orientado a objetos.

¿Por qué es un lenguaje? 

- Elementos primitivos (números)
- Reglas de escritura (nombres de vairbales, por ejemplo)


```python

```

## Los conjuntos en Pytho

Listas:

- elementos repetidos
- orden
- son mutables


```python
lista = ['a','b','c','d','a','a']
lista
```


```python
len(lista)
```


```python
lista[0]
```


```python
for element in lista:
    print(element)
```


```python
conjunto = set(lista)
```


```python
conjunto
```


```python
len(conjunto)
```


```python
for element in conjunto:
    print(element)
```


```python
empty = set()
A = set([2,4,6,8,10])
B = set([3,6,9,12,15])
```


```python
A.difference(B)
```


```python
B.difference(A)
```


```python
A.intersection(B)
```


```python
empty.difference(A)
```


```python
A.union(B)
```


```python
A.union(empty) == A
```


```python
A.add(12)
```


```python
A
```


```python
A.add(2)
```


```python
A
```


```python
lista
```


```python
lista.append('z')
```


```python
lista
```


```python
lista + lista
```


```python
lista
```


```python
A + B
```


```python
A_subset = {2,4}
```


```python
A_subset.issubset(A)
```


```python
A.issubset(A_subset)
```


```python
empty.issubset(A)
```


```python
A.issuperset(A_subset)
```


```python
A.symmetric_difference(B)
```


```python
A.remove(2)
```


```python
A
```


```python
A.pop()
```


```python
A
```

{% include additional_content.html%}


```python

```
