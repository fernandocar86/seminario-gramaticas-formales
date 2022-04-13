```python
import re
```


```python
texto = open('data/motomami.txt').read() # Abre el archivo motomami.txt de la carpeta data.
print(texto) # Imprime el texto
```


```python
with open('data/motomamifiltrado.txt','w') as f:
    textofiltrado = texto.lower() # ¿qué les parece que hace?
    f.write(textofiltrado)
    print(textofiltrado)
```


```python
with open('data/motomamifiltrado.txt','w') as f:
    textofiltrado = ' '.join(re.sub("(@[a-z0-9_]+)", "", textofiltrado).split()) # ¿qué les parece que hace?
    f.write(textofiltrado)
    print(textofiltrado)
```


```python
lista = re.findall(r'(@[a-z0-9_]+)', texto)
print(lista)
```


```python
with open('data/motomamifiltrado.txt','w') as f:
    textofiltrado = ' '.join(re.sub("(\w+:\/\/\S+)", "", textofiltrado).split()) # ¿qué les parece que hace?
    f.write(textofiltrado)
    print(textofiltrado)
```


```python
lista = re.findall(r'(\w+:\/\/\S+)', texto)
print(lista)
```


```python
with open('data/motomamifiltrado.txt','w') as f:
    textofiltrado = ' '.join(re.sub("((#[A-Za-z0-9]+ ?)+)", "", texto).split()) # ¿qué les parece que hace?
    f.write(textofiltrado)
    print(textofiltrado)
```


```python
lista = re.findall("(#[A-Za-z0-9]+)", texto)
print(lista)
```


```python
with open('data/motomamifiltrado.txt','w') as f:
    textofiltrado = ' '.join(re.sub("[\.\,\!\?\:\;\-\=¿¡\|\(\)#\[\]\"]", "", texto).split()) # ¿qué les parece que hace?
    f.write(textofiltrado)
    print(textofiltrado)
```

{% include additional_content.html %}

{% include copybutton.html %}
