# Implementación de HPSG

Para ver la documentación de ACE ir a [https://pydelphin.readthedocs.io/en/latest/api/delphin.ace.html](https://pydelphin.readthedocs.io/en/latest/api/delphin.ace.html).

Ace utiliza como base una gramática que utiliza el formalismo de HPSG. Al correr el parser devuelve como resultado una matriz de atribución de valores que refleja la MRS o *Minimal Recursion semantics*. 

Para poder correr esta jupyter es necesario primero descargarse a este mismo directorio el archivo ace.zip, que está en la carpeta compartida del seminario, en la carpeta "Paquetes o recursos complementarios". Una vez descargado, descomprimirlo.

## Paquetes requeridos


```python
from delphin import ace
import re
import os  
import platform                                                                                     
import subprocess
```

## Parser de ACE


```python
ace.compile('ace/trunk/ace/config.tdl', 'ace/grammar.dat',executable="ace/ace/ace")
ace.parse(grm='ace/grammar.dat', datum='The cat is under the table', executable="ace/ace/ace")
```


```python
with ace.ACEParser(grm='ace/grammar.dat', executable="ace/ace/ace") as parser:
    response = parser.interact('Every dog bites a person in his arm')
    mrs = response.result(0)['mrs']
    tree = response.result(0)['tree']
    print(mrs)
    print(tree)
    
```


```python
# Este código convierte el mrs en un código de latex para convertirlo en una avm. 
# Hay que tener latex instalado.
# Una vez convertido, en linux (chequeado) y windows (no chequeado) abre el pdf.
# Para otros sistemas operativos hay que tunear el código o abrir manualmente.

mrs = re.sub(r'\]', r'\\]', mrs) # Agrega \ adelante de ]
mrs = re.sub(r'\[', '\\[', mrs) # Agrega \ adelante de [
mrs = re.sub(r'\<', '\\<', mrs) # Agrega \ adelante de <
mrs = re.sub(r'\>', '\\>', mrs) # Agrega \ adelante de >
mrs = re.sub(r'_', '\\_', mrs) # Agrega \ adelante de _
mrs = re.sub(r'([a-z]\d+)', r'\@{\1}', mrs) # Agrega "\@{" adelante y "}" atrás de cada secuencia "[a-z]\d+"
mrs = re.sub(r'([A-Z]+)', r'\\\\\1', mrs) # Agrega \\ delante de todas las secuencias de mayúsculas (esto hay que restringirlo para que sea solo en los casos en que no hay un [ antes)
#print(mrs)
with open('avm.tex', 'w', encoding='utf-8') as f:
    line1 = '\\documentclass[a4paper, 12pt]{article}\\usepackage[paperheight=30cm, paperwidth=50cm, tmargin=0.5cm, bmargin=0.5cm, lmargin=0.5cm, rmargin=0.5cm]{geometry}\\usepackage[utf8]{inputenc}\\usepackage[T1]{fontenc}\\usepackage[spanish]{babel}\\usepackage[normalem]{ulem}\\usepackage{lmodern}\\usepackage{graphicx, avm}\\begin{document}\\resizebox{48cm}{!}{\\begin{avm}'
    line3 = '\\end{avm}}\\end{document}'
    f.write(line1)
    f.write(mrs)
    f.write(line3)
    f.close()
subprocess.run(['pdflatex', '-interaction=nonstopmode', 'avm.tex'])
os.remove("avm.log")
os.remove('avm.aux')
if platform.system().lower() == 'windows':
    os.startfile('avm.pdf')
elif platform.system().lower() == 'linux':
    subprocess.run(['xdg-open', 'avm.pdf'])
else:
    raise RuntimeError('Unknown operating system "{}"'.format(platform.system()))
```


```python
# Este código convierte el tree en un código de latex para convertirlo en una avm. 
# Hay que tener latex instalado.
# Una vez convertido, en linux (chequeado) y windows (no chequeado) abre el pdf.
# Para otros sistemas operativos hay que tunear el código o abrir manualmente.

tree = re.sub(r'\(', r'[', tree) # Reemplaza "(" por "["
tree = re.sub(r'\)', r' ]', tree) # Reemplaza ")" por " ]"
tree = re.sub(r'\"([A-Z]+)\"', r'.\1', tree) # Agrega punto antes de los no terminales
tree = re.sub(r'\"([a-z]+)\"', r' \1', tree) # Agrega espacio antes de los terminales y les saca las comillas
print(tree)
with open('tree.tex', 'w', encoding='utf-8') as f:
    line1 = '\\documentclass[a4paper, 12pt]{article}\\usepackage[paperheight=50cm, paperwidth=50cm, tmargin=0.5cm, bmargin=0.5cm, lmargin=0.5cm, rmargin=0.5cm]{geometry}\\usepackage[utf8]{inputenc}\\usepackage[T1]{fontenc}\\usepackage[spanish]{babel}\\usepackage[normalem]{ulem}\\usepackage{lmodern,graphicx,qtree}\\begin{document}\\resizebox{48cm}{48}{\\Tree '
    line3 = '}\\end{document}'
    f.write(line1)
    f.write(tree)
    f.write(line3)
    f.close()
subprocess.run(['pdflatex', '-interaction=nonstopmode', 'tree.tex'])
os.remove("tree.log")
os.remove('tree.aux')
if platform.system().lower() == 'windows':
    os.startfile('tree.pdf')
elif platform.system().lower() == 'linux':
    subprocess.run(['xdg-open', 'tree.pdf'])
else:
    raise RuntimeError('Unknown operating system "{}"'.format(platform.system()))
```


```python

```
