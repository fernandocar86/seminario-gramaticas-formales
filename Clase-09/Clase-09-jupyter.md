# Implementación de HPSG

Para ver la documentación de ACE ir a [https://pydelphin.readthedocs.io/en/latest/api/delphin.ace.html](https://pydelphin.readthedocs.io/en/latest/api/delphin.ace.html).

Ace utiliza como base una gramática que utiliza el formalismo de HPSG. Al correr el parser devuelve como resultado una matriz de atribución de valores que refleja la MRS o *Minimal Recursion semantics*. 

Para poder correr esta jupyter es necesario primero descargarse en este mismo directorio el archivo ace.zip, que está en la carpeta compartida del seminario, en la carpeta "Paquetes o recursos complementarios".

## Paquetes requeridos


```python
from delphin import ace
import re
import os  
```

## Parser de ACE


```python
ace.compile('trunk/ace/config.tdl', 'grammar.dat',executable="ace/ace")
ace.parse(grm='grammar.dat', datum='Mary', executable="ace/ace",full_forest=True,tsdbinfo=False)
```


```python
with ace.ACEParser(grm='grammar.dat', executable="ace/ace",tsdbinfo=False,full_forest=True) as parser:
    response = parser.interact('The cat is under the table.')
    mrs = response.result(0)['mrs']
    print(mrs)
    
```


```python
# Este código convierte el mrs en un código de latex para convertirlo en una avm. Hay que tener latex instalado.

import platform                                                                                     
import subprocess

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

```
