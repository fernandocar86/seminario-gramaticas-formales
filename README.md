# Requisitos técnicos

En la cursada vamos a utilizar diversos recursos computacionales, principalmente en los lenguajes de programación Python y Prolog, librerías como NLTK, Spacy y Delphin, entre otras, programas como Wish, y el sistema de control de versiones git. Para poder hacer las actividades y correr los códigos que se suban a esta repositorio, es necesario que los estudiantes tengan todo lo necesario instalado. 

- Quienes tengan mayores conocimientos de programación pueden instalarse todo lo necesario a partir de la lista que incluimos abajo. Es importante aclarar que la cátedra no va a hacer un seguimiento personalizado de los problemas de instalación que puedan surgir mediante este método, por lo que recomendamos hacer la instalación independiente solo en caso de tener experiencia en la instalación de este tipo de recursos.
    - [lista de recursos](installation/recursos.md)

- Quienes no tienen experiencia en programación o en el manejo de la línea de comandos, pueden instalarse una máquina virtual especialmente diseñada para esta cursada. Una máquina virtual es una computadora huésped que corre dentro de la anfitriona. La computadora huésped tiene su propia memoria asignada y su propio sistema operativo. La presente máquina virtual tiene como sistema operativo Ubuntu y tiene ya instalado todo lo que vamos a usar en la cursada. Pueden acceder con el siguiente link
    - [máquina virtual](installation/vm.md) (el sistema operativo anfitrión es indistinto)

#### Instrucciones para el uso de este repositorio

Para clonarte este repo, ejecutá la siguiente línea:

```
git clone https://github.com/fernandocar86/seminario-gramaticas-formales.git
```

Si vas a utilizar `notebooks`, te aconsejamos limpiarlas antes de subirlas. Para que esto se haga de forma automática, podés configurar el repositorio usando el siguiente comando (solo para usuarios de **Linux** y **Mac**):

```
./set_config.sh
```

*Solo para usuarios de Linux y Mac.

# Cronograma de clases y materiales



| Clase | Temas | Docente a cargo | Materiales |
| ------ | ------ | ------ | ------ | 
| Clase 01 | Presentación del equipo y del programa. Formalización como matematización. Teoría de conjuntos y funciones. Nociones básicas de teoría de los lenguajes (lenguaje, alfabeto, etc.).   Jerarquía de lenguajes formales.   | | - [Parsers](Clase-01/parsers.md)  | 
| Clase 02 | Jerarquía de lenguajes formales; Gramáticas, autómatas y lenguajes; equivalencia débil y equivalencia fuerte; teoría de la complejidad. Uso básico de git | | |
| Clase 03 | Gramáticas Independientes de contexto. Definición; axiomas de dominancia y de precedencia. Algunas limitaciones. Parsers: RecursiveDescentParser (NLTK), Shift-ReduceParser (NLTK), ChartParser (NLTK). Freeling. | | |
| Clase 04 | Diferenciación entre reglas de precedencia lineal y reglas de dominancia inmediata, metarreglas, postulados de significado. Principios: la convención de Rasgo Nuclear (Head Feature Convention), principio del rasgo Foot (Foot Feature Principle), principio del control de la concordancia, Restricciones de Coaparición de Rasgos, reglas léxicas. | | |
| Clase 05 | El declive de GPSG: insuficiencia del poder restrictivo de las gramáticas independientes de contexto, complejidad computacional de GPSG. Repaso de la gramática generativa. El minimalismo. Operaciones básicas: Ensamble, Adjunción, movimiento de núcleos, Agree | | |
| Clase 06 | Gramáticas Minimalistas (Minimalist grammars), Conflated Minimalist Grammars, Phase-based Minimalist Grammars, Relativized Minimalist Grammars. Las operaciones de transferencia, selección, ensamble (Merge), adjunción, el léxico y los ítems léxicos. Implementación en Prolog y Python: mgpx parser | | |
| Clase 07 | Gramáticas minimalistas con movimiento de núcleos. Implementación en Prolog y Python: mghapx parser. Nociones básicas de las gramáticas de unificación y rasgos: matrices de rasgos, rasgos simples y complejos, unificación y subsunción. Implementación mediante Feature-based grammars en NLTK  | | |
| Clase 08 | Feature-based grammars en NLTK con slash. LFG: estructura-c, estructura-f, estructura-a, ecuación funcional y descripción funcional. Implementaciones computacionales.  | | |
| Clase 09 | HPSG: esquemas de frase, reglas léxicas y structure sharing. Implementaciones computacionales  | | |
| Clase 10 | Gramáticas de dependencias. La noción de dependencia. Motivaciones para las gramáticas de dependencias. Tipos de dependencias: semánticas, sintácticas y morfológicas. Definición formal. Axiomas de las Gramáticas de dependencias: condición de raíz única, conectividad, no multidominancia, proyectividad. Implementación computacional: ProjectiveDependencyParser de NLTK en Python  | | |
| Clase 11 | Gramática de dependencias como cuádrupla: relaciones, terminales, categorías, funciones de asignación. Implementación computacional; Spacy, MaltParser, parser de dependencias de Freeling, PyStanford Dependencies. | | |
| Clase 12 | Gramáticas categoriales: Conectivas. Reglas: aplicación, asociatividad, composición, ascenso/Regla de Geech, división. Representación de la estructura sintáctica en términos de funciones y argumentos. Implementación de una gramática categorial clásica  con el parser nltk. CCG en Python. Proyecto OpenCCG. | | |
| Clase 13 | Notación lambda. Paralelismo entre la sintaxis y la semántica. Reglas léxicas. Gramáticas categoriales generalizadas. Implementación en nltk.CCG  | | |
| Clase 14 | Cierre de cursada | | |


{% comment %} 
## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/fernandocar86/seminario-gramaticas-formales/edit/main/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [Basic writing and formatting syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/fernandocar86/seminario-gramaticas-formales/settings/pages). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
{% endcomment %}

