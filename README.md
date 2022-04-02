{% include head.html %}

## Requerimientos técnicos

En la cursada vamos a utilizar diversos recursos computacionales, principalmente en los lenguajes de programación Python y Prolog, librerías como NLTK, Spacy y Delphin, entre otras, programas como Wish, y el sistema de control de versiones git. Para poder hacer las actividades y correr los códigos que se suban a esta repositorio, es necesario que los estudiantes tengan todo lo necesario instalado.

- Quienes tengan mayores conocimientos de programación pueden instalarse todo lo necesario a partir de la lista que incluimos abajo. Es importante aclarar que la cátedra no va a hacer un seguimiento personalizado de los problemas de instalación que puedan surgir mediante este método, por lo que recomendamos hacer la instalación independiente solo en caso de tener experiencia en la instalación de este tipo de recursos.
    - [lista de recursos](./Instructivos/recursos.md)

- Quienes no tienen experiencia en programación o en el manejo de la línea de comandos, pueden instalarse una máquina virtual especialmente diseñada para esta cursada. Una máquina virtual es una computadora huésped que corre dentro de la anfitriona. La computadora huésped tiene su propia memoria asignada y su propio sistema operativo. La presente máquina virtual tiene como sistema operativo Ubuntu y tiene ya instalado todo lo que vamos a usar en la cursada. Pueden acceder con el siguiente link
    - [máquina virtual](./Instructivos/tutorialmv.pdf) (el sistema operativo anfitrión es indistinto)

En las partes teóricas de las clases vamos a usar *handouts* en pdf. Puestos que algunos de estos documentos incluyen campos especiales que no son compatibles con todos los visualizadores de pdf (por ejemplo, no todas las versiones del acrobat reader), recomendamos utilizar [evince](https://wiki.gnome.org/Apps/Evince). Pueden encontrar una versión para Windows [aquí](https://evince.softonic.com/) o [aquí](https://evince.uptodown.com/windows). No hemos probado ninguno de los dos links, por lo que agredecemos si nos pueden avisar si funcionan bien.

## Instrucciones para los trabajos prácticos
Para cumplir con la regularidad en este seminario, es necesario realizar una serie de trabajos prácticos que serán oportunamente presentados durante la cursada. Para poder resolverlos, es necesario tener en cuenta [estas instrucciones](./Instructivos/flujo_de_trabajo.md).

## Cronograma de clases y materiales


| Clase | Temas | Materiales |
| ------ | ------ | ------ |
|Clase 01 <br> 29/03 | Presentación del equipo y del programa. Dinámica de clases. Presentación de trabajos prácticos. |- [Form de presentación](https://docs.google.com/forms/d/1KPm1NavIN9sPfl7bTirTtNs-BcgAO1jZvV2TAurPuyE/edit)<br>- [Usuario de GitHub](./Instructivos/github_user.md)<br>- [Máquina virtual](./Instructivos/tutorialmv.pdf) |
| Clase 02 <br> 05/04 | Formalización como matematización. Teoría de conjuntos y funciones. Nociones básicas de teoría de los lenguajes (lenguaje, alfabeto, etc.). <br> Uso básico de git | - [Apunte sobre git](./Clase-02/git-basics.md)<br>- [TP #1](./TPs/tp1.md)<br>- [Apuntes sobre formalización]()<br>- [Notebook sobre Python]() |
| Clase 03 <br> 12/04 | Jerarquía de lenguajes formales; Gramáticas, autómatas y lenguajes; equivalencia débil y equivalencia fuerte; teoría de la complejidad. |  |
| Clase 04 <br> 19/04 | Gramáticas Independientes de contexto. Definición; axiomas de dominancia y de precedencia. Algunas limitaciones.<br>Parsers: RecursiveDescentParser (NLTK), Shift-ReduceParser (NLTK), ChartParser (NLTK). Freeling. | - [Visualización de Notebook](Clase-04/Clase-04-jupyter.md) |
| Clase 05 <br> 26/04 | Diferenciación entre reglas de precedencia lineal y reglas de dominancia inmediata, metarreglas, postulados de significado. Principios: la convención de Rasgo Nuclear (Head Feature Convention), principio del rasgo Foot (Foot Feature Principle), principio del control de la concordancia, Restricciones de Coaparición de Rasgos, reglas léxicas.<br>El declive de GPSG: insuficiencia del poder restrictivo de las gramáticas independientes de contexto, complejidad computacional de GPSG. | |
| Clase 06 <br> 03/05 | Repaso de la gramática generativa. El minimalismo. Operaciones básicas: ensamble interno, ensamble externo, agree. Teoría de X'. Teoría de la frase desnuda. Estructura básica de la cláusula. <br>Gramáticas minimalistas (Minimalist grammars), Conflated Minimalist Grammars, Phase-based Minimalist Grammars, Relativized Minimalist Grammars. Las operaciones de transferencia, selección, ensamble (Merge), el léxico y los ítems léxicos.<br>Implementación en Prolog y Python: mgpx parser e implementación del minimalismo de Alex Warstadt | [Instrucciones para correr el parser minimalista en Prolog](Clase-06/ParserMinimalistaStabler1/instructions.md) |
| Clase 07 <br> 10/05 | Gramáticas minimalistas con movimiento de núcleos, adjunción, incorporación.<br>Implementación en Prolog y Python: mghapx parser. | [Instrucciones para correr el parser minimalista en Prolog](Clase-07/ParserMinimalistaStabler2/instructions.md)
| Clase 08 <br> 17/05 | Nociones básicas de las gramáticas de unificación y rasgos: matrices de rasgos, rasgos simples y complejos, unificación y subsunción.<br>Implementación mediante Feature-based grammars en NLTK. | - [Visualización de Notebook](Clase-08/Clase-08-jupyter.md) |
| Clase 09 <br> 24/05 | LFG: estructura-c, estructura-f, estructura-a, ecuación funcional y descripción funcional. Implementaciones computacionales: XLE (demo). | |
| Clase 10 <br> 31/05 | HPSG: esquemas de frase, reglas léxicas y structure sharing.<br>Minimal Recursion Semantics.<br>Implementaciones computacionales: ACE, pydelphin. | - [Visualización de Notebook](Clase-10/Clase-10-jupyter.md) |
| Clase 11 <br> 07/06 | Gramáticas de dependencias. La noción de dependencia. Motivaciones para las gramáticas de dependencias. Tipos de dependencias: semánticas, sintácticas y morfológicas.<br>Definición formal. Axiomas de las Gramáticas de dependencias: condición de raíz única, conectividad, no multidominancia, proyectividad. <br>Implementación computacional; Spacy, MaltParser, parser de dependencias de Freeling, PyStanford Dependencies. | - [Visualización de Notebook](Clase-11/Clase-11-jupyter.md) |
| Clase 12 <br> 14/06 | Gramática de dependencias como cuádrupla: relaciones, terminales, categorías, funciones de asignación.<br>Implementación computacional: ProjectiveDependencyParser de NLTK en Python | - [Visualización de Notebook](Clase-12/Clase-12-jupyter.md) |
| Clase 13 <br> 21/06 | Gramáticas categoriales: Conectivas. Reglas: aplicación, asociatividad, composición, ascenso/Regla de Geech, división. Representación de la estructura sintáctica en términos de funciones y argumentos.<br>Implementación de una gramática categorial clásica con el parser NLTK. CCG en Python. Proyecto OpenCCG.| - [Visualización de Notebook](Clase-13/Clase-13-jupyter.md) |
| Clase 14 <br> 28/06 | Notación lambda. Paralelismo entre la sintaxis y la semántica. Reglas léxicas.<br>Gramáticas categoriales generalizadas. Implementación en NLTK y CCG. | - [Visualización de Notebook](Clase-14/Clase-14-jupyter.md) |
| Clase 15 <br> 05/07| Cierre de cursada | |

{% include additional_content.html %}
