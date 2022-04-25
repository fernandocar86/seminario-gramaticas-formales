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

<table>
  <tr>
    <th>Clase</th>
    <th>Fecha</th>
    <th>Temas</th>
    <th>Materiales</th>
  </tr>
  <tr>
    <td><a href="./Clase-01/index.md">01</a></td>
    <td>29/03/22</td>
    <td>
        <ul>
            <li>Presentación del equipo y del programa.</li>
            <li>Dinámica de clases.</li>
            <li>Presentación de trabajos prácticos.</li>
        </ul>
    </td>
    <td>
        <ul>
            <li><a href="https://forms.gle/aogRG9iQtLSGnf3q9">Form de presentación</a></li>
            <li><a href="./Instructivos/github_user.md">Usuario de GitHub</a></li>
            <li><a href="./Instructivos/tutorialmv.pdf">Máquina virtual</a></li>
        </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-02/index.md">02</a></td>
    <td>05/04/22</td>
    <td>
        <ul>
            <li>Uso básico de git.</li>
            <li>Formalización como matematización. Teoría de conjuntos y funciones. Nociones básicas de teoría de los lenguajes (lenguaje, alfabeto, etc.).</li>
            <li>Introducción a Python.</li>
        </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-02/git-basics.md">Apunte sobre git</a></li>
        <li><a href="./TPs/tp1.md">TP #1</a></li>
        <li><a href="./Clase-02/handout.pdf">Apuntes sobre formalización</a></li>
        <li><a href="./Clase-02/intro-python.md">Notebook sobre Python</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-03/index.md">03</a></td>
    <td>12/04/22</td>
    <td>
        <ul>
            <li>Jerarquía de lenguajes formales. Gramáticas, autómatas y lenguajes. Equivalencia débil y equivalencia fuerte; teoría de la complejidad.</li>
        </ul>
    </td>
    <td>
    <ul>
    	<li><a href="./Clase-03/handout.pdf">Apunte sobre jerarquía de Chomsky</a></li>
    	<li><a href="./Clase-03/Clase-03-jupyter.md">Notebook librería re</a></li>
    </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-04/index.md">04</a></td>
    <td>19/04/22</td>
    <td>
      <ul>
        <li>Gramáticas Independientes de contexto. Definición; axiomas de dominancia y de precedencia. Algunas limitaciones.</li>
        <li>Parsers: RecursiveDescentParser (NLTK), Shift-ReduceParser (NLTK), ChartParser (NLTK).</li>
      </ul>
    </td>
    <td>
      <ul>
      	<li><a href="./Clase-04/handout.pdf">Apunte sobre CFG</a></li>
        <li><a href="./Clase-04/Clase-04-jupyter.md">Visualización de Notebook</a></li>	
        <li><a href="./TPs/tp2.md">TP #2</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-05/index.md">05</a></td>
    <td>26/04/22</td>
    <td>
      <ul>
        <li>Diferenciación entre reglas de precedencia lineal y reglas de dominancia inmediata, metarreglas, postulados de significado. Principios: la convención de Rasgo Nuclear (Head Feature Convention), principio del rasgo Foot (Foot Feature Principle), principio del control de la concordancia, Restricciones de Coaparición de Rasgos, reglas léxicas.</li>
        <li>El declive de GPSG: insuficiencia del poder restrictivo de las gramáticas independientes de contexto, complejidad computacional de GPSG.</li>
      </ul>
    </td>
    <td>
      <ul>
    <li><a href="./Clase-05/handout.pdf">Apunte sobre GPSG</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-06/index.md">06</a></td>
    <td>03/05/22</td>
    <td>
      <ul>
        <li>Repaso de la gramática generativa. El minimalismo. Operaciones básicas: ensamble interno, ensamble externo, agree. Teoría de X'. Teoría de la frase desnuda. Estructura básica de la cláusula.</li>
        <li>Gramáticas minimalistas (Minimalist grammars), Conflated Minimalist Grammars, Phase-based Minimalist Grammars, Relativized Minimalist Grammars. Las operaciones de transferencia, selección, ensamble (Merge), el léxico y los ítems léxicos.</li>
        <li>Implementación en Prolog y Python: mgpx parser e implementación del minimalismo de Alex Warstadt.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-06/ParserMinimalistaStabler1/instructions.md">Instrucciones para correr el parser minimalista en Prolog</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-07/index.md">07</a></td>
    <td>10/05/22</td>
    <td>
      <ul>
        <li>Gramáticas minimalistas con movimiento de núcleos, adjunción, incorporación.</li>
        <li>Implementación en Prolog y Python: mghapx parser.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-07/ParserMinimalistaStabler2/instructions.md">Instrucciones para correr el parser minimalista en Prolog</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-08/index.md">08</a></td>
    <td>17/05/22</td>
    <td>
      <ul>
        <li>Nociones básicas de las gramáticas de unificación y rasgos: matrices de rasgos, rasgos simples y complejos, unificación y subsunción.</li>
        <li>Implementación mediante Feature-based grammars en NLTK.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-08/Clase-08-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-09/index.md">09</a></td>
    <td>24/05/22</td>
    <td>
      <ul>
        <li>LFG: estructura-c, estructura-f, estructura-a, ecuación funcional y descripción funcional.</li>
        <li>Implementaciones computacionales: XLE (demo).</li>
      </ul>
    </td>
    <td></td>
  </tr>
  <tr>
    <td><a href="./Clase-10/index.md">10</a></td>
    <td>31/05/22</td>
    <td>
      <ul>
        <li>HPSG: esquemas de frase, reglas léxicas y structure sharing.</li>
        <li>Minimal Recursion Semantics.</li>
        <li>Implementaciones computacionales: ACE, pydelphin.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-10/Clase-10-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-11/index.md">11</a></td>
    <td>07/06/22</td>
    <td>
      <ul>
        <li>Gramáticas de dependencias. La noción de dependencia. Motivaciones para las gramáticas de dependencias. Tipos de dependencias: semánticas, sintácticas y morfológicas.</li>
        <li>Definición formal. Axiomas de las Gramáticas de dependencias: condición de raíz única, conectividad, no multidominancia, proyectividad.</li>
        <li>Implementación computacional; Spacy, MaltParser, parser de dependencias de Freeling, PyStanford Dependencies.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-11/Clase-11-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-12/index.md">12</a></td>
    <td>14/06/22</td>
    <td>
      <ul>
        <li>Gramática de dependencias como cuádrupla: relaciones, terminales, categorías, funciones de asignación.</li>
        <li>Implementación computacional: ProjectiveDependencyParser de NLTK en Python.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-12/Clase-12-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-13/index.md">13</a></td>
    <td>21/06/22</td>
    <td>
      <ul>
        <li>Gramáticas categoriales: Conectivas. Reglas: aplicación, asociatividad, composición, ascenso/Regla de Geech, división. Representación de la estructura sintáctica en términos de funciones y argumentos.</li>
        <li>Implementación de una gramática categorial clásica con el parser NLTK. CCG en Python. Proyecto OpenCCG.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-13/Clase-13-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-14/index.md">14</a></td>
    <td>28/06/22</td>
    <td>
      <ul>
        <li>Notación lambda. Paralelismo entre la sintaxis y la semántica. Reglas léxicas.</li>
        <li>Gramáticas categoriales generalizadas. Implementación en NLTK y CCG.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li><a href="./Clase-14/Clase-14-jupyter.md">Visualización de Notebook</a></li>
      </ul>
    </td>
  </tr>
  <tr>
    <td><a href="./Clase-15/index.md">15</a></td>
    <td>05/05/22</td>
    <td>
      <ul>
        <li>Cierre de cursada.</li>
      </ul>
    </td>
    <td></td>
  </tr>
</table>

{% include change_href.html %}

{% include additional_content.html %}
