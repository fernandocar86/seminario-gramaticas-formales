# Trabajo Práctico #2

Para la resolución de este TP, deberán formar grupos de 4 personas y seguir las indicaciones del documento "Flujo de trabajo"

## Consignas

Dadas las siguientes oraciones:

a- Gurgeh y Yay juegan juegos violentos. - Recursive Descent Parser

b- Ellos patean y dan piñas virtualmente. - Recursive Descent Parser

c- Disparan armas que son falsas. - Shift Reduce Parser

d- Estos amigos manipulan jugadores con sus piezas. - Shift Reduce Parser

1- Escribir las producciones de una gramática independiente de contexto que pueda generarlas. Para ello, cada miembro del grupo será responsable de agregar las reglas de una sola de las oraciones (si la oración es ambigua, deberá agregar todas las reglas necesarias para generar los múltiples significados). Para las categorías no terminales pueden utilizar las vistas en clase, pero recomendamos utilizar el símbolo S (en vez de O) para indicar la oración.

2- Cada uno de los miembros del grupo deberá crear una notebook llamada "TP2-{NOMBRE_DE_SU_ORACION}" en la que importará el parser indicado a la derecha de la oración que tiene a cargo y lo usará para parsearla. El resultado final deberá ser el dibujo del árbol correspondiente. Si alguno de los parsers fallara para una oración determinada (no puede parsear la oración o no devuelve todas los árboles posibles), entre TODOS los miembros del equipo deberán escribir una explicación para esta falla en la misma notebook usando el formato "markdown".

## Entrega

La entrega se hará siguiendo lo indicado en el "Flujo de trabajo". 

El plazo límite para entregar este trabajo es el **martes 03 de mayo a las 9:00 am**.

Para ese horario esperamos que completen [este formulario](https://forms.gle/EHqi19bi48UgMth59) en el que se les solicitará que especifiquen quiénes son las personas que integran el grupo de trabajo y cuál es el repositorio que debemos evaluar.

Es suficiente con que una sola persona por grupo complete el formulario.

## Evaluación

Se evaluarán los siguientes aspectos:

| Tema | Desempeño | Evaluación |
|------|-----------|------------|
| Gramática | Reglas completas, genera la oración esperada, no sobre genera, no contradice reglas propuestas por los compañeros | **40** pts |
| Gramática | Reglas incompletas o no genera la oración esperada o sobre genera o contradice reglas propuestas por los compañeros | **20** pts |
|Gramática | Reglas incompletas, no genera la oración esperada, sobre genera, contradice reglas propuestas por los compañeros | **10** pts |
| Notebook | Agrega parser, el parser funciona correctamente, la oración es parseada con éxito (salvo falla esperable en el parser) | **40** pts |
| Notebook | El parser no se agrega correctamente o no funciona correctamente o no se cumple el parseo esperado de la oración | **20** pts |
| Notebook | No se agrega el parser o se agrega pero no se parsea la oración | **10** pts |

### Extras
- Parser falla esperadamente y se ofrece una explicación: **10** pts

- Flujo de trabajo seguido correctamente: **10** pts

### Puntajes

100 pts: excelente

80-90 pts: muy bueno

60-70 pts: bueno

40-60 pts: regular

10-30 pts: insuficiente


{% include copybutton.html %}

{% include additional_content.html %}
