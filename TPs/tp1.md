# Trabajo Práctico #1

Para simplificar la enunciación de las consignas vamos a considear que _A_, _B_ y _C_ refieren a tres personas distintas integrantes del equipo, pero siempre a la misma persona. Pueden decidir entre ustedes quién es cada una.

## Consignas

1. **La persona A** deberá crear un **repositorio público** con:
   - un `README.md`
   - un `.gitignore` para Python
2. **La persona B** debe clonarse ese repositorio y crear una rama llamada "persona-B" a partir de main. Allí debe:
   - hacer un commit que agregue en la tercera línea del README los nombres y apellidos de los integrantes separados por una coma como se indica a continuación:
        ```
        INTEGRANTES: <integrante-A>, <integrante-B>, <integrante-C>
        ```
        donde las cadenas encerradas entre corchetes angulares (_<_,_>_) deben ser reemplazadas por los nombres y apellidos reales
   - hacer un segundo commit donde se suba una `notebook` llamada "intro_python.ipynb" en la que defina una lista con elementos repetidos y la convierta en un conjunto (tanto la lista como el conjunto deben estar asignados a una variable)
   - hacer push de su rama al repositorio en GitHub
3. La **persona C** también debe clonarse ese repositorio y crear una rama llamada "persona-C". En ella debe:
    - hacer un commit que modifique el README pegando la siguiente frase en la línea 3: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    - hacer un nuevo commit donde suba una `notebook` llamada "intro_python.ipynb" en la que se le asigne una cadena a una variable, se defina una función que cuente caracteres y se aplique esta función a la variable antes declarada
    - hacer push de su rama al repositorio en GitHub
4. La **persona A** debe agregar un commit en la rama "main" en el que agregue al README una descripción de las notebooks generadas por los otros integrantes del equipo. Hacer push de los cambios en la rama "main" al repositorio en GitHub.


## Fecha de entrega

El plazo límite para entregar este trabajo es el **martes 12 de abril a las 9:00 am**.

Para ese horario esperamos que completen [este formulario](https://forms.gle/ypXstqEBqJCwnxXg7) en el que se les solicitará que especifiquen quiénes son las personas que integran el grupo de trabajo y cuál es el repositorio que debemos evaluar.

Es suficiente con que una sola persona por grupo complete el formulario.

{% include copybutton.html %}

{% include additional_content.html %}
