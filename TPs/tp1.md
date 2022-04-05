# Trabajo Práctico #1

Este trabajo práctico está pensado para ser resuelto en grupos de 3 personas. Para simplificar la enunciación de las consignas llamaremos a cada integrante _A_, _B_ y _C_. Pueden decidir entre ustedes quién es A, quién es B y quién es C. Lo importante es que, una vez distribuidos los roles, los respeten.

## Consignas

1. **La persona A** deberá crear un **repositorio público** con un `README.md` que solo tenga, en la primera línea, el nombre del repositorio
2. **La persona B** debe clonarse ese repositorio y crear una rama llamada "persona-B" a partir de main. Allí debe:
   - hacer un commit que agregue en la tercera línea del README los nombres de los integrantes separados por comas como se indica a continuación:
        ```
        INTEGRANTES: <integrante-A>, <integrante-B>, <integrante-C>
        ```
        donde las cadenas encerradas entre corchetes angulares (_<_,_>_) deben ser reemplazadas por los nombres y apellidos reales
   - hacer un segundo commit donde se suba un archivo llamado `archivo_B.txt` vacío
   - hacer push de su rama al repositorio en GitHub
3. La **persona C** también debe clonarse ese repositorio y crear una rama llamada "persona-C". En ella debe:
   - hacer un commit que modifique el README pegando la siguiente frase en la línea 3: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
   - hacer un segundo commit donde se suba un archivo llamado `archivo_C.txt` vacío
   - hacer push de su rama al repositorio en GitHub
   - mergear su rama a la rama main (_i.e._ introducir los cambios de su rama en main) y actualizar el remoto
4. La **persona A** debe:
   - mergear la rama de la **persona B** a main y actualizar el repositorio remoto.

Para la resolución de las consigas, es importante que sigan los pasos en el orden propuesto. Si en algún momento surgen conflictos, aceptar los cambios de todos los integrantes.

## Fecha de entrega

El plazo límite para entregar este trabajo es el **martes 12 de abril a las 9:00 am**.

Para ese horario esperamos que completen [este formulario](https://forms.gle/ypXstqEBqJCwnxXg7) en el que se les solicitará que especifiquen quiénes son las personas que integran el grupo de trabajo y cuál es el repositorio que debemos evaluar.

Es suficiente con que una sola persona por grupo complete el formulario.

{% include copybutton.html %}

{% include additional_content.html %}
