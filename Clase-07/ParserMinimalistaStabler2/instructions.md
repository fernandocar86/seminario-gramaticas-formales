# Gramática minimalista de Stabler

## Información básica

Al bajarse el repositorio de la cursada, en la carpeta correspondiente a la clase 7 van a encontrar una carpeta llamada ParserMinimalistaStabler2. En esta carpeta se encuentra el código simplificado por el grupo docente para correr en Prolog el Parser Minimalista desarrollado por Stabler, que pueden encontrar en su versión original en [la página de Stabler](https://linguistics.ucla.edu/person/edward-stabler/).

Esta versión simplificada es capaz de parsear gramáticas minimalistas que aceptan las operaciones de ensamble externo, ensamble interno, operaciones de licenciamiento con rasgos, movimiento de núcleos e incorporación.
 
## Requerimientos

Para poder correr estos materiales es preciso tener instalado Prolog, LaTeX y Wish (Simple Windowing Shell).

## Instrucciones

1. Abrir el archivo ``setup.pl`` 
2. Chequear en el archivo la sección ``Gramáticas`` y dejar descomentada solo la gramática que se desea probar (en Prolog los comentarios se introducen con el signo ``%``.
3. Guardar setup.
4. Entrar a la terminal desde ese mismo directorio.
5. Iniciar Prolog con alguno de los siguientes comandos: 
    ```
    prolog
    swipl
    ```
   Si todo funciona bien, en la pantalla va a aparecer un mensaje como el siguiente:
   ```
   Welcome to SWI-Prolog (threaded, 64 bits, version 8.4.1)
   SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
   Please run ?- license. for legal details.
   
   For online help and background, visit https://www.swi-prolog.org
   For built-in help, use ?- help(Topic). or ?- apropos(Word).
   
   ?- 
   ```
6. Cargar el archivo ``setup.pl``, encerrándolo entre corchetes, como se muestra a continuación:
   ```
   [setup].
   ```
   Van a aparecer varios errores, principalmente de variables libres, pero estos errores no deberían interferir.
7. Copiar alguno de los comandos showParse que están comentados en ``setup.pl`` que correspondan a la gramática elegida y apretar enter. Por ejemplo. Al copiar el comando ``showParse(['Juan',leer,'-pres',el,libro]).``, perteneciente a la gramática ``spanish1`` aparece lo siguiente.
   ```
   ?- showParse(['Juan',leer,'-pres',el,libro]).
   analizando...
   runtime:13.
   chartlength:23.
   aceptada como categoría C: Juan leer -pres el libro
   derivation:[0, 1, 4, 11, 24, 44, 32].
   visualizar (enter, o h para ayuda)? 
   ```
8. Si la oración es aceptada, introducir h y apretar enter. Va a aparecer una serie de opciones para visualizar los distintos árboles.
   ```
   En la terminal:	¿más?
              	  <cr>	terminar
              	   ;	más respultados
              	   t	vizualizar derivación con tk
              	   d	imprimir árbol con tk y ltree.tex
              	   b	imprimir árbol escueto con tk y ltree.tex
              	   x	imprimir árbol de x con barra con tk y ltree.tex
              	   p	imprimir árbol en la terminarl y ltree.tex
              	   q	imprimir árbol escueto en la terminal y ltree.tex
              	   r	imprimir árbol de x con barra en la terminal y ltree.tex
              	   0	mostrar estructura de dependencias no factorizada horizontalmente
              	   1	mostrar estructura de dependencia factorizada verticalmente
              	   a	abortar
          	 or anything else	for this help
   ```

{% include copybutton.html %}

{% include additional_content.html %}
