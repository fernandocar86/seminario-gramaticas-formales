# Gramática minimalista de Stabler

## Información básica

Al bajarse el repositorio de la cursada, en la carpeta correspondiente a la clase 6 van a encontrar una carpeta llamada ParserMinimalistaStabler1. En esta carpeta se encuentra el código simplificado por el grupo docente para correr en Prolog el Parser Minimalista desarrollado por Stabler, que pueden encontrar en su versión original en [la página de Stabler](https://linguistics.ucla.edu/person/edward-stabler/).

Esta versión simplificada es capaz de parsear gramáticas minimalistas que solo aceptan las operaciones de ensamble externo, ensamble interno y operaciones de licenciamiento con rasgos. 
 
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
7. Copiar alguno de los comandos showParse que están comentados en ``setup.pl`` que correspondan a la gramática elegida y apretar enter. Por ejemplo. Al copiar el comando ``showParse([a,b,c]).``, perteneciente a la gramática ``anbncn`` aparece lo siguiente.
   ```
   ?- showParse([a,b,c]).
   analizando...
   runtime:10.
   chartlength:21.
   
   s:  (A,A,_18538):=A +c T2   
   s:  (A,A,_18538):=T2 +b T1   
   s:  (A,A,_18538):=T1 +a S   
   s:  (A,A,_18518):S   
   s:  (0,1,_18544):=B A -a   
   s:  (0,1,_18554):=B +a A -a   
   s:  (1,2,_18544):=C B -b   
   s:  (1,2,_18554):=C +b B -b   
   s:  (2,3,_18534):C -c   
   s:  (2,3,_18554):=A +c C -c   
   c:  (1,2,_18572):B -b   (2,3,_18576):-c   
   c:  (1,2,_18582):+b B -b   (2,3,_18586):-c   
   c:  (0,1,_18612):A -a   (1,2,_18616):-b   (2,3,_18616):-c   
   c:  (0,1,_18622):+a A -a   (1,2,_18626):-b   (2,3,_18626):-c   
   c:  (A,A,_18656):+c T2   (0,1,_18656):-a   (1,2,_18656):-b   (2,3,_18656):-c   
   c:  (2,3,_18662):+c C -c   (0,1,_18666):-a   (1,2,_18666):-b   (2,3,_18666):-c   
   c:  (2,3,_18610):T2   (0,1,_18614):-a   (1,2,_18614):-b   
   c:  (2,3,_18610):+b T1   (0,1,_18614):-a   (1,2,_18614):-b   
   c:  (1,3,_18566):T1   (0,1,_18570):-a   
   c:  (1,3,_18570):+a S   (0,1,_18574):-a   
   c:  (0,3,_18522):S   
   
   aceptada como categoría S: a b c
   derivation:[1, 2, 3, 4, 6, 8].
   display (return, or h for help)? 
   ```
8. Si la oración es aceptada, introducir h y apretar enter. Va a aparecer una serie de opciones para visualizar los distintos árboles.

{% include copybutton.html %}

{% include additional_content.html %}
