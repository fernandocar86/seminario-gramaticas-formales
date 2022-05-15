# Guía para un uso básico de Git

## Motivación

Si alguna vez hemos abordado la escritura de un trabajo medianamente extenso al que hemos necesitado ir haciéndole correcciones, probablemente nos haya pasado de terminar teniendo múltiples archivos que refieren a un momento particular de ese proceso de escritura, cada uno con alguna modificación o comentario específico.

Esto resulta inconveniente por varios motivos:

- puede que no recordemos cuál es el orden de los archivos (qué versión va primero, cuál después)
- quizá perdamos algún cambio o corrección importante
- tal vez una misma línea fue modificada en más de un archivo
- si solo estamos guardando nuestros cambios en nuestra computadora y algo le sucede, perdemos el trabajo realizado
- si además estamos guardando los archivos en alguna nube, puede que no tengamos una forma sencilla de verificar que la versión en la nube está alineada con la que tenemos localmente

Git es un controlador de versiones que nos puede ayudar a resolver todo esto.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-motivation.jpg" width="60%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a></figcaption>
    </figure>
</div>

## Control de versiones

Un controlador de versiones es un sistema que permite crear, modificar y eliminar archivos registrando cada uno de esos cambios en un historial, de manera tal que luego podamos revertir las modificaciones realizadas, comparar distintas versiones del archivo en el que estamos trabajando y elegir sobre cuál de ellas queremos continuar trabajando.

Esto es posible porque cada versión de un archivo es registrada como una foto de cómo se encontraba ese proyecto en un momento del tiempo en particular.

Existen tres tipos de sistemas de control de cambios (VCS, por sus siglas en inglés):

- **Control de versiones local:** guarda copias de las versiones de los archivos de manera local, en nuestra computadora. Desventaja: si la computadora se rompe o algo le sucede, perdemos nuestro trabajo.
- **Control de versiones centralizado (CVCS):** guarda copias de las versiones de los archivos en un servidor remoto. Las personas que están trabajando en un mismo proyecto pueden conectarse a dicho servidor, acceder a la versión que desean y hacer sus modificaciones. Desventaja: si por alguna razón el servidor se cae o no tenemos acceso a él, no podremos trabajar.
- **Control de versiones distribuido (DVCS):** las copias de los archivos y su historial de cambios son guardados en un servidor remoto y, a su vez, cada integrante del equipo puede tener una copia de estos guardada localmente.

## ¿Qué es Git?

Git es un **control de versiones distribuido** creado por [Linus Torvalds](https://www.cs.helsinki.fi/u/torvalds/) en 2005. 

Al utilizarlo para gestionar proyectos, las personas que trabajan en equipo (y también las que trabajan de forma individual) pueden almacenar sus proyectos de manera remota (en la nube) y, a su vez, tener una copia local en la que trabajar.

Guardar la información de forma remota posibilita que otras personas accedan a dicha información y se la descarguen, en caso de tener los permisos. A su vez, es en ese repositorio remoto donde cada miembro del equipo irá subiendo sus cambios para unificarlos con los del resto y, en caso de que algo le suceda a nuestra computadora, podemos tener la tranquilidad de que toda la información estará respaldada allí.

## ¿Qué es GitHub?

GitHub es un servidor remoto al cual se conecta nuestra computadora cada vez que subimos (pusheamos) o bajamos (clonamos o pulleamos) información, entre otras cosas. Así como existe [GitHub](https://github.com/), existen otros servidores: [GitLab](https://about.gitlab.com/), [Bitbucket](https://bitbucket.org/product/), [SourceForge](https://sourceforge.net/), etc.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-vs-github.webp" width="70%">
        <figcaption>Imagen tomada de <a href="https://blog.devmountain.com/git-vs-github-whats-the-difference">Devmountain</a></figcaption>
    </figure>
</div>

Git me brinda una serie de comandos que me permiten interactuar con el servidor en donde alojaré mis archivos, pero el servidor que elija para esto es indistinto (por supuesto, siempre que sea un servidor compatible con Git).

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-servers.jpg" width="50%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a></figcaption>
    </figure>
</div>

## ¿Cómo funciona Git?

En Git, la información relacionada a nuestros archivos es manejada como una _copia instantánea_ de estos. Cada vez que se realiza un cambio en un proyecto, "se toma una foto" del estado de cada archivo en ese proyecto y se guarda la referencia. Para ser más eficiente, el sistema solo guarda nuevamente los archivos que se han modificado respecto de la última versión. Si un archivo no fue modificado, el sistema conserva la vieja versión que ya poseía almacenada.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-checkins-over-time.webp" width="70%">
        <figcaption>Imagen tomada de <a href="https://blog.softtek.com/es/git-c%C3%B3mo-poner-orden-en-el-caos">Softtek</a>
        </figcaption>
    </figure>
</div>

## Los estados de Git

Git tiene tres estados principales en los que se pueden encontrar los archivos: 

- modificado (*modified*): significa que el archivo ha sido modificado pero todavía no se encuentra preparado para su confirmación, no ha sido añadido al índice
- preparado (*staged*): significa que un archivo modificado ha sido marcado para que ser almacenado en la próxima confirmación
- confirmado (*committed*): significa que el archivo ha sido confirmado y, la próxima vez que se realice un push, se subirá al remoto. Aquí los datos están almacenados de manera segura en la base de datos LOCAL, pero todavía no se encuentran en la copia remota del repositorio (alojada en el servidor que se haya elegido).

De este modo, un proyecto de Git cuenta con tres secciones principales: 

- el directorio de Git (*git directory*): donde se almacenan los metadatos y la base de datos de objetos para nuestro proyecto. Es la parte más importante de Git, y es lo que se copia cuando clonamos un repositorio desde la nube a nuestra computadora.

- el directorio de trabajo (*working directory*): copia de una versión del proyecto. Estos archivos se sacan de la base de datos comprimida en el directorio de Git, y se colocan en el disco para que los podamos usar o modificar.

- el área de preparación (*staging area*): es un archivo, generalmente contenido en nuestro directorio de Git, que almacena información acerca de lo que va a ir en nuestra próxima confirmación.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-workflow.png" width="70%">
        <figcaption>Imagen tomada de <a href="https://medium.com/@techblurbs/git-getting-started-with-version-control-16a682736933">TechBlurbs</a>
        </figcaption>
    </figure>
</div>


El **flujo de trabajo** básico en Git es algo así:

- En nuestro directorio de trabajo tenemos una serie de archivos a los que les realizamos modificaciones. En esta instancia los archivos se encuentran **modificados**.

- Luego, los preparamos y los añadimos al área de preparación. Una vez hecho esto, los archivos están preparados o **staged**.

- Finalmente, confirmamos los cambios. Este paso toma los archivos tal y como están en el área de preparación y almacena esa copia instantánea de manera permanente en nuestro directorio de Git. Aquí se dice que los archivos fueron confirmados o **commiteados**.

Hasta aquí hemos trabajado en forma local. Si bien hemos hecho cambios en nuestros archivos y los hemos confirmado, si vamos a nuestra página de de GitHub (o a la página del servidor que hayamos elegido para gestionar nuestros archivos), no veremos tales cambios en el remoto. Por el momento, nuestros últimos cambios se encuentran solamenten en nuestra computadora. Para subir estos cambios al remoto, lo que debemos hacer es subir los archivos al repo en la nube, esto es, pushearlos.
## Iniciar un repositorio

Existen dos maneras de iniciar un repositorio:

- crearlo desde el servidor y _clonarlo_
- inicializar un repositorio de forma local e indicarle cuál será el servidor para _backupear_ los archivos

<div style="text-align:center">
    <img src="git-basics-images/git-init.jpg" width="50%">
</div>

### Crear un repo en el servidor y clonarlo

Crear un repositorio es muy sencillo. Simplemente debemos ir a nuestra cuenta en [GitHub](https://github.com/) y cliquear a en el botón _New repo_ que aparecerá a la izquierda, debajo de nuestro usuario:

<div style="text-align:center">
    <img src="git-basics-images/git-new-repo.png" width="50%">
</div>

Eso nos llevará a una página que nos pedirá indicar el nombre del repositorio y, si lo deseamos, también una descripción. Podemos elegir si queremos que cualquier persona pueda ver nuestro repo (opción _Public_) o si queremos darle cierta privacidad (opción _Private_). Adicionalmente, podemos agregarle algún contenido determinado (para generar un repo vacío, dejar todas estas opciones sin marcar):
- un archivo README.md básico (tendrá solo el nombre del repositorio y luego podremos modificarlo)
- un `.gitignore` (en la sección [Ignorar archivos](#ignorar-archivos) veremos cuál es su utilidad)
- una licencia

<div style="text-align:center">
    <img src="git-basics-images/git-new-repo-settings.png" width="50%">
</div>

Una vez creado el repositorio podemos dercargarlo o, más propiamente dicho, *clonarnos*. Para ello, debemos copiar la url que aparece al cliquear el botón _Code_:

<div style="text-align:center">
    <img src="git-basics-images/git-clone.png" width="60%">
</div>

Luego, abrimos la terminal y, en la carpeta donde deseamos descargar el repositorio, escribimos:

```
git clone <url>
```

Esto creará un directorio dentro de la carpeta con el mismo nombre que el repositorio clonado y con todos los subdirectorios necesarios para poder realizar el control de versiones de los archivos.

Con este proceso se puede descargar cualquier repositorio existente (y al cual tengamos acceso y permisos suficientes), ya sea un repositorio que acabamos de crear o uno que tiene varios archivos y contenidos.

### Inicializar un repo local

Supongamos que en nuestra computadora tenemos una carpeta con una serie de archivos y que queremos usar Git como controlador de versiones para sus cambios. En ese caso, lo que debemos hacer es ir hasta esa carpeta y abrirla en una terminal. Una vez allí, escribimos:

```
git init
```

Este comando lo que hará es crear un subdirectoio _.git_, dentro de nuestra carpeta. Allí, se ubicarán todos los archivos necesarios del repositorio.

De todos modos, el comando `init` solo inicializa la carpeta con los archivos que permitan armar la estructura necesaria para gestionar un repositorio, pero no le indica ninguna información en particular. Para cotejar esto, basta con abrir el archivo _config_ ubicado dentro de _.git_ y ver que se encuentra vacío de contenido. Nosotros debemos configurarlo.

En primer lugar, debemos indicar cuál será nuestro almacenamiento remoto. Para esto, debemos disponer de un repositorio (para simplificar las cosas, vacío). Si no tenemos uno, podemos crearlo como se indicó en el [apartado anterior](#crear-un-repo-en-el-servidor-y-clonarlo). Debemos copiar su url desde el botón _Code_ y con el siguiente comando, agregar esa información a nuestro repositorio local:

```
git remote add origin <url>
```

Si mientras hacemos esto vamos mirando el archivo _config_, podremos ver cómo se va agregando la información.

Del mismo modo, tenemos que indicarle a Git quiénes somos y cuál es nuestro correo electrónico. Esto puede hacerse con los siguientes comandos:

```
git config --local user.name <nombre>
git config --local user.email <email>
```        

El flag `--local` indica que esa configuración solo es válida para el repositorio que se está utilizando en ese momento. Otros repositorios en nuestra computadora pueden tener otra configuración. En caso de querer utilizar el mismo nombre y correo electrónico en todos los repositorios que se tengan en la computadora, se debe cambiar el flag `--local` por `--global`.

**Aclaración:** No es necesario que el correo sea el mismo que está registrado en nuestro repositorio en la nube, ni que nuestro nombre sea el mismo que indicamos allí. Esta información solo es necesaria por cuestiones protocolares: cada vez que hacemos un commit, Git indica el nombre y el correo de quien hizo los cambios a fin de que, si alguien más lo necesita, pueda ponerse en contacto.

### [HINT] Configuración

Si bien podemos leer el archivo _config_ para ver cómo está configurado nuestro repositorio, dado que `.git` ofrece la posibilidad de acceder a esta información desde consola, es preferible realizar tal consulta mediante comandos. Algunos útiles son:

```
git config --list           # muestra toda la información
                            # disponible en config
        
git config --get user.name  # muestra el nombre de usuario
                            # puede cambiarse user.name por
                            # user.email y obtener el correo
                                       
git remote -vv              # muestra la url del remoto
                            # y su nombre asociado
```

Acceder a la configuración de este modo y no abriendo el archivo _config_ resulta más seguro. Podría ocurrir que, al abrir el archivo, sobreescribamos o modifiquemos de algún modo la información allí almacenada y que eso nos traiga problemas para el uso del repositorio. Además, una vez que nos acostumbramos al uso de comandos, se vuelve un poco más práctico.
### Subir archivos al remoto

Una vez que tenemos nuestro repositorio, podemos ejecutar el comando `status`. Este comando es sumamente útil porque podemos usarlo en todo momento para ver en qué estado están nuestros archivos.

```
git status
```

En esta instancia, en la que tenemos un archivo nuevo que Git desconoce, nos mostrará algo como lo siguiente:

```
On branch main
Your branch is up to date with 'origin/main'.

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	new_file.txt

nothing added to commit but untracked files present (use "git add" to track)
```

Para agregar los archivos contenidos en nuestra carpeta, usamos el comando `add`:

```
git add <file-path>                 # agrega el archvio <file-path>

git add <file-path-1> <file-path-2> # agrega los archivos <file-path-1>
                                    # y <file-path-2>

git add .                           # agrega todos los archivos que estén
                                    # en el repo DENTRO de la ubicación
                                    # en la que se encuentra el usuario
```

Si deseamos verificar que nuestros archivos se hayan agregado correctamente, podemos volver a ejecutar `status`. Ahora nos dirá que tenemos un nuevo archivo y que podemos confirmarlo:

```
On branch main
Your branch is up to date with 'origin/main'.

Changes to be committed:
  (use "git restore --staged <file>..." to unstage)
	new file:   new_file.txt
```

Para esto último, podemos usar alguno de los siguientes comandos:
 
```
git commit                      # abre un editor en consola para poder
                                # escribir el mensaje de confirmación

git commit -m "Primer commit"   # confirma con el mensaje que le sigue
                                # al flag -m
```

Luego de ejecutar alguno de los anteriores comandos, la consola nos mostrará un mensaje similar a este:

```
[main 0fcc82b] agrego nuevo archivo
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 new_file.txt
```

Si ahora volvemos a correr `status`, veremos que la consola nos indica que estamos adelantados a la historia del remoto.

```
On branch main
Your branch is ahead of 'origin/main' by 1 commit.
  (use "git push" to publish your local commits)

nothing to commit, working tree clean
```

Esto sucede porque el `commit` confirmó los archivos que queremos subir, pero no los subió al remoto (¡todavía podemos perderlos si algo le sucede a nuestra computadora!). Para subirlos debemos ejecutar los siguientes comandos:

```
git branch -M main                  # cambia el nombre de la rama
                                    # por defecto: master

git push origin main                # sube los archivos al remoto en la
                                    # url de origin bajo la rama main
```

No siempre es necesario incluir la información del remoto y la rama. Podemos configurar el repositorio y la rama con la que se sincroniza el repositorio local utilizando un flag al pushear:

```       
git push --set-upstream origin <remote-branch>
```

Con esto, ya no será necesario incluir el remoto y la rama cada vez (para esta rama, en otras ramas sí deberemos hacerlo a menos que también realicemos esta configuración).
Si ahora volvemos a la página donde se encuentra nuestro repositorio remoto y le damos _refresh_, veremos que tiene los archivos que hemos subido.


<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fire.jpg" width="40%">
        <figcaption>Imagen tomada de <a href="https://medium.com/@lulu.ilmaknun.q/kompilasi-meme-git-e2fe49c6e33e">Lulu Ilmaknun Qurotaini</a>
        </figcaption>
    </figure>
</div>

### Ignorar archivos

Si en nuestra carpeta o directorio tenemos archivos que no queremos que Git rastree cada vez que hacemos un cambio, lo que debemos hacer es agregar un archivo llamado `.gitignore` a la misma altura que se encuentra el directorio `.git` y allí listar los paths de archivos que se deseen ignorar, ya sea por nombre o por extensión (ej: *.txt).

Por ejemplo, si queremos que se ignoren las carpetas `data/`, el archivo `config/keys.json` y todos los archivo `.csv` contenidos en la carpeta `result/`, nuestro `.gitignore` debería verse del siguiente modo:

```
# ignora data/
data/

# ignora config/keys.json
config/keys.json

# ignora los .csv contenidos en results/
results/*.csv
```

El caracter `#` nos permite agregar comentarios que no serán tenidos en cuenta por Git. Estos nos posibilita dejar indicaciones más claras para la comprensión del archivo.

### Control de diferencias

Hasta ahora solo nos hemos ocupado de crear un repositorio (o descargarlo si ya existía previamente) y subir los archivos disponibles en nuestra computadora. Cuando hacemos esto, le indicamos a Git que queremos que _trackee_ esos archivos: que los tenga presentes y que, si hacemos alguna modificación, nos permita verla, subirla o deshacerla y volver al archivo como estaba en otro momento.

Entonces, cuando realizamos algún cambio en alguno de los archivos que Git está siguiendo, podemos ver no solo qué archivo se modificó sino también cuáles fueron las modificaciones realizadas:

```
git diff                # muestra las diferencias en archivos
                        # modificados y la última confirmación

git diff <file-path>    # muestra las diferencias en el archivo
                        # <file-path> y la última confirmación

git diff --staged       # muestra las diferencias en archivos
                        # que se agregaron (add) y la última
                        # confirmación
```

Una alcaración importante es que Git no puede hacer esto con cualquier tipo de archivos. Con archivos de texto plano (como los que generamos cuando usamos algún programa tipo Notepad o como los srcipts de código) no tendremos problemas. Pero frente a archivos de tipo binario (como los documentos de _Word_), Git solo podrá decirnos si el archivo cambió o no, pero no podrá especificarnos qué líneas ni caracteres. ¿Significa esto que no podemos subir un archivo `.docx` a GitHub? No necesariamente. Podemos subirlo y utilizar Git para recuperar sus distintas versiones, pero deberemos prescindir de una gran parte de las facilidades que ofrece la herramienta.
### Actualizar el repo local

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-not-updated.jpg" width="70%">
        <figcaption>La foto a partir de la cual se generó este meme fue encontrada en <a href="https://www.1news.co.nz/2022/03/28/will-smith-chris-rock-oscars-meme-not-appropriate-luxon">1news</a>
        </figcaption>
    </figure>
</div>

Cada nuevo commit que hacemos es una especie de foto que guarda Git de nuestros archivos. Podemos pensar esa secuencia de fotos como una suerte de línea temporal en la que podemos volver hacia atrás si queremos recuperar información que luego fue modificada.

Pero para esto es importante tener nuestro repositorio local, ese sobre el que trabajamos y en el que modificamos nuestros archivos, actualizado. Si no vamos alineando nuestro repositorio local con el remoto y descargándole cada tanto las actualizaciones que otras personas (si trabajamos de forma colaborativa) o nosotros mismos (si tenemos más de una computadora o si editamos algún archivo desde la interfaz web) realizamos, Git podría no dejarnos pushear, debido a que en el remoto hay nuevos cambios que no tenemos de forma local y, por ende, los archivos no se encuentran en su última versión.

Para descargar la última versión de los archivos usamos el siguiente comando:

```
git pull
```
## Forkear un repo

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork.jpg" width="50%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a>
        </figcaption>
    </figure>
</div>

Cuando forkeamos un repositorio, hacemos una bifurcación de un proyecto (i.e. tomamos una foto de ese proyecto en determinado momento del tiempo). De esta manera, podemos copiar el código de un repositorio ajeno, que vive en el servidor de otra persona y guardar dicha copia en un repositorio propio. Esto nos permitirá trabajar en esa copia con la seguridad de que no estamos modificando el código del proyecto original. 

Para forkear un repositorio simplemente debemos clickear en el botón `fork` (usualmente ubicado en la esquina superior derecha de la pantalla).

<div style="text-align:center">
    <img src="git-basics-images/git-fork.png" width="60%">
</div>

Esto hará que se genere una copia idéntica al proyecto forkeado en nuestro servidor. GitHub nos indica esto anteponiendo el usuario al nombre del proyecto. El repositorio ajeno mostrará el nombre del usuario al que pertenece el proyecto, pero nuestra copia de ese proyecto (alojada en nuestro servidor) mostrará nuestro usuario y la fuente desde la cual fue forkeada dicha copia.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-other-repo.png" width="70%">
        <figcaption>Repositorio ajeno</figcaption>
    </figure>
</div>

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-own-repo.png" width="70%">
        <figcaption>Repositorio forkeado</figcaption>
    </figure>
</div>

Una vez que tenemos el proyecto que deseamos forekado en un repositorio propio, podemos clonarlo en nuestra computadora y trabajar sobre él. Esto es útil cuando queremos utilizar código ajeno y realizarle modificaciones, pero no tenemos los permisos para subir dichas modificaciones al repositorio donde se aloja el código. En estos casos, basta con forkear el repositorio en cuestión. Dado que esa copia vive en un repositorio propio, podremos subir nuestras modificaciones sin inconvenientes.

Algo que conviene tener presente en este punto es que la copia forkeada es una foto del estado del repositorio original en el momento en el que la tomamos. Esto significa que, si tomamos la foto un día x nuestra copia tendrá el mismo contenido que tenía el repositorio original hasta ese día. Si el día x+1 el repositorio original fue actualizado con nueva información, nuestra copia estará desactualizada a menos que la actualicemos nosotros mismos. En este caso, GitHub nos mostrará una advertencia como la siguiente:

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-behind.png" width="100%">
    </figure>
</div>


Si estamos usando GitHub como repositorio remoto, existen dos formas de realizar esta actualización: por interfaz o por línea de comandos.


**OPCIÓN #1: por interfaz**

GitHub nos brinda la posibilidad de actualizar la copia forkeada desde su interfaz visual. Para ello, debemos ir a la página de nuestro repositorio forkeado y clickear en el botón *Fetch upstream*.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-update-from-github.png" width="45%">
    </figure>
</div>

Esto nos dará la opción de introducir los cambios (*Fetch and merge*) y también de compararlos previamente (*Compare*), para observar si se producirán conflictos.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-update-from-github-options.png" width="45%">
    </figure>
</div>

**OPCIÓN #2: por línea de comandos**

Si el servidor remoto que estamos utilizando no nos provee de una interfaz con la cual podamos realizar la actualización del repositorio forkeado, siempre podremos hacerlo por consola. Para ello, los pasos a seguir son los siguientes:

1. Si todavía no se hizo, clonar el repositorio forkeado con el comando `git clone`:
    
    ```{bash}
    git clone <url>
    ```

    Aquí, la variabla `<url>` debe cambiarse por la dirección que nos proporciona GitHub para clonar el repositorio (la copia alojada en nuestro servidor).

2.  (OPCIONAL) Verificar las urls para actualizar la versión local del repositorio:

    ```{bash}
    git remote -vv
    ```

    Este comando nos devolverá algo como lo siguiente:

    ```{bash}
    origin	https://github.com/macfernandez/seminario-gramaticas-formales.git (fetch)
    origin	https://github.com/macfernandez/seminario-gramaticas-formales.git (push)
    ```

    Esto significa que el remoto llamado `origin` está asociado con la url indicada, tanto para descargar datos (fetch) como para subirlos (push).

3. Agregar la url del repositorio ajeno, del cual obtuvimos el repo forkeado:

    ```{bash}
    git remote add upstream https://github.com/fernandocar86/seminario-gramaticas-formales.git
    ```

    Este comando agregará a nuestro repo local la configuración de un nuevo remoto llamado `upstream`, el cual estará asociado a la url indicada a continuación en el mismo comando.

4. (OPCIONAL) Si se ejecutó el paso 2 y ahora se vuelve a correr el mismo comando, se verá que el resultado ha cambiado:

    ```{bash}
    origin	https://github.com/macfernandez/seminario-gramaticas-formales.git (fetch)
    origin	https://github.com/macfernandez/seminario-gramaticas-formales.git (push)
    upstream https://github.com/fernandocar86/seminario-gramaticas-formales.git (fetch)
    upstream https://github.com/fernandocar86/seminario-gramaticas-formales.git (push)
    ```

    Esto signfica que ahora nuestro repositorio local tiene configurados dos remotos y podemos elegir desde cuál descargar datos y a cuál subir modificaciones.

5. Actualizar el repositorio local. Para eso, nos moverse a la rama `main` (la cual no debe tener commits agregados por nosotros) y descargar las modificaciones hechas en el repositorio `upstream`:
   
    ```{bash}
    git checkout main
    git pull upstream main:main
    ```

    Si acaso realizamos modificaciones propias en la rama `main`, utilizar los siguientes comandos en lugar de los anteriores:

    ```{bash}
    git checkout main
    git rebase upstream/main
    ```

    Considerar que, si se introdujeron modificaciones propias en `main`, es posible que surjan conflictos como consecuencia de que el repositorio original (aquel desde el cual obtuvimos el repo forkeado) y nuestra copia del mismo tienen historias divergentes. En este caso, habrá que resolverlos como se resuelven las situaciones de conflictos o bien, desahacer los commits que introduzcan cambios propios en `main`.

6. Por último, subir las modificaciones descargadas al remoto propio:

    ```{bash}
    git push
    ```

Una vez realizado alguno de los dos procedimientos, si volvemos a mirar nuestro repositorio remoto en la interfaz de GitHub, veremos un mensaje como el siguiente:

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-fork-uptodate.png" width="100%">
    </figure>
</div>

## Deshacer cambios

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-reset-changes.jpg" width="50%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a>
        </figcaption>
    </figure>
</div>

¿Quá pasa si hacemos un cambio y nos arrepentimos?

En caso de haber hecho cambios pero no haberlos agregado al área de preparación:

```
git restore <file-path>     # deshace cambios realizados en <file-path>
                            # y vuelve el archivo a la versión en la
                            # que se encontraba antes de modificarlo
```

Si hicimos cambios y los agregamos al área de preparación (add):

```
git restore --staged <file-path>    # quita los cambios agregados al
                                    # área de preparación en el archivo
                                    # <file-path>, pero los deja en el
                                    # área de trabajo
```

Si ya confirmamos nuestros cambios (*commit*):

```
git revert <SHA>            # revierte el commit con el SHA indicado 
                            # pueden escribirse solamente los primeros 7
                            # caracteres este comando genera un nuevo
                            # commit que revierte lo modificado en el
                            # indicado (i.e. agrega in información a la
                            # historia de trabajo)
        
git reset --soft HEAD~1     # deshace el último commit hecho pero conserva
                            # los archivos modificados en el área de
                            # staging (este comando modifica la historia
                            # recopilada en Git porque borra el commit)
        
git reset --hard HEAD~1     # ídem reset --soft solo que no conserva los
                            # archivos modificados en el área de staging
```

## [HINT] Rastrear cambios

Puede que a veces nos interese rastrear los cambios dentro de un repositorio. El comando `log` nos permite visualizar los *commits* realizados, qué archivos modificaron y cuáles fueron los cambios realizados, entre otras cosas:

```
git log                 # permite visualizar todos los commits, sus
                        # mensajes de confirmación

git log --stat          # ídem anterior pero muestra además cuáles fueron
                        # los archivos modificados

git log -p <file-path>  # ídem anterior pero muestra además las
                        # modificaciones realizadas

git log --oneline       # ídem log pero muestra los commits en una sola línea

git log <file-path>     # permite visualizar los commits que han modificado
                        # el archivo <file-path>    
```

## Ramas

<div style="text-align:center">
    <img src="git-basics-images/git-branch.jpg" width="50%">
</div>

Cuando hablamos de ramificaciones, nos referimos a que podemos tomar una rama de desarrollo (supongamos, llamada main) y, a partir de ella, generar otra igual en la que seguiremos trabajando, pero cuyos cambios en principio no afectarán a la rama de origen.

Esto es sumamente útil cuando estamos compartiendo nuestro repositorio con otras personas y no queremos que sus cambios interfieran en nuestro trabajo, pero es posible que luego queramos integrar todo, los archivos en nuestra rama con los de las ramas de los demás.

Para Git, una rama es simplemente un apuntador móvil que señala cada una de las confirmaciones que vamos haciendo en nuestro repositorio, lo que significa que señala cada foto que vamos tomando de nuestros archivos.

Cuando creamos un repositorio, la primera rama que se crea por defecto suele ser `master` o `main` y esta es la que se considera la _rama base_, pero esto es configurable y se puede indicar cualquier otra rama en su lugar.

Que una rama sea la _rama base_ implica, entre otras cosas, que cuando alguien clone el repo, la rama en la que se encontrará ni bien se genera la copia local será esta rama.

Con cada confirmación que realicemos, y mientras no nos cambiemos de rama, el apuntador de la rama base (o de la rama en la que nos encontremos) irá avanzando en la historia que registra Git de un archivo.

Para crear una nueva rama, podemos usar el siguiente comando:

```
git checkout -b <branch-name>
```

Este comando creará un nuevo apuntador llamado \<new-branch> que estará basado en la rama desde la cual fue creado. Esto significa que, hasta que hagamos alguna modificación en esta nueva rama o en aquella desde la que partimos, ambas serán iguales. 
Además, al mismo tiempo que crea la rama, el comando nos moverá a ese apuntador. Cualquier cambio que hagamos será seguido por ese apuntador y no por aquel donde nos encontrábamos previamente.

Si, en cambio, solo queremos crear una nueva rama pero no movernos hacia ella, debemos ejecutar el siguiente comando:

```
git branch <branch-name>
```
La nueva rama creada también será idéntica a aquella desde la cual se la creó hasta que le hagamos alguna modificación.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-new-branch.png" width="50%">
        <figcaption>Imagen tomada de <a href="https://aprendeconalf.es/docencia/git/manual/gestion-ramas">Aprender con Alf</a>
        </figcaption>
    </figure>
</div>

Para movernos entre branches usamos el siguiente comando:

```
git checkout <branch-name>
```

Para poder ejecutar este comando es necesario tener el directorio de trabajo de la rama en la que nos encontremos limpio. De lo contrario, Git nos pedirá que subamos nuestros cambios al remoto o los descartemos.

Si queremos borrar una rama en el repositorio local, debemos usar:

```
git branch -D <branch-name>
```

Este comando borrará la rama de mi repo local, pero no afectará la que se encuentra en el remoto.

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-branch-everywhere.jpg" width="60%">
        <figcaption>Meme tomado de <a href="https://medium.com/droidsonroids/android-studio-and-git-branches-how-to-simplify-your-work-698aee7c38dc">Łukasz Kopociński</a>
        </figcaption>
    </figure>
</div>

## Merge

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-merge.jpg" width="50%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a>
        </figcaption>
    </figure>
</div>

Si deseamos fusionar los cambios de una rama en otra, debemos movernos a aquella en la que queremos importar los cambios utilizando el comando `checkout` y llevar los cambios de la rama deseada con el comando `merge`:

```
git checkout <branch-A>                     # nos mueve a la rama que
                                            # recibirá los cambios

git pull                                    # asegura que la rama esté
                                            # actualizada

git fetch <remote> <branch-B>:<branch-B>    # asegura de que la rama cuyos
                                            # cambios queremos introducir
                                            # esté sincronizada con el remoto
                                            # <remote>
        
git merge <branch-B>                        # importa los cambios de la rama
                                            # <branch-B> en aquella en la que
                                            # nos encontramos (<branch-A>)
```

## Pull Requests

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-pr-merge-relation.jpg" width="60%">
        <figcaption>Meme tomado de <a href="https://imgflip.com/memegenerator">Meme Generator</a>
        </figcaption>
    </figure>
</div>

Cuando hacemos un *merge*, una rama A (en la que estamos posicionados) se trae los cambios de otra rama B.

En una *pull request* (en GitHub y Bitbucket) o una *merge request* (en GitLab) es la rama B la que le pide a la rama A que incorpore sus cambios.

Esta acción debe hacerse desde la interfaz de la página del servidor donde se encuentre el remoto.

Para ello, debemos ir a la página del repositorio remoto, a la sección  _Pull requests_ y cliquear en el botón _New pull request_.

<div style="text-align:center">
    <img src="git-basics-images/git-pull-requests.png" width="50%">
</div>


<div style="text-align:center">
    <img src="git-basics-images/git-new-pull-request.png" width="50%">
</div>


Esta no es la única forma de hacer esto. En general, cuando actualizamos una rama, al ingresar al remoto GitHub nos sugiere la posibilidad de comparar ramas y hacer una PR.

**Aclaración:** Tanto el merge como el MR o PR pueden tener conflictos si la rama que se intenta fusionar no tiene (al momento de hacer la fusión) todos los cambios que tiene la rama a la que se quiere fusionar (*i.e.*, debe tener en su historial los *commits* de la rama a la cual se quieren fusionar los cambios). En caso de existir conflictos, se los deberá resolver como se detalla en la [siguiente sección](#resolución-de-conflictos).

### Resolución de conflictos

<div style="text-align:center">
    <figure>
        <img src="git-basics-images/git-conflicts.jpg" width="70%">
        <figcaption>Meme generado con <a href="https://imgflip.com/memegenerator">Meme Generator</a>
        </figcaption>
    </figure>
</div>

Supongamos que estamos trabajando en el mismo repositorio con otras personas y estamos compartiendo nuestra rama de trabajo con alguien más, por lo que dos personas están pusheando a la misma rama.

Supongamos, además, que la otra persona se sincronizó su rama en el mismo momento que nosotros y, por ende, tenemos las mismas versiones de los archivos.

En ese contexto, ambos empezamos a trabajar y a introducir cambios en el repositorio. Sin embargo, la otra persona pushea sus modificaciones antes que nosotros y entonces, cuando queremos subir al remoto las nuestras, la consola nos dice que éste contiene cambios que no tenemos en nuestro repo local (claro, los que acaba de subir la otra persona). 

¿Qué hacer entonces? Hacemos un `pull` y nos descargamos los nuevos cambios.

Si las modificaciones que estaban en el remoto no afectaban a los mismos archivos que nosotros modificamos, no tendremos problemas. Simplemente nos saldrá un mensaje editable (que no es otra cosa que un mensaje de confirmación) indicando que se realiza un *merge* y podremos hacer el `push`.

Si, en cambio, la otra persona modificó un archivo también editado por nosotros, es problable que haya conflictos y tengamos que solucionarlos. En este caso, cuando hagamos el *pull*, nuestra consola mostrará algo como lo siguiente:

```
Auto-merging README.md
CONFLICT (content): Merge conflict in README.md
Automatic merge failed; fix conflicts and then commit the result.
```

Y si pedimos un poco más de detalle con el comando `status`:

```
On branch main
Your branch and 'origin/main' have diverged,
and have 1 and 1 different commits each, respectively.
  (use "git pull" to merge the remote branch into yours)

You have unmerged paths.
  (fix conflicts and run "git commit")
  (use "git merge --abort" to abort the merge)

Unmerged paths:
  (use "git add <file>..." to mark resolution)
	both modified:   README.md

no changes added to commit (use "git add" and/or "git commit -a")
```
        
Todo aquello que sea conflictivo y no se haya podido resolver, se marca como "sin fusionar" (unmerged). Git añade a los archivos conflictivos unos marcadores especiales de resolución de conflictos que nos guiarán cuando los abramos manualmente y los editemos para corregirlos. 

Si abrimos los archivos en un editor de texto plano convencional, veremos algo como lo siguiente:

```
<<<<<<< HEAD (Current Changes)
Mi repositorio en GitHub
=======
Mi primer repositorio en GitHub
>>>>>>> main (Incomming Changes)
```

Aquí se nos marca que la versión en HEAD (nuetra versión del directorio de trabajo) contiene lo indicado en la parte superior del bloque y la versión entrante contiene el resto, lo indicado en la parte inferior del bloque. 

De todos modos, para una visualización más clara, lo mejor suele ser usar un editor de código del tipo de [Visual Studio Code](https://code.visualstudio.com/)(VSC), donde veremos algo como lo siguiente:

<div style="text-align:center">
    <img src="git-basics-images/git-visual-conflicts.png" width="100%">
</div>


Si queremos aceptar los cambios entrantes, simplemente debemos hacer click en el **Accept Incoming Change**.
Si, en cambio, queremos dejar nuestros cambios, debemos elegir **Accept Current Change**.
O podemos aceptar ambo, para ello usamos **Accept Both Changes**.

Esto debemos hacerlo para cada cambio. Pero, si ya sabemos que queremos aceptar todos los cambios entrantes o todos los cambios actuales (los que están en nuestro directorio de trabajo) y estamos usando VSC, podemos utilizar la paleta de comandos. Para ello, apretamos `Ctrl+Shift+P` y escribimos _Accept All_. Esto hará que se nos muestren las opciones para resolver todos los conflictos de fusión de la misma forma (aceptando todos los cambios entrantes, todos los actuales o ambos cambios).

Si no estamos usando algún editor de código, para resolver nuestros conflictos (los de código, al menos) deberemos abrir un editor de texto plano (Text Editor, Notepad o el que sea de nuestra preferencia), arremangarnos y buscar cada ocurrencia de conflicto (podemos buscar _HEAD_). Cada vez que encontremos una, tenemos que borrar las líneas que indican _<<<<<<< HEAD (Current Changes)_, _>>>>>>> \<branch> (Incomming Changes)_, _=======_ y las de la porción de código que queremos desestimar. Solo debe quedar el cambio que queremos (el *current* o el *incomming*). En el ejemplo anterior, deberíamos dejar o bien _Mi repositorio en GitHub_, o bien _Mi primer repositorio en GitHub_

Una vez hecho esto guardamos el archivo y lo agregamos al *staging* (con el ya conocido comando `add`). Y si ejecutamos el comando ```git status```, veremos:

```
On branch main
Your branch and 'origin/main' have diverged,
and have 1 and 1 different commits each, respectively.
  (use "git pull" to merge the remote branch into yours)

All conflicts fixed but you are still merging.
  (use "git commit" to conclude merge)
```
            
Si todo ha salido correctamente, podremos utilizar el comando `commit` para terminar de confirmar la fusión y luego pushear.

### Conflictos en una PR

Cuando realizamos una PR, puede suceder que nos surjan conflictos. En el caso de que los mismos se deban a que estamos compartiendo la rama desde la que hacemos la PR con otra persona, debemos resolverlos del mismo modo que se detalló anteriormente.

Sin embargo, también puede ocurrir que la interfaz de nuestro remoto nos indique que el conflicto se da con la rama en la que queremos introducir nuestros cambios (recordemos que una *pull request* es una solicitud a una rama para que incorpore los cambios de nuestra rama). En este caso, lo que sucede es que la rama que desea introducir cambios y aquella en la cual se los introduce divergen en algún punto de su historia. ¿Qué significa esto? Que la rama que recibirá los cambios tiene algún *commit* con modificaciones que la rama que desea introducir cambios no tiene. 

Para evitar esto, lo mejor es siempre armar nuestra rama de trabajo partiendo de aquella en la que luego querremos introducir nuestros arreglos o mejoras. Por ejemplo, si la rama base de nuestro repo es _main__ y sabemos que nuestros cambios deben fusionarse con ella, partimos de ahí para generar nuestra rama de trabajo. Pongámosle a esta, por nombre, _fix/bug_. 

Ahora bien, una vez que tenemos nuestro trabajo listo, hacemos la PR. Si la rama en la cual queremos introducir los cambios no recibió otras modificaciones en el medio del proceso, probablemente no tengamos mayores iconvenientes. Pero si alguien más introdujo cambios en la rama "target", la rama que solicita la PR no tendrá el mismo historial de cambios y deberemos sincronizar ambas para alinear sus historias.

En este caso, lo que debemos hacer es lo siguiente. En nuestra copia local del repositorio ejecutamos:

```
git checkout <pr-branch>                            # nos posiciona en
                                                    # la rama que tiene
                                                    # nuestro trabajo
    
git fetch <remote> <target-branch>:<target-branch>  # nos asegura que la
                                                    # copia local de la
                                                    # rama a la que hacemos
                                                    # la PR esté sincronizada
                                                    # con el remoto

git merge <target-branch>                           # fusionamos los cambios de
                                                    # la rama target (a la que 
                                                    # hacemos la PR) en la rama
                                                    # de trabajo
```

Siguiendo nuestro ejemplo anterior:

```
git checkout fix/bug123
    
git fetch origin main:main

git merge main
```

Una vez hecho esto, pueden suceder dos cosas:
- En el mundo feliz, el *merge* con la rama *target* se hace sin conflictos y solo nos restará hacer un `push` a nuestra rama de trabajo. Luego de hacerlo, veremos que la PR realizada se actualiza y las advertencias de conflictos en el remoto deberían desaparecer.
- En un mundo menos feliz, Git podría indicarnos que hay conflictos porque en la rama *target* había modificaciones que alteraban los mismos archivos que nosotros cambiamos. En este caso, debemos resolverlos del mismo modo que se indicó [anteriormente](#resolución-de-conflictos). Una vez resueltos, realizar los ya clásicos `add`, `commit` y `push`. Igual que en el mundo feliz, la PR se actualizará (debido a que la rama se actualizó) y deberíamos dejar de ver las advertencias de conflictos.

## [HINT] Flags útiles

### add

```
git add -u                          # agrega todos los archivos que se
                                    # hayan modificado

git add -A                          # agrega todos los archivos que estén
                                    # en el repo sin importar dónde está
                                    # ubicado el usuario

git add -p                          # muestra las modificaciones realizadas
                                    # y permite elegir en cada caso si
                                    # agregar la modificación o no
```

### branch

```
git branch --list       # muestra la lista de ramas
```

### checkout

```
git checkout <commit-hash> -- <file-path>   # vuelve el archivo
                                            # <file-path> a la versión
                                            # confirmada en el commit indicado
                                            # en <commit-hash>

git checkout <branch-name> -- <file-path>   # trae el archivo <file-path> desde
                                            # <branch-name> (sin importar si el
                                            # archivo ya se encuentra en la
                                            # rama a la cual se lo quiere traer
                                            # o no)
```

### clone

```
git clone <url> <folder-name>   # permite clonar el repo a una carpeta
                                # con el nombre que indiquemos en
                                # <folder-name>
```

### commit

```
git commit --amend -m "Mensaje nuevo"   # permite modificar el mensaje
                                        # del último commit no pusheado
```

### diff

```
git diff <remote-repo>/<remote-branch>  # muestra las modificaciones en
                                        # los archivos confirmados que
                                        # todavía no fueron pusheados

git diff <commit-hash> <file-path>      # muestra las diferencias en
                                        # <file-path> entre su versión en el
                                        # commit indicado con el <commit-hash>
                                        # y la versión que se está trabajando
```

### fetch

```
git fetch <remote-name> <branch-name>   # trae los cambios de la rama
                                        # indicada del remoto, pero no
                                        # los descarga al directorio de
                                        # trabajo
        
git fetch --all                         # ídem anterior pero con todas
                                        # las ramas y todos los remotos
                                        # (usarlo con cuidado)
```

Estos comandos nos permiten ver si hay cambios que puedan generar conflictos con los nuestros (porque modifican el mismo archivo, por ejemplo).
Si no hay cambios o si vemos que no resultan conflictivos, lo que debemos hacer es ejecutar los comandos `add` y `commit` de modo que nuestros cambios se guarden en el área de preparación. Una vez hecho esto, nos descargamos los cambios del remoto utilizando el comando `pull` y, finalmente, pusheamos nuestros cambios (solo pullear si hay cambios, si no los hay, luego de commitear, pushear directamente).

### log

```
git log --author=<author-name>  # muestra los commits realizador por
                                # <author-name>
```
### push

```
git push <remote> --delete <branch-nam>     # borra la rama en el remoto
```

## Cheat Sheet

<div style="text-align:center">
    <a href="git-cheat-sheet.pdf" target="_blank">
        <img src="git-basics-images/git-cheat-sheet.png" alt="Git Cheat Sheet" width="50%">
    </a>
</div>

## Referencias

- [Pro Git Book (versión en español)](https://git-scm.com/book/en/v2)


{% include copybutton.html %}

{% include additional_content.html %}
