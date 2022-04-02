# Guía para un uso básico de Git

## Motivación

Si alguna vez hemos abordado la escritura de un trabajo medianamente extenso al que hemos necesitado ir haciéndole correcciones, probablemente nos haya pasado de terminar teniendo múltiples archivos que refieren a un momento particular de ese proceso de escritura, cada uno con una con alguna modificación o comentario específico.

Esto resulta inconveniente por varios motivos:

- puede que no recordemos cuál es el orden de los archivos (qué versión va primero, cuál después)
- quizá perdamos algún cambio o corrección importante
- tal vez una misma línea fue modificada en más de un archivo
- si solo estamos guardando nuestros cambios en nuestra computadora y algo le sucede, perdemos el trabajo realizado
- si además estamos guardando los archivos en alguna nube, puede que no tengamos una forma sencilla de verificar que la versión en la nube está alineada con la que tenemos localmente

Git es un controlador de versiones que nos puede ayudar a resolver todo esto.

<div style="text-align:center">
    <img src="git-basics-images/git-motivation.jpg" width="60%">
    <br>
    <em>
        Meme generado con 
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

## Control de versiones

Un controlador de versiones es un sistema que permite crear, modificar y eliminar archivos registrando cada uno de esos cambios en un historial, de manera tal que luego podamos revertir las modificaciones realizadas, comparar distintas versiones del archivo en el que estamos trabajando y elegir sobre cuál de ellas queremos continuar trabajando.

Esto es posible porque cada versión de un archivo es registrada como una foto en la que se encontraba ese proyecto en un momento del tiempo en particular.

Existen tres tipos de sistemas de control de cambios (VCS, por sus siglas en inglés):

- **Control de versiones local:** guarda copias de las versiones de los archivos de manera local, en nuestra computadora. Desventaja: si la computadora se rompe o algo le sucede, perdemos nuestro trabajo.
- **Control de versiones centralizado (CVCS):** guarda copias de las versiones de los archivos en un servidor remoto. Las personas que están trabajando en un mismo proyecto pueden conectarse a dicho servidor, acceder a la versión que desean y hacer sus modificaciones. Desventaja: si por alguna razón el servidor se cae o no tenemos acceso a él, no podremos trabajar.
- **Control de versiones distribuido (DVCS):** las copias de los archivos y su historial de cambios son guardados en un servidor remoto y, a su vez, cada integrante del equipo puede tener una copia de estos guardada localmente.

## Git y Servidores

### ¿Qué es git?

Git es un **control de versiones distribuido** creado por [Linus Torvalds](https://www.cs.helsinki.fi/u/torvalds/) en 2005. 

Al utilizarlo para gestionar proyectos, las personas que trabajan en equipo (y también las que trabajan de forma individual) pueden almacenar sus proyectos de manera remota (en la nube) y, a su vez, tener una copia local en la que trabajar.

Guardar la información de forma remota posibilita que otras personas accedan a dicha información y se la descarguen, en caso de tener los permisos. A su vez, es en ese repositorio remoto donde cada miembro del equipo irá subiendo sus cambios para unificarlos con los del resto y, en caso de que algo le suceda a nuestra computadora, podemos tener la tranquilidad de que toda la información estára respaldada allí.

### ¿Y GitHub?

GitHub es un servidor remoto al cual se conecta nuestra computadora cada vez que subimos (pusheamos) o bajamos (clonamos o pulleamos) información, entre otras cosas. Así como existe [GitHub](https://github.com/), existen otros servidores: [GitLab](https://about.gitlab.com/), [Bitbucket](https://bitbucket.org/product/), [SourceForge](https://sourceforge.net/), etc.

<div style="text-align:center">
    <img src="git-basics-images/git-vs-github.webp" width="70%">
    <br>
    <em>
        Imagen tomada de
        <a href=https://blog.devmountain.com/git-vs-github-whats-the-difference>
        Devmountain
        </a>
    </em>
</div>

Git me brinda una serie de comandos que me permiten interactuar con el servidor en donde alojaré mis arcvhivos, pero el servidor que elija para esto es indistinto (por supuesto, siempre que sea un servidor compatible con git).

<div style="text-align:center">
    <img src="git-basics-images/git-servers.jpg" width="50%">
    <br>
    <em>
        Meme generado con
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

## Iniciar un repositorio

Existen dos maneras de iniciar un repositorio:

- crearlo desde el servidor y _clonarlo_
- inicializar un repositorio de forma local e indicarle cuál será el servidor para _backupear_ los archivos

<div style="text-align:center">
    <img src="git-basics-images/git-init.jpg" width="50%">
</div>

### Crear un repo en el servidor y clonarlo

Crear un repositorio es muy sencillo. Simplemente debemos ir a nuestra cuenta en [GitHub](https://github.com/) y cliquer a en el botón _New repo_ que aparecerá a la izquierda, debajo de nuestro usuario:

<div style="text-align:center">
    <img src="git-basics-images/git-new-repo.png" width="50%">
</div>

Eso nos llevará a una página que nos pedirá indicar el nombre del repositorio y, si lo deseamos, también una descripción. Podemos elegir si queremos que cualquier persona pueda ver nuestro repo (opción _Public_) o si queremos darle cierta privacidad (opción _Private_). Adicionalmente, podemos agregarle algún contenido determinado (para generar un repo vacío, dejar todas estas opciones sin marcar):
- un archivo README.md básico (tendrá solo el nombre del repositorio y luego podremos modificarlo)
- un `.gitignore` (en la sección x veremos cuál es su utilidad)
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

Supongamos que en nuestra computadora tenemos una carpeta con una serie de archivos y que queremos usar git como controlador de versiones para sus cambios. En ese caso, lo que debemos hacer es ir hasta esa carpeta y abrirla en una terminal. Una vez allí, escribimos:

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

Del mismo modo, tenemos indicarle a git quiénes somos y cuál es nuestro correo electrónico. Esto puede hacerse con los siguientes comandos:

```
git config --local user.name "nombre"
git config --local user.email "email"
```        

El flag `--local` indica que esa configuración solo es válida para el repositorio que se está utilizando en ese momento. Otros repositorios en nuestra computadora pueden tener otra configuración. En caso de querer utilizar el mismo nombre y correo electrónico en todos los repositorios que se tengan en la computadora, se debe cambiar el flag `--local` por `--global`.

**Aclaración:** No es necesario que el correo sea el mismo que está registrado en nuestro repositorio en la nube, ni que nuestro nombre sea el mismo que indicamos allí. Esta información solo es necesaria por cuestiones protocolares: cada vez que hacemos un commit, git indica el nombre y el correo de quien a fin de que, si alguien más lo necesita, pueda ponerse en contacto.

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

Una vez que tenemos nuestro repositorio, podemos agregar los archivos contenidos en nuestra carpeta. Para esto, usamos el comando `add`:

```
git add <file-path>                 # agrega el archvio <file-path>

git add <file-path-1> <file-path-2> # agrega los archivos <file-path-1>
                                    # y <file-path-2>

git add .                           # agrega todos los archivos que estén
                                    # en el repo DENTRO de la ubicación
                                    # del usuario
```
    
Una vez preparados los arhcivos, es necesario confirmarlos. Para esto, podemos usar alguno de los siguientes comandos:
 
```
git commit                          # abre un editor en consola para poder
                                    # escribir el mensaje de confirmación

git commit -m "Primer commit"       # confirma con el mensaje que le sigue
                                    # al flag -m
```

El paso anterior dejó los archivos en nuestro repositorio local, pero todavía no están subidos al remoto (¡todavía podemos perderlos si algo le sucede a nuestra computadora!). Para subirlos al remoto debemos ejecutar los siguientes comandos:

```
git branch -M main                  # cambia el nombre de la rama
                                    # por defecto: master

git push origin main                # sube los archivos al remoto
                                    # en la url de origin
                                    # bajo la rama main
```

No siempre es necesario incluir la información del remoto y la rama. Podemos configurar el repositorio y la rama con la que se sincroniza el repositorio local utilizando flag al pushear:

```       
git push --set-upstream origin <remote-branch>
```
    
Si ahora volvemos a la página donde se encuentra nuestro repositorio remoto y le damos _refresh_, veremos que tiene los archivos que hemos subido.


<div style="text-align:center">
    <img src="git-basics-images/git-not-updated.jpg" width="70%">
    <br>
    <em>
        La foto a partir de la cual se generó este
        <br>
        meme fue encontrada en
        <a href=https://www.1news.co.nz/2022/03/28/will-smith-chris-rock-oscars-meme-not-appropriate-luxon>
        1news
        </a>
    </em>
</div>

### Actualizar el repo local

Hasta ahora solo nos hemos ocupado de crear un repositorio (o descargarlo si ya existía previamente) y subir los archivos disponibles en nuestra computadora. Cuando hacemos esto, le indicamos a git que queremos que _trackee_ esos archivos: que los tenga presentes y que, si hacemos alguna modificación, nos permita verla, subirla o deshacerla y volver al archivo como estaba en otro momento.

Entonces, cuando realizamos algún cambio en alguno de los archivos que git está siguiendo, podemos ver no solo qué archivo se modificó sino también cuáles fueron las modificaciones realizadas:

```
git diff                # muestra las diferencias en
                        # archivos modificados

git diff <file-path>    # muestra las diferencias en el
                        # archivo <file-path>

git diff --staged       # muestra las diferencias en
                        # archivos que se agregaron (add)
```

- pull

<div style="text-align:center">
    <img src="git-basics-images/git-fire.jpg" width="40%">
    <br>
    <em>
        Imagen tomada de
        <a href=https://medium.com/@lulu.ilmaknun.q/kompilasi-meme-git-e2fe49c6e33e>
        Lulu Ilmaknun Qurotaini
        </a>
    </em>
</div>

## Forkear un repo

<div style="text-align:center">
    <img src="git-basics-images/git-fork.jpg" width="50%">
    <br>
    <em>
        Meme generado con
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

## Deshacer cambios

<div style="text-align:center">
    <img src="git-basics-images/git-reset-changes.jpg" width="50%">
    <br>
    <em>
        Meme generado con
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

- restore
- reset ~HEAD
- reset
- revert
## Ramas

<div style="text-align:center">
    <img src="git-basics-images/git-branch.jpg" width="50%">
</div>

- checkout -b
- branch --list
- branch -D
- push --delete origin

<div style="text-align:center">
    <img src="git-basics-images/git-branch-everywhere.jpg" width="60%">
    <br>
    <em>
        Meme tomado de
        <a href=https://medium.com/droidsonroids/android-studio-and-git-branches-how-to-simplify-your-work-698aee7c38dc>
        Łukasz Kopociński
        </a>
    </em>
</div>

## Merge

<div style="text-align:center">
    <img src="git-basics-images/git-merge.jpg" width="50%">
    <br>
    <em>
        Meme generado con
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

- merge

### Resolución de conflictos

<div style="text-align:center">
    <img src="git-basics-images/git-conflicts.jpg" width="70%">
    <br>
    <em>
        Meme generado con 
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

- cómo resolver conflictos
## Pull Requests

<div style="text-align:center">
    <img src="git-basics-images/git-pr-merge-relation.jpg" width="60%">
    <br>
    <em>
        Meme tomado de
        <a href=https://imgflip.com/memegenerator>
        Meme Generator
        </a>
    </em>
</div>

- qué son

## [HINT] Flags útiles

### clone

```
git clone <url> <folder-name>       # permite clonar el repo a una
                                    # carpeta con el nombre que
                                    # indiquemos en <folder-name>
```

### add

```
git add -u                          # agrega todos los archivos que se
                                    # hayan modificado

git add -A                          # agrega todos los archivos que estén
                                    # en el repo sin importar dónde está
                                    # ubicado el usuario
```

### commit

```
git commit --amend -m "Mensaje nuevo"   # permite modificar el mensaje del
                                        # último commit no pusheado
```

### diff

```
git diff <remote-repo>/<remote-branch>..<local-branch>  # muestra las diferencias entre los archivos
                                                        # que fueron confirmados y los que están
                                                        # en el remoto

git diff <commit-hash> <file-path>                      # muestra las diferencias entre el archivo
                                                        # indicado en <file-path>
                                                        # que se
                                                # que está en el commit indicado con el <commit-hash>
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
