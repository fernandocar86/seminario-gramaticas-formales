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

## Primeros pasos

<div style="text-align:center">
    <img src="git-basics-images/git-init.jpg" width="50%">
</div>

### Clonar un repositorio

- clone
### Armar un repositorio

- init
- gitignore
- remote
- config
### Actualizar un repositorio

- add
- commit
- push

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

- pull
- diff

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

## Referencias

- [Pro Git Book (versión en español)](https://git-scm.com/book/en/v2)

## Cheat Sheet

<div style="text-align:center">
    <a href="git-cheat-sheet.pdf" target="_blank">
        <img src="git-cheat-sheet.png" alt="Git Cheat Sheet" width="50%">
    </a>
</div>

{% include copybutton.html %}

{% include additional_content.html %}
