# Guía para un uso básico de Git

## Motivación

Si alguna vez hemos abordado la escritura de un trabajo medianamente extenso al que hemos necesitado ir haciéndole correcciones, probablemente nos haya pasado de terminar teniendo múltiples archivos que refieren a un momento particular de ese proceso de escritura, cada uno con una con alguna modificación o comentario específico.

Esto resulta inconveniente por varios motivos:

- puede que no recordemos cuál es el orden de los archivos (qué versión va primero, cuál después)
- quizá perdamos algún cambio o corrección importante
- tal vez una misma línea fue modificada en más de un archivo
- si solo estamos guardando nuestros cambios en nuestra computadora y algo le sucede, perdemos el trabajo realizado
- si estamos guardando los archivos en alguna nube, puede que no tengamos una forma sencilla de verificar que la versión en la nube está alineada con la que tenemos localmente

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

¿Qué es?
¿Cómo funciona?
¿Qué tipos hay?

## Servidores

Volver a generar meme.

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

Git vs. GitHub

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

<div style="text-align:center">
    <a href="git-cheat-sheet.pdf" target="_blank">
        <img src="git-cheat-sheet.png" alt="Git Cheat Sheet" width="50%">
    </a>
</div>

{% include copybutton.html %}

{% include additional_content.html %}
