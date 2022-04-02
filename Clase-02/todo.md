git status

---------------------------------------------

git ignore

Si en nuestra carpeta o directorio tenemos archivo que no queremos que Git rastree cada vez que hacemos un cambio, lo que debemos hacer es agregar un archivo llamado _.gitignore_ a la misma altura que se encuentra el archivo _.git_ y allí listar los paths de archivos que se deseen ignorar, ya sea por nombre o por extensión (ej: *.txt).

Por ejemplo, si queremos que se ignoren las carpetas `data/`, el archivo `config/keys.json` y todos los archivo `.csv` contenidos en la carpeta `result/`, nuestro `.gitignore` debería verse del siguiente modo:

```{txt}
# ignora data/
data/

# ignora config/keys.json
config/keys.json

# ignora los .csv contenidos en results/
results/*.csv
```

El caracter `#` nos permite agregar comentarios que no serán tenidos en cuenta por git. Estos nos permiten dejar indicaciones más claras para la comprensión del archivo.

**¿Y qué pasa si no me di cuenta de hacer esto cuando creé el repositorio?**

Si Git ya estaba haciendo un seguimiento de un archivo y queremos dejar de seguirlo pero no queremos borrar el archivo de nuestro directorio, lo que se debe hacer es ejecutar el siguiente comando:

        $ git rm --cached <file-path>
        
Esto quitará nuestro archivo del listado de seguimiento sin eliminarlo de nuestra carpeta local. Si no se agrega el flag --cached, Git removerá el archivo de manera --force: lo quitará del listado de seguimiento y lo eliminará de nuestra computadora (similar a hacer ```rm <file-path>``` solo que tampoco lo encontraremos en la papelera).

Una vez ejecutado este comando, git nos mostrará que el archivo que se ha quitado del rastreo fue eliminado, pero también nos mostrará que el mismo archivo se encuentra _unstaged_ (sin seguimiento). Esto ocurre porque el archivo aún existe en nuestro directorio de trabajo y, si bien git recibió la orden de olvidar que debía rastrearlo, todavía no se le indicó que a futuro también se lo quiere ignorar. Para ello, se debe [incluir el path del archivo en el `.gitignore`](#3.1.-Ignorar-archivos) tal como se indicó previamente.

---------------------------------------------

agregar comentario sobre el trackeo de archivos binarios en diff

---------------------------------------------

git log

        $ git log <file-path>                   # permite visualizar los commits que han modificado el
                                                # archivo indicado en <file-path>
                                                # también muestra los mensajes de confirmación
        
        $ git log -p <file-path>                # ídem anterior pero muestra además las modificaciones
                                                # realizadas

        $ git log                               # permite visualizar todos los commits y sus mensajes de
                                                # confirmación

        $ git log --oneline                     # ídem anterior pero muestra los commits en una sola
                                                # línea

        # git log --author=<author-name>        # muestra los commits realizador por <author-name>
