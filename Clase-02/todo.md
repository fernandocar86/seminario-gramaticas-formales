
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
