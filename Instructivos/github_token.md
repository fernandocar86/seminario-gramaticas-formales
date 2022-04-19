# Cómo generar y usar un token de acceso personal en GitHub

Movete hasta el ícono de tu usuario en GitHub y seleccioná "settings"

![](./token-images/20220418172147.png)  

Si te movés hasta abajo sobre la barra de la derecha, encontrás los "Developer Settings"

![](./token-images/20220418172246.png)  

Si clickeas ahí, vas a encontrarte con el "Personal Access Token"

![](./token-images/20220418172337.png)  

Una vey dentro de la opción "Personal Access Token" debés seleccionar "Generate new token" para generar uno nuevo

![](./token-images/20220418172429.png)  

Te va a pedir que vuelvas a poner tu contraseña para asegurarse de tu identidad:

![](./token-images/20220418172524.png)  

Agregale un nombre, un tipo de permiso ("scopes", preferiblemente "repos") y una cantidad de días en que puede estar habilitado (después de ese tiempo, vas a tener que crear otro si seguís usando el mismo repo en el que pusiste este token)

![](./token-images/20220418172729.png)  

Al final de las opciones de permisos vas a encontrar un botón verde que te permite finalmente crealo:

![](./token-images/20220418172819.png)  

Copiá el token generado. Va a ser la única vez que puedas verlo, pero podés crear tantos como quieras o copiarlo y guardarlo en un lugar seguro:

![](./token-images/20220418172947.png)  

La próxima vez que, al hacer un pull o push, git te pida que coloques tu nombre y tu contraseña de github, usá el token como contraseña. Así quedará fijado hasta que te muevas a otro repo o la contraseña expire:

![](./token-images/20220418173216.png)  

