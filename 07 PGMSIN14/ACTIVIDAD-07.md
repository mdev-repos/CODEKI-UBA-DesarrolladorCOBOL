<div style="text-align:center">

<h3> Clase Sincrónica N° 14:  Construcción de un programa COBOL con validación </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un programa y ejecutarlo exitosamente, utilizando codificación COBOL con validación.   

<strong>ESPECIFICACIONES</strong>:
* NOMBRE DEL PROGRAMA: <strong> PGMSIN14</strong>.
* Leer secuencialmente los registros del archivo <strong>NOVCLIEN</strong>.
    * Registros de 50 bytes - largo fijo.
    * Estructura del archivo de entrada: <strong>CPNOVCLI</strong> ( COPY ). 

* Luego de leído cada registro validar:  
    * <strong>TIPO DE DOCUMENTO</strong>; los válidos son: <strong>DU, PA, PE, CI</strong> 

* Si es un registro válido: se podrá optar por alguna de las siguientes soluciones:
    * <strong>SOLUCIÓN 1</strong> --> Se grabará el registro en un archivo de salida con diseño <strong>CPNCLIV</strong> ( COPY ) Teniendo en cuenta: 
        * El campo <strong>NOV-SECUEN</strong> es un número de secuencia que comienza por 1 y se irá incrementando de 1 por cada registro grabado.
        * El campo <strong>NOV-RESTO</strong> será la copia del registro del archivo de ENTRADA.

    * <strong>SOLUCIÓN 2</strong> -->  Se mostrará vía DISPLAY el tipo y número de documento que resultó satisfactorio indicando: REGISTRO VALIDADO OK.

* Si es un registro inválido: Mostrar por DISPLAY el tipo y número de documento indicando: <strong>TIPO DOCUMENTO INVÁLIDO</strong>.

* Al final se mostrará:  
    * CANTIDAD DE REGISTROS LEÍDOS 
    * CANTIDAD DE REGISTROS GRABADOS 
    * CANTIDAD DE REGISTROS ERRÓNEOS

*  Se podrá utilizar como esqueleto de código COBOL <strong>PGMEJEM1</strong> (PGM COBOL PROVISTO POR LA CÁTEDRA)

* Para ejecutar, partir como esqueleto del programa <strong>JCLEJEM1</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>