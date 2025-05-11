<div style="text-align:center">

<h3> Clase Sincrónica N° 13:  Construcción de un programa COBOL con validación </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un programa y ejecutarlo exitosamente, utilizando codificación COBOL.   

<strong>ESPECIFICACIONES</strong>:
* NOMBRE DEL PROGRAMA: <strong> PGMC1CAB</strong>.
* Leer secuencialmente los registros del archivo <strong>CLICOB</strong>.
    * Cada registro del archivo es de 93 bytes de largo fijo .
    * Estructura del archivo de entrada: <strong>CLICOB</strong> ( COPY ). 

* Luego de leído cada registro comparar:
    * Si corresponde a “SOLTERO” (WS-SUC-EST-CIV= SOLTERO) contar la cantidad de registros que cumplen la condición.
    * Si corresponde a “CASADO” (WS-SUC-EST-CIV= CASADO) contar la cantidad de registros que cumplen la condición.
    * Si corresponde a “VIUDO” (WS-SUC-EST-CIV= VIUDO) contar la cantidad de registros que cumplen la condición.
    * Si corresponde a “DIVORCIADO” (WS-SUC-EST-CIV= DIVORCIADO) contar la cantidad de registros que cumplen la condición.

* Al final del programa mostrar por DISPLAY la cantidad de registros leídos y la cantidad de registros que cumplieron cada condición de punto anterior.

*  Se podrá utilizar como esqueleto de código COBOL <strong>PGMEJEM1</strong> (PGM COBOL PROVISTO POR LA CÁTEDRA)

* Para ejecutar, partir como esqueleto del programa <strong>JCLEJEM1</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>