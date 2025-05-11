<div style="text-align:center">

<h3>Clase Asincrónica N° 6: Primer corte de control</h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un programa de corte de control y ejecutarlo exitosamente. Como es el 
primer programa con esta estructura típica solamente se hará CORTE DE CONTROL por SUCURSAL. 

* Al finalizar esta práctica el estudiante habrá logrado un nuevo objetivo: ESCRIBIR 
CÓDIGO COBOL CON UN CORTE DE CONTROL en forma exitosa; o sea que, al ejecutarlo, resuelve el corte de control solicitado.
 

<strong>ESPECIFICACIONES</strong>:

* NOMBRE DEL PROGRAMA: <strong>PGM2CCAB</strong>.
* Partir como esqueleto del PGM <strong>PGMCORTE</strong> ( <em>PGM COBOL PROVISTO POR LA CÁTEDRA</em> ).
* Leer secuencialmente los registros del archivo <strong>CORTE</strong>.
    * Cada registro del archivo es de largo fijo de 20 bytes de largo.
    * Estructura del archivo de entrada: <strong>CORTE</strong> ( COPY ). 

* Con los registros procesados; hacer corte de control por el campo <strong>WS-SUC-NRO</strong>.
*  Al final de cada corte de SUCURSAL, MOSTRAR (<strong>DISPLAY</strong>): Número de sucursal e importe 
total junto a la sumatoria de importes correspondientes a dicha sucursal (campo <strong>
WS-SUC-IMPORTE</strong>).  Con máscara de edición que elimine los ceros no significativos.
    * Ejemplo:

|||    
| -----| -------------|
|Sucursal 1|1.000.000|
|Sucursal 2|21.148|
|Sucursal 3|33.458|

* Al final del programa MOSTRAR: el total general de importes para el archivo procesado. 
Sumatoria de WS-SUC-IMPORTE. Con máscara de edición que elimine los ceros no 
significativos. 
    * Ejemplo:

|TOTAL GENERAL|1.054.606|    
| -----| -------------|

* Para ejecutar, partir como esqueleto del programa <strong>JCLCORT1</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

<div style="text-align:justify">

🎯 **Dificultades**  
* Implementar correctamente la lógica de **corte de control** (inicialización, comparación de claves y procesamiento de totales).
* Aplicar **máscaras de edición** para el formato de salida (eliminar ceros no significativos).
* Manejar dos niveles de totalización: por sucursal y general.

📂 **Archivos**  
* `PGM2CCAB.cob` 🟦 (Programa con lógica de corte de control)  
* `JCL2CCAB.txt` ⚙️ (Job para ejecución en Mainframe)  
* `CORTE(COPY).txt` 📄 (Copybook con estructura del registro)  
* `CORTE(QSAM).txt` 📁 (Dataset de entrada - QSAM)  
* `SYSOUT.txt` 📋 (Ejemplo de salida esperada)

📝 **Nota**
* Primer programa con corte de control, técnica fundamental para reportes bancarios (ej: balances por sucursal).

</div>