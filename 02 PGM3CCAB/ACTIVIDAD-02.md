<div style="text-align:center">

<h3>Clase Sincrónica N° 11: Ejercitación código COBOL</h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un nuevo programa en código COBOL.
 
* Al finalizar esta práctica el estudiante habrá logrado un nuevo objetivo: PRACTICAR 
CONCEPTO CÓDIGO COBOL en forma exitosa; o sea que, al ejecutarlo realiza la 
funcionalidad solicitada

<strong>ESPECIFICACIONES</strong>:

* NOMBRE DEL PROGRAMA: <strong>PGM3CCAB</strong>.
* Partir como esqueleto del PGM <strong>PGMPRUAR</strong> ( <em>PGM COBOL PROVISTO POR LA CÁTEDRA</em> ).
* Leer secuencialmente los registros del archivo <strong>CLIENTES</strong>. 
    * Estructura del archivo de entrada: <strong>CPCLI</strong>. 

* Con cada uno de los registros leídos; hacer:
    * SELECCIONAR solamente los registros cuyo campo <strong>CLI-TIP-DOC = ‘DU’</strong>.
    * Con los registros seleccionados; SUMAR campo <strong>CLI-SALDO</strong> en un totalizador general.
*  Al final del programa; MOSTRAR (<strong>DISPLAY</strong>): Total importe sumado para tipo de documento DU.   
* Para ejecutar, partir como esqueleto del programa <strong>EJESUIMP</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

<div style="text-align:justify">

🎯 **Dificultades**  
* Implementar correctamente la condición `IF CLI-TIP-DOC = 'DU'` dentro del flujo de lectura secuencial.
* Inicializar y actualizar el totalizador (`WS-TOTAL-DU`) sin interferir con el proceso principal.

📂 **Archivos**  
* `PGM3CCAB.cob` 🟦 (Programa COBOL | Lógica de filtrado y suma de saldos)  
* `JCL3CCAB.txt` ⚙️ (Job con asignación del archivo CLIENTES)  
* `CPCLI(COPY).txt` 📄 (Copybook con estructura del registro)  
* `CLIENTE(QSAM).txt` 📁 (Dataset de entrada - QSAM) 
* `SYSOUT.txt` 📋 (Salida obtenida)

📝 **Nota**
* Primer contacto con filtrado de registros y totalizadores en COBOL. ¡Esencial para procesamiento batch bancario!.

</div>