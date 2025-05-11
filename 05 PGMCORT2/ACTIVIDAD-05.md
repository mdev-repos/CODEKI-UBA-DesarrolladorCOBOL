<div style="text-align:center">

<h3>Clase Asincrónica N° 7:  Programa de CORTE DE CONTROL </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Comprender la resolución de un CORTE DE CONTROL dentro del negocio financiero.   

<strong>ESPECIFICACIONES</strong>:
* Este programa resolverá  <strong>CORTE DE CONTROL POR TIPO DOCUMENTO Y SEXO</strong>.
    * Tener presente de considerar <strong>SOLAMENTE TIPO DOCUMENTO VÁLIDOS: 'DU'; 'PA'; 'PE'; 'CI'</strong>. 
* NOMBRE DEL PROGRAMA: <strong>PGMCORT2</strong>.
* Leer secuencialmente los registros del archivo <strong>CLICOB</strong>.
    * Cada registro del archivo es de 93 bytes de largo fijo .
    * Estructura del archivo de entrada: <strong>CLICOB</strong> ( COPY ). 

* CORTE MAYOR - TIPO DE DOCUMENTO campo <strong>WS-SUC-TIP-DOC</strong>. Contar cantidad de registros por TIPO DE DOCUMENTO  y mostrarlos al final de cada corte.

*  CORTE MENOR - SEXO campo <strong>WS-SUC-SEXO</strong>. Contar cantidad de registros por SEXO  y mostrarlos al final de cada corte.

* Tener en cuenta que el archivo podría estar vacío y resolver.

* Al final mostrar cantidad total de registros leídos.

* Para ejecutar, partir como esqueleto del programa <strong>JCLCORTE</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Implementar **validación de documentos** (solo aceptar 'DU', 'PA', 'PE', 'CI')
* Manejar **archivos potencialmente vacíos** sin errores de ejecución
* Organizar **contadores anidados** (tipo doc > sexo) con reinicialización adecuada

📂 **Archivos**  
* `PGMCORT2.cob` 🟦 (Programa con validación y doble corte)  
* `JCLCORT2.txt` ⚙️ (Job con asignación de archivos)  
* `CLICOB(COPY).txt` 📄 (Copybook con estructura de 93 bytes)  
* `CLICOB(QSAM).txt` 📁 (Dataset de clientes)  
* `SYSOUT.txt` 📋 (Ejemplo de reporte final)  

📝 **Nota**
* Ejercicio clave para reportes demográficos en bancos (ej: análisis por tipo documento y género).
La validación de documentos simuló requisitos reales de compliance.

⚠️ **Caso especial**
* La estructura maneja correctamente archivos vacíos.