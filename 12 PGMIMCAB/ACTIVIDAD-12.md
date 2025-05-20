<div style="text-align:center">

<h3> Clase Asincrónica N° 10: Programa de impresión </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Imprimir cada uno de los registros de <strong>CLIENTES</strong> leídos.
* Grabar un archivo tipo <strong>FBA</strong> ( Fixed Block Address )

Al finalizar esta actividad; el estudiante habrá desarrollado una nueva habilidad, que
es la generación de un archivo de tipo FBA en código COBOL.

<br>

<strong>ESPECIFICACIONES</strong>:  
Esta actividad se podrá visualizar desde el editor o bajar a papel impreso ( práctica NO recomendada en la actualidad; para el cuidado del ambiente ). En adelante nos referiremos a este concepto como: <strong>IMPRESIÓN</strong> o <strong>IMPRIMIR</strong>. Este programa leerá un archivo <strong>QSAM</strong> ( Queue Sequence Access Method | Método de Acceso Secuencial en Cola ) de <strong>CLIENTES</strong> para imprimir cada uno de los registros leídos; haciendo corte de control por <strong>TIPO DE CUENTA</strong> (<strong>CLIS-TIPO</strong>).

* NOMBRE DEL PROGRAMA: <strong>PGMIMCAB</strong>.

* Archivo de INPUT QSAM: <strong>CLIENTE</strong>.
    * Registros de 50 bytes - largo fijo.
    * Estructura del archivo de entrada: <strong>CPCLIENS</strong> ( COPY ).

* Archivo de OUTPUT FBA: <strong>SALIDA</strong>.
    * Registros de 132 bytes - largo fijo.


</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>
<br>

🎯 **Dificultades clave**
* Implementar **formato FBA** (Fixed Blocked Architecture) con longitud fija de 132 bytes
* Manejar **corte de control** por tipo de cuenta (Corriente, Ahorro, Plazo Fijo)
* Diseñar **estructuras de impresión** profesionales con subtotales y totales
* Gestionar **paginación automática** (60 líneas por página)
<br>

📂 **Archivos del Proyecto**
* `PGMIMCAB.cob` 🟦 (Programa principal con lógica de impresión)
* `JCLIMCAB.txt` ⚙️ (Job con pre-SORT y configuración FBA)
* `CPCLIENS(COPY).txt` 📄 (Copybook de entrada)
* `CLIENTE(QSAM).txt` 📁 (Dataset de clientes)
* `SALIDA(FBA).txt` 📜 (Ejemplo de reporte generado)
* `SYSOUT.txt` 📜 (Estadísticas del programa)
<br>


💡**Best Practices Implementadas**

* Validación de File Status después de cada operación I/O

* Inicialización explícita de acumuladores (VALUE ZEROS)

* Manejo estructurado de errores con rutina dedicada (9000-SALIDA-ERRORES)

* Documentación interna con párrafos autoexplicativos

<br>

🔗 **Aplicación en Sistemas Reales**

* Reportes de posición consolidada

* Estados de cuenta mensuales

* Conciliaciones bancarias

* Saldos por tipo de producto

<br>

<div style="text-align:center">

💡 <strong><em>El formato FBA garantiza compatibilidad con herramientas de reporting como SAS y generación de PDFs</em></strong>

</div>
