<div style="text-align:center">

<h3> Clase Asincrónica N° 8: Programa de validación </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Un programa debe contemplar todas las instancias y considerar posibles cancelaciones programadas; ello se logra a través de la incorporación de la validación de variables de ingreso.

<strong>ESPECIFICACIONES</strong>:
* Este programa leerá el archivo de <strong>NOVEDADES DE CLIENTES</strong> y validará cada uno de los campos de los registros leídos  según las siguientes consignas: 

* NOMBRE DEL PROGRAMA: <strong> PGMVACAB</strong>.
* Partir del programa ya realizado ( <strong>PGMSIN14</strong> ) donde se valida solamente el tipo de documento. Agregar el resto de las validaciones que se solicitan. 

    * Las validaciones están dispuestas dentro de la estructura en <strong>CPNOVCLI</strong>. 

*  <strong>Validar</strong>
    * Los campos numéricos y las fechas.  
    * En caso de encontrar errores, mostrar la situación por <strong>DISPLAY</strong>; mostrar el registro (solamente los campos <strong>TIPO Y NRO DOCUMENTO</strong> más el <strong>CAMPO INVÁLIDO</strong>) y luego detallar cada error encontrado.
        * Ejemplo: Si la fecha es errónea, mostrar los campos del registro indicados más arriba y luego la leyenda “fecha errónea”. 


* Se deberán mostrar (DISPLAY) todos los errores encontrados en cada registro de entrada.

* Luego de la validación satisfactoria de cada registro de NOVEDADES, se proponen dos soluciones las cuales cada estudiante podrá elegir para presentar en MOODLE o también podrá presentar ambas:
    * <strong>SOLUCIÓN 1</strong> --> se deberá grabar un registro en el archivo de novedades validadas. Agregando el número de secuencia; según orden de ingreso de las novedades. 

    * <strong>SOLUCIÓN 2</strong> -->  Mostrar vía DISPLAY Tipo y nro de documento que cumplió exitosamente la validación de todos sus campos.

* Al final del programa mostrar por DISPLAY: 
    * CANTIDAD NOVEDADES LEÍDAS (sumatoria registros leídos del archivo NOVEDADES) 
    * CANTIDAD NOVEDADES ERRÓNEAS 
    * CANTIDAD REGISTROS GRABADOS

* NOMBRE DEL ARCHIVO DE OUTPUT NOVEDADES VALIDADAS CLIENTES: <strong>NOVCLIEN.VALID</strong>.


</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Implementar **validación múltiple de campos** (numéricos, fechas, formatos)
* Generar **mensajes de error detallados** manteniendo claridad en DISPLAYs
* Manejar **registros con múltiples errores** sin interrumpir el flujo

📂 **Archivos**  
* `PGMVACAB.cob` 🟦 (Programa con validación extendida)  
* `JCLVACAB.txt` ⚙️ (Job con asignación de archivos)  
* `CPNOVCLI(COPY).txt` 📄 (Copybook con estructura y validaciones)  
* `NOVCLIEN(QSAM).txt` 📁 (Dataset de novedades)  
* `NOVCLIEN.VALID(QSAM).txt` ✅ (Dataset de salida para válidos)  
* `SYSOUT.txt` 📋 (Ejemplo de salida con errores y resumen)

📝 **Nota**
* Sistema completo de validación de datos para procesos ETL bancarios. Patrón aplicable a:
    * Migraciones de datos
    * Cargas iniciales
    * Procesos de homologación

💡 **Best Practices implementadas**
* Flag de error (WS-ERROR-FLAG) para Manejo coherente de múltiples validaciones.
* Módulos separados por tipo de validación (fechas, números, documentos).
* Mensajes autocontenidos con identificación clara del registro erróneo.