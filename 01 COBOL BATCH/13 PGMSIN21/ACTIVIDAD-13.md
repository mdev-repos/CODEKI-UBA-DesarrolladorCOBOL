<div style="text-align:center">

<h3> Clase Sincrónica N°21: Archivos FBA con Corte de Control </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar <strong>corte de control</strong> por tipo de documento (CLIS-TIP-DOC)
* Generar archivo <strong>FBA de 132 bytes</strong> con formato de reporte profesional
* Dominar técnicas de alineación y presentación de datos en entornos mainframe

Al finalizar esta práctica; el estudiante podrá generar reportes listos para impresión con totales parciales y estructura jerárquica.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de generación de reportes con lógica de corte de control y formato FBA estándar.

* NOMBRE DEL PROGRAMA: <strong>PGMSIN21</strong>.

* Archivo INPUT QSAM: <strong>CLIENTES</strong>
    * Registros de 50 bytes (fixed length)
    * Campos clave para corte: CLIS-TIP-DOC

* Archivo OUTPUT FBA: <strong>PGMSIN21.SALIDA</strong>
    * Registros de 132 bytes (formato impresión)
    * Estructura detallada en área de WORKING-STORAGE

* Características especiales:
    * Numeración de páginas
    * Fecha automática
    * Totales por tipo de documento
    * Diseño profesional con separadores

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Lógica de <strong>corte de control</strong> anidado
* Alineación precisa en formato FBA
* Manejo de <strong>paginación</strong> automática
* Conversión de datos numéricos a formatos visuales

📂 **Archivos**  
* `PGMSIN21.cob` 🖨️ (Programa COBOL | Estructura FBA)  
* `JCLSIN21.txt` ⚙️ (JCL de ejecución)  
* `PGMSIN21.SALIDA.txt` 📋 (Archivo FBA generado)  
* `SYSOUT.txt` 📊 (Salida obtenida)  

💻 **Estructuras Clave**  
```cobol
01  WS-HEADER-INICIAL.                                           
    02  FILLER          PIC X(10) VALUE SPACES.                  
    02  FILLER          PIC X(19) VALUE "DETALLE DE CLIENTES".   
    02  FILLER          PIC X(11) VALUE SPACES.                  
    02  FILLER          PIC X(07) VALUE "FECHA: ".               
    02  WS-FECHA        PIC X(10).                               
    02  FILLER          PIC X(13) VALUE SPACES.                  
    02  FILLER          PIC X(08) VALUE "PAGINA: ".              
    02  WS-PAGINA       PIC Z9.                                  
    02  FILLER          PIC X(52) VALUE SPACES.
```

📝 Notas Técnicas

* Patrón aplicable a:
    * Reportes financieros
    * Extractos de cuentas
    * Listados de movimientos

* Sección de WORKING-STORAGE organizada en 5 bloques:
    * Header inicial
    * Cabecera de columnas
    * Subcabecera
    * Detalle
    * Totales

💡 Técnicas Aprendidas
* Diseño de <strong>formatos FBA</strong> profesionales
* Uso de <strong>PIC Z9</strong> para ocultar ceros
* Alineación con <strong>FILLER</strong> y pipes ("|")
* Formateo numérico con símbolos monetarios