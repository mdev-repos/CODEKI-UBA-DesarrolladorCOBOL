<div style="text-align:center">

<h3> Clase Sincrónica N°22: Lectura VSAM + Generación FBA </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Leer archivo <strong>VSAM KSDS</strong> de clientes con acceso secuencial
* Formatear datos para impresión profesional en <strong>FBA 132 bytes</strong>
* Implementar conversiones de datos y mapeo de valores codificados

Al finalizar esta práctica; el estudiante dominará el flujo completo ETL en mainframe: desde lectura de VSAM hasta generación de reportes listos para impresión.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de transformación de datos VSAM a formato impresión con conversiones avanzadas.

* NOMBRE DEL PROGRAMA: <strong>PGMVSCAB</strong>.

* Archivo INPUT VSAM: <strong>CLIENT1.KSDS</strong>
    * Registros de 50 bytes (Key-sequenced)
    * Estructura: <strong>CPCLIE</strong> (COPYBOOK)
    * Organización: KSDS (Clave primaria embebida)

* Archivo OUTPUT FBA: <strong>PGMVSCAB.SALIDA</strong>
    * Registros de 132 bytes (estándar impresión)
    * Formateo profesional con:
        - Descripciones extensas (no códigos)
        - Fechas en formato DD/MM/AAAA
        - Símbolos monetarios
        - Alineación precisa

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Manejo de <strong>códigos de retorno VSAM</strong> (00, 10)
* Conversión de <strong>formatos numéricos</strong> (COMP-3 → Display)
* Transformación de <strong>valores codificados</strong> a descriptivos
* Formateo de <strong>fechas AAAAMMDD</strong> a DD/MM/AAAA

📂 **Archivos**  
* `PGMVSCAB.cob` 🗂️ (Programa principal)  
* `JCLVSCAB.txt` ⚙️ (JCL con parámetros VSAM)  
* `CPCLIE(COPY).txt` 📑 (Copybook de estructura)  
* `PGMVSCAB.SALIDA.txt` 🖨️ (Ejemplo de salida FBA)  
* `SYSOUT.txt` 📋 (Log de ejecución)  

💻 **Técnicas Clave Implementadas**
```cobol
2400-FORMATEAR-CAMPO.
   EVALUATE TRUE
      WHEN CLI-TIP-CUE = '01' MOVE 'CORRIENTE   ' TO DET-TIPO-CTA
      WHEN CLI-TIP-CUE = '02' MOVE 'AHORRO      ' TO DET-TIPO-CTA
      WHEN CLI-TIP-CUE = '03' MOVE 'PLAZO FIJO  ' TO DET-TIPO-CTA
      WHEN OTHER MOVE 'DESCONOCIDO ' TO DET-TIPO-CTA
   END-EVALUATE.

   *-- Formateo fecha AAAAMMDD → DD/MM/AAAA --*
   MOVE CLI-AAAAMMDD(7:2) TO DET-FECHA-CLI(1:2)
   MOVE '/' TO DET-FECHA-CLI(3:1)
   MOVE CLI-AAAAMMDD(5:2) TO DET-FECHA-CLI(4:2)
   MOVE '/' TO DET-FECHA-CLI(6:1)
   MOVE CLI-AAAAMMDD(1:4) TO DET-FECHA-CLI(7:4)
```

📝 Notas Técnicas

* Flujo de procesamiento:
    * Apertura <strong>VSAM</strong> en modo INPUT
    * Escritura registro formateado a <strong>FBA</strong>

💡 Patrones Reutilizables

* Mapeo de códigos a descripciones con <strong>EVALUATE</strong>
* Formateo de fechas con <strong>reordenamiento de substrings</strong>
* Plantillas FBA con <strong>alineación por columnas</strong>

🔍 Ejemplo de Estructura FBA
```cobol
01  WS-DETALLE.
    02  DET-TIPO-CTA    PIC X(12).
    02  FILLER          PIC X(3) VALUE ' | '.
    02  DET-SEXO-CLI    PIC X(8).
    02  FILLER          PIC X(3) VALUE ' | '.
    02  DET-FECHA-CLI   PIC X(10).
    02  FILLER          PIC X(3) VALUE ' | '.
    02  DET-SALDO-CLI   PIC $$$,$$$,$$9.99.
```