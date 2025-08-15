<div style="text-align:center">

<h3> Clase Sincrónica N°28: SQL con Cursor + Salida FBA </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar consultas <strong>SELECT con JOIN</strong> en DB2 desde COBOL usando cursores
* Generar reportes profesionales en formato <strong>FBA (132 bytes)</strong>
* Manejar <strong>paginación automática</strong> y formatos de impresión

Al finalizar esta práctica; el estudiante habrá aprendido a:
- Codificar SQL embebido con cursores para consultas complejas
- Diseñar layouts de impresión profesionales
- Implementar control de paginación en reportes

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de generación de listados mediante consulta JOIN a tablas DB2.

* NOMBRE DEL PROGRAMA: <strong>PGMSIN28</strong>.

* Consulta SQL:
    * <strong>INNER JOIN</strong> entre <strong>TBCURCTA</strong> y <strong>TBCURCLI</strong>
    * Filtro: <strong>SALDO > 0</strong>
    * 11 columnas seleccionadas

* Salida requerida:
    * Archivo <strong>KC03CAB.LISTADO.CLIENTES</strong> (FBA, 132 bytes)
    * Encabezados con fecha del sistema
    * Paginación cada 10 registros
    * Totales de registros procesados

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Primer uso de <strong>cursores DB2</strong> en COBOL
* Diseño de formato FBA con alineación precisa
* Implementación de paginación automática
* Mapeo de tipos de datos SQL → COBOL

📂 **Archivos Principales**  
* `PGMSIN28.cob` 🟦 (Programa COBOL | Cursor SQL)  
* `LISTADO.CLIENTES.txt` 📋 (Salida FBA generada)  
* `SYSOUT.txt` 📊 (Salida obtenida)  

🔧 **Archivos de Soporte**  
* `JCLSIN28.txt` 🖥️ (JCL de ejecución)  

💡 **Técnicas Aprendidas** 
* Declaración y uso de <strong>cursores DB2</strong>
* Formateo profesional con <strong>máscaras de edición</strong>
* Control de <strong>paginación automática</strong>
* Manejo de <strong>SQLCODE</strong> en operaciones FETCH

🚨 **Consideraciones Clave**  
1. Validar siempre <strong>SQLCODE</strong> después de OPEN/FETCH
2. El formato FBA requiere precisión en posiciones de campos
3. Los cursores deben cerrarse explícitamente
4. Las fechas SQL requieren conversión para presentación

🔍 **SQL de Referencia**  
```sql
EXEC SQL
  DECLARE ITEM_CURSOR CURSOR FOR
    SELECT A.TIPCUEN, A.NROCUEN, A.SUCUEN,
           A.NROCLI, B.NOMAPE, A.SALDO, A.FECSAL
    FROM TBCURCTA A
    INNER JOIN TBCURCLI B
      ON A.NROCLI = B.NROCLI
    WHERE A.SALDO > 0
END-EXEC.
```

📝 Fragmento Destacado: Formato FBA
```cobol
01 WS-DETALLE.
   02 FILLER          PIC X(08) VALUE "|       ".
   02 DET-TIP-CUEN    PIC X(02).
   02 FILLER          PIC X(07) VALUE "       ".
   02 FILLER          PIC X(06) VALUE "|     ".
   02 DET-NRO-CUEN    PIC ZZZZ9.
   02 FILLER          PIC X(06) VALUE "      ".
   02 FILLER          PIC X(07) VALUE "|     ".
   02 DET-NRO-SUC     PIC Z9.
   02 FILLER          PIC X(06) VALUE "      ".
   02 FILLER          PIC X(08) VALUE "|       ".
   02 DET-NRO-CLI     PIC ZZ9.
   02 FILLER          PIC X(07) VALUE "       ".
   02 FILLER          PIC X(02) VALUE "| ".
   02 DET-NOM-APE     PIC X(30).
   02 FILLER          PIC X(01) VALUE " ".
   02 FILLER          PIC X(02) VALUE "| ".
   02 DET-SALDO       PIC $ZZ.ZZ9,99-.
   02 FILLER          PIC X(01) VALUE " ".
   02 FILLER          PIC X(04) VALUE "|   ".
   02 DET-FECHA       PIC X(10).
   02 FILLER          PIC X(04) VALUE "  | ".
```

📊 Ejemplo de Salida
```cobol
LISTADO DE CLIENTES CON SALDO MAYOR A CERO - FECHA: 05-07-2025
-----------------------------------------------------------------------------------
| TIPO CUENTA | NRO CUENTA | SUC | NRO CLIENTE | NOMBRE Y APELLIDO   | SALDO      |
|       CC    |    12345   | 01  |     100     | JUAN PEREZ          | $ 1.234,56 |
|       CA    |    67890   | 02  |     101     | MARIA GOMEZ         | $ 5.678,90 |
-----------------------------------------------------------------------------------
```