<div style="text-align:center">

<h3> Clase Sincrónica N°29: Procesamiento QSAM + Actualizaciones DB2 </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar operaciones <strong>CRUD</strong> en DB2 mediante SQL embebido
* Procesar archivos <strong>QSAM</strong> con validaciones complejas
* Generar estadísticas completas de procesamiento batch

Al finalizar esta práctica; el estudiante dominará el ciclo completo de actualización de bases de datos mainframe: desde validación de archivos planos hasta ejecución de transacciones SQL.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de gestión de clientes con operaciones de alta y modificación mediante SQL embebido.

* NOMBRE DEL PROGRAMA: <strong>PGMSIN29</strong>.

* Archivo INPUT QSAM: <strong>PGMSIN29.NOVEDAD</strong>
    * Registros de 80 bytes (formato fijo)
    * Estructura: <strong>NOVECLIE</strong> (COPYBOOK)
    * Tipos de novedad:
        - AL: Alta completa
        - CL: Modificación número cliente
        - CN: Modificación nombre
        - CX: Modificación sexo

* Operaciones DB2:
    * Tabla destino: <strong>KC02787.TBCURCLI</strong>
    * Validación de claves primarias
    * Manejo de errores SQLCODE

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Manejo de <strong>deadlocks DB2</strong> (SQLCODE -911)
* Conversión de <strong>formatos de fecha</strong> (AAAAMMDD → YYYY-MM-DD)
* Validación condicional por <strong>tipo de operación</strong>
* Gestión de <strong>transacciones implícitas</strong>

📂 **Archivos**  
* `PGMSIN29.cob` 🗂️ (Programa con SQL embebido)  
* `JCLSIN29.txt` ⚙️ (JCL de ejecución)  
* `NOVECLIE(COPY).txt` 📑 (Estructura de registros)  
* `SYSOUT.txt` 📋 (Reporte de resultados)  

💻 **Técnicas Clave Implementadas**

*-- Validación de año bisiesto --*
```cobol
IF FUNCTION MOD(WS-FECHA-ANIO, 400) = 0
   MOVE 29 TO WS-MAX-DIA
ELSE
   MOVE 28 TO WS-MAX-DIA
END-IF
```

*-- SQL para altas --*
```sql
EXEC SQL
  INSERT INTO KC02787.TBCURCLI
    (TIPDOC, NRODOC, NROCLI, NOMAPE, FECNAC, SEXO)
  VALUES (:WS-CLI-TIPDOC, :WS-CLI-NRODOC, 
          :WS-CLI-NROCLI, :WS-CLI-NOMAPE, 
          :WS-CLI-FECNAC, :WS-CLI-SEXO)
END-EXEC
```

📝 **Notas Técnicas**

* Flujo de validación por tipo de operación:
    1. **ALTAS**: Verificación de todos los campos obligatorios
    2. **MODIFICACIONES**: Validación específica por campo

* Estadísticas automatizadas:
    * Registros leídos
    * Errores detectados
    * Altas/modificaciones exitosas

💡 **Patrones Reutilizables**
* Uso de **COPYBOOKS** para estructuras de datos
* Modularización con **párrafos especializados**
* Plantilla para **reporte de estadísticas**