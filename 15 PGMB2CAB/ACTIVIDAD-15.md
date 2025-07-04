<div style="text-align:center">

<h3> Clase Sincrónica N°27: Insertar datos a tabla DB2 </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar operaciones <strong>INSERT</strong> en tabla DB2 desde COBOL
* Manejar <strong>SQL embebido</strong> y códigos de retorno SQLCODE

Al finalizar esta práctica; el estudiante habrá aprendido a codificar queries sobre tablas DB2 embebidas dentro de un programa COBOL (Sequence Query Language embebido).

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de actualización de clientes mediante operaciones masivas desde archivo VSAM a DB2.

* NOMBRE DEL PROGRAMA: <strong>PGMB2CAB</strong>.

* Archivo INPUT VSAM: <strong>NOVECLI.KSDS</strong>
    * Registros de 244 bytes
    * Estructura: <strong>TBVCLIEN</strong> (COPY)
    * Clave primaria: Posición 1 (17 bytes)

* Operación DB2: <strong>INSERT</strong> en tabla <strong>TBCURCLI</strong>
    * Debe contemplar manejo de errores SQLCODE (-803 para claves duplicadas)
    * Mapeo completo de columnas según layout

* Salidas requeridas:
    * Display con métricas de procesamiento
    * Registros erróneos identificados

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Primer contacto con <strong>SQL embebido</strong> en COBOL
* Mapeo preciso de campos VSAM → columnas DB2
* Gestión de errores SQLCODE durante proceso batch
* Configuración de entorno DB2 (precompilado y bind)

📂 **Archivos Principales**  
* `PGMB2CAB.cob` 🟦 (Programa con SQL embebido)  
* `NOVECLI.KSDS.txt` 📁 (Datos de entrada VSAM)  
* `TBVCLIEN.txt` 📋 (Layout de registros)  
* `SYSOUT.txt` 📊 (Reporte de ejecución)  

🔧 **Archivos de Soporte (multi-ejercicio)**  
* `COMPDB2(SQL-EMBEBIDO).txt` ⚙️ (JCL para compilación con SQL)  
* `BINDESTU(BIND).txt` 🔗 (JCL para bindeo PGM-DB2)  
* `JCLDB2XX.txt` 🖥️ (JCL de ejecución)  
* `JCLVSAM.txt` 🗃️ (JCL generación archivos VSAM)  

📝 **Nota Técnica**  
* Patrón base para:
    * Migraciones VSAM→DB2
    * Cargas iniciales de datos
    * Procesos ETL en entornos mainframe

💡 **Técnicas Aprendidas** 
* Uso de <strong>EXEC SQL INSERT</strong>
* Manejo de estructura <strong>SQLCA</strong>
* Flujo de precompilado COBOL-DB2
* Proceso completo BIND/ejecución

🚨 **Consideraciones Clave**  
1. Validar siempre <strong>SQLCODE</strong> después de cada operación
2. El archivo VSAM debe estar previamente ordenado por clave primaria
3. Requiere permisos GRANT sobre tabla destino
4. Tener en cuenta isolation levels (CS por defecto)

🔍 **SQL de Referencia**  
```sql
EXEC SQL INSERT                                   
  INTO TBCURCLI                           
    (TIPDOC, NRODOC, NROCLI, NOMAPE, FECNAC, SEXO)
  VALUES (:WS-CLI-TIPDOC,                         
          :WS-CLI-NRODOC,                         
          :WS-CLI-NROCLI,                         
          :WS-CLI-NOMAPE,                         
          :WS-CLI-FECNAC,                         
          :WS-CLI-SEXO)                           
END-EXEC.                                         