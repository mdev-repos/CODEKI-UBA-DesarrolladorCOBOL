<div style="text-align:center">

<h3> Clase Sincrónica N°31: Apareo de QSAM con Cursor DB2 </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar técnica de <strong>apareo</strong> entre archivo QSAM y cursor DB2
* Procesar actualizaciones de saldo mediante <strong>SQL embebido</strong>
* Generar reporte detallado de movimientos y estadísticas

Al finalizar esta práctica; el estudiante dominará el apareo de estructuras secuenciales con tablas DB2, incluyendo manejo de errores y generación de reportes.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de actualización de saldos mediante apareo entre archivo de novedades y tabla de cuentas.

* NOMBRE DEL PROGRAMA: <strong>PGMSIN31</strong>.

* Archivo INPUT QSAM: <strong>PGMSIN31.SORT</strong>
    * Registros de 23 bytes
    * Estructura: <strong>NOVCTA</strong> (COPYBOOK)
    * Contiene movimientos a aplicar

* Cursor DB2: 
    * Tabla: <strong>KC02787.TBCURCTA</strong>
    * Filtro: Sucursal = 1
    * Campos: Tipo cuenta, Nro cuenta, Sucursal, Nro cliente, Saldo, Fecha

* Operaciones:
    * Búsqueda de cliente en <strong>TBCURCLI</strong>
    * Actualización de saldos
    * Generación de reporte con:
        - Datos de la cuenta
        - Movimiento aplicado
        - Saldo actualizado

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Sincronización entre <strong>lectura secuencial</strong> y <strong>cursor DB2</strong>
* Manejo de <strong>claves compuestas</strong> en apareo
* Conversión de <strong>formatos numéricos</strong> (COMP-3 a display)
* Gestión de <strong>errores SQL</strong> durante el apareo

📂 **Archivos**  
* `PGMSIN31.cob` 🗂️ (Programa COBOL)  
* `JCLSIN31.txt` ⚙️ (JCL de ejecución)  
* `SYSOUT.txt` 📋 (Salida obtenida)  
* `PGMSIN31.SORT(QSAM).txt` 📁 (Datos de entrada)  
* `NOVCTA(COPY).txt` 📑 (Copybook de estructura)  
* `CURSOR-DATA.txt` 🔍 (Resultados del cursor)  

💻 **Técnicas Clave Implementadas**

*-- Declaración del cursor --*
```sql
EXEC SQL
  DECLARE ITEM_CURSOR CURSOR FOR
    SELECT TIPCUEN, NROCUEN, SUCUEN, NROCLI, SALDO, FECSAL
    FROM KC02787.TBCURCTA
    WHERE SUCUEN = 1
    ORDER BY TIPCUEN, SUCUEN, NROCLI
END-EXEC
```

*-- Lógica de apareo --*

```cobol
EVALUATE TRUE
  WHEN WS-CLAVE-NOVEDAD = WS-CLAVE-CURSOR
    PERFORM 2400-BUSCAR-CLIENTE
    PERFORM 2600-MOSTRAR-DATOS
  WHEN WS-CLAVE-NOVEDAD > WS-CLAVE-CURSOR
    DISPLAY 'CUENTA SIN NOVEDAD'
    PERFORM 2200-FETCH-CURSOR
  WHEN WS-CLAVE-NOVEDAD < WS-CLAVE-CURSOR
    DISPLAY 'NOVEDAD NO ENCONTRADA'
    PERFORM 1600-LEER-NOVEDAD
END-EVALUATE
```

📝 **Notas Técnicas**

* Flujo de apareo optimizado para:
    * Minimizar accesos a DB2
    * Garantizar ordenamiento correcto
    * Manejar todos los casos de no apareo

* Estadísticas automatizadas:
    * Novedades procesadas
    * Clientes encontrados/no encontrados
    * Saldos actualizados

🔍 **Ejemplo de Salida**

```cobol
****************************************
  - TIPO DE CUENTA: CC
  - NRO. DE CUENTA: 1001
  - NRO. DE CLIENTE: 501
  - NOMBRE: MAZZITELLI MATIAS
     * SALDO ANTERIOR: $1.250,50
     + MOVIMIENTO: $200,00
  - SALDO ACTUALIZADO: $1.450,50
****************************************
```

