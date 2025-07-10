<div style="text-align:center">

<h3> Clase Asincrónica N°16: SQL con Cursor + Corte de Control </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar consultas <strong>SELECT con JOIN</strong> en DB2 usando cursores
* Aplicar técnica de <strong>corte de control</strong> sobre resultados SQL
* Generar reportes con totales por agrupamiento

Al finalizar esta práctica; el estudiante habrá aprendido a:
- Combinar cursores DB2 con lógica de corte de control
- Calcular totales parciales y generales
- Manejar resultados SQL en estructuras COBOL

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de generación de reportes con agrupamiento mediante consulta JOIN a tablas DB2.

* NOMBRE DEL PROGRAMA: <strong>PGMB2CAB</strong>.

* Consulta SQL:
    * <strong>INNER JOIN</strong> entre tablas DB2
    * Selección de campos clave para agrupamiento
    * Filtrado específico según requerimientos

* Salida requerida:
    * <strong>DISPLAY</strong> con totales por sucursal
    * Total general al final del proceso
    * Control preciso de cambios de grupo

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Integración de <strong>lógica COBOL</strong> con <strong>resultados SQL</strong>
* Manejo de <strong>transiciones</strong> en corte de control
* Sincronización entre <strong>FETCH</strong> y <strong>comparación de claves</strong>

📂 **Archivos Principales**  
* `PGMB2CAB.cob` 🟦 (Programa con cursor SQL + corte)  
* `SYSOUT.txt` 📊 (Salida display + Reporte de ejecución)  

🔧 **Archivos de Soporte**  
* `COMPCDB2.txt` ⚙️ (JCL para compilación con SQL embebido)  
* `BINDESTU.txt` ⚙️ (JCL para proceso BIND)  
* `JCLB2CAB.txt` 🖥️ (JCL de ejecución)  

💡 **Técnicas Aprendidas** 
* Patrón <strong>FETCH + comparación de claves</strong>
* Cálculo de <strong>totales acumulados</strong>
* Manejo de <strong>indicadores de cambio</strong>
* Integración <strong>SQL-COBOL</strong> para reporting

🚨 **Consideraciones Clave**  
1. Ordenamiento <strong>consistente</strong> entre SQL y lógica COBOL
2. Inicialización correcta de <strong>variables acumuladoras</strong>
3. Manejo de <strong>SQLCODE</strong> en cada FETCH
4. Liberación de <strong>recursos DB2</strong> post-proceso

🔍 **SQL de Referencia**  
```sql
EXEC SQL                               
  DECLARE ITEM_CURSOR CURSOR FOR       
    SELECT A.TIPCUEN,                  
           A.NROCUEN,                  
           A.SUCUEN,                   
           A.NROCLI,                   
           B.NOMAPE,                   
           A.SALDO,                    
           A.FECSAL                    
    FROM KC02787.TBCURCTA A            
    INNER JOIN KC02787.TBCURCLI B      
      ON A.NROCLI = B.NROCLI           
    WHERE A.SALDO > 0                  
    ORDER BY A.SUCUEN                  
END-EXEC.                              
```