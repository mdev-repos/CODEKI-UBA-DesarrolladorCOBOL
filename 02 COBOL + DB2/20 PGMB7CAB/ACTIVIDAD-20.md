<div style="text-align:center">

<h3> Clase Sincrónica: Apareo de Cursors DB2 en COBOL </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar técnica de <strong>apareo manual</strong> entre dos cursors DB2
* Practicar acceso relacional a tablas <strong>sin usar JOIN</strong> SQL
* Generar reporte con lógica de control por NROCLI

Al finalizar esta práctica; el estudiante dominará el apareo de resultados SQL mediante lógica programática en COBOL.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa que relaciona clientes con sus cuentas mediante apareo de cursors.

* NOMBRE DEL PROGRAMA: <strong>PGMB7CAB</strong>.

* Tablas DB2:
    * <strong>KC02787.TBCURCLI</strong> (Datos de clientes)
    * <strong>KC02787.TBCURCTA</strong> (Datos de cuentas)

* Estructuras:
    * Dos cursors independientes ordenados por NROCLI
    * Lógica de apareo manual en COBOL
    * Generación de archivo FBA con resultados

* Salidas requeridas:
    * Listado de clientes encontrados
    * Mensajes para registros no apareados
    * Totales estadísticos del proceso

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Sincronización de <strong>dos cursors activos</strong> simultáneamente
* Manejo de <strong>fin de datos</strong> en cada cursor
* Preservar <strong>ordenamiento</strong> durante el apareo
* Conversión de <strong>formatos DB2</strong> a salida legible

📂 **Archivos**  
* `PGMB7CAB.cob` 🗂️ (Programa con SQL embebido)  
* `JCLB7CAB.txt` ⚙️ (JCL para ejecución DB2)  
* `CLIENTES(FBA).txt` 📄 (Reporte de salida formateado)  
* `SYSOUT.txt` 📋 (Estadísticas de ejecución)  

💻 **Técnicas Clave Implementadas**
```sql
*-- Declaración de cursors --*
* CURSOR TBCURCLI                                          
                                            
     EXEC SQL                                                
       DECLARE CURSOR-CLI CURSOR FOR                         
         SELECT TIPDOC,                                      
                NRODOC,                                      
                NROCLI,                                      
                NOMAPE                                       
          FROM KC02787.TBCURCLI                              
      ORDER BY NROCLI                                        
     END-EXEC.                                               
                                                             
* CURSOR TBCURCTA                                            
                                                             
     EXEC SQL                                                
       DECLARE CURSOR-CTA CURSOR FOR                         
         SELECT NROCLI,                                      
                SUCUEN                                       
          FROM KC02787.TBCURCTA                              
      ORDER BY NROCLI                                        
     END-EXEC.                                               
