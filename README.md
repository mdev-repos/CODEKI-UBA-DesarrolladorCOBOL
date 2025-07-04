<div style="text-align:center">

## 🏦 Repositorio de Formación COBOL Mainframe (UBA/CODEKI)  

</div>

<div style="text-align:justify">

¡Hola! 👋 Soy **Matías**, futuro desarrollador Mainframe especializado en **COBOL**, **CICS** y **DB2**.  
Este repositorio documenta mi camino de aprendizaje con ejercicios prácticos del curso **Desarrollador COBOL** brindado por los expertos de **CODEKI | UBA Económicas**.  

**¿Qué encontrarás aquí?**  
- Soluciones a problemas **reales del sector bancario** (simulados en el curso).  
- Código **COBOL** bien documentado, **JCL**s funcionales y ejemplos con archivos **QSAM**.  
- Mi evolución: desde programas básicos hasta sistemas con validaciones, apareos y cortes de control.  

<div style="text-align:center">

*"El Mainframe no es viejo, es clásico. Y los clásicos nunca pasan de moda."*  

<hr>

### Resumen de las soluciones desarrolladas

</div>


| <em>n</em>| <em>Nombre</em>| <em>COBOL</em>| <em>JCL</em>| <em>Archivos</em>|
| -----| -------------      |:-------------:|:-------------:|:-------------:|
|01| <strong>PGMSUMA</strong> |Sintaxis Básica|Ejecución|NO|
|02| <strong>PGM3CCAB</strong> |Lectura de Archivo|Archivo (Entrada)|QSAM (Entrada)|
|03| <strong>PGM2CCAB</strong> |Lectura de Archivo <br> Corte (1) de Control|Archivo (Entrada) <br> SORT|QSAM (Entrada)|
|04| <strong>PGMSIN12</strong> |Lectura de Archivo <br> Corte (2) de Control|Archivo (Entrada) <br> SORT|QSAM (Entrada)|
|05| <strong>PGMCORT2</strong> |Lectura de Archivo <br> Corte (2) de Control|Archivo (Entrada) <br> SORT|QSAM (Entrada)|
|06| <strong>PGMC1CAB</strong> |Lectura de Archivo <br> Validación de (1) Campo|Archivo (Entrada)|QSAM (Entrada)|
|07| <strong>PGMSIN14</strong> |Lectura de Archivo <br> Validación de (1) Campo <br> Escritura de Archivo|Archivo (Entrada) <br> Archivo (Salida)|QSAM (Entrada) <br> QSAM (Salida)|
|08| <strong>PGMVACAB</strong> |Lectura de Archivo <br> Validacion de Campos <br> Escritura de Archivo|Archivo (Entrada) <br> Archivo (Salida)|QSAM (Entrada) <br> QSAM (Salida)|
|09| <strong>PGMAPCAB</strong> |Lectura de Archivos (2) <br> Apareo de Archivos <br> Escritura de Archivo|Archivos (Entrada) <br> SORT <br> Archivo (Salida)|QSAM (Entradas) <br> QSAM (Salida)|
|10| <strong>PGMTACAB</strong> |Lectura de Archivos (2) <br> Uso de Vectores (OCCURS)|Archivos (Entrada)|QSAM (Entradas)|
|11| <strong>PGMSIN18</strong> |Lectura de Archivos (2) <br> Apareo de Archivos <br> Escritura de Archivo|Archivos (Entrada) <br> SORT <br> Archivo (Salida)|VSAM (Entrada) <br> QSAM (Entrada) <br> QSAM (Salida)|
|12| <strong>PGMIMCAB</strong> |Lectura de Archivos (1) <br> Corte de Control <br> Escritura de Archivo FBA|Archivos (Entrada) <br> SORT <br> Archivo (Salida)|QSAM (Entrada) <br> FBA (Salida)|
|13| <strong>PGMSIN21</strong> |Lectura de Archivos (1) <br> Corte de Control <br> Escritura de Archivo FBA|Archivos (Entrada) <br> SORT <br> Archivo (Salida)|QSAM (Entrada) <br> FBA (Salida)|
|14| <strong>PGMVSCAB</strong> |Lectura de Archivos (1) <br> Escritura de Archivo FBA|Archivos (Entrada) <br> Archivo (Salida)|VSAM (Entrada) <br> FBA (Salida)|
|15| <strong>PGMB2CAB</strong> |Creacion de Archivo VSAM (1) <br> Lectura de Archivo (1) <br> INSERT de datos en Tabla Db2|Archivo (Entrada) <br> SQL Embebido|VSAM (Entrada) <br> Tabla Db2 (Actualizada)|