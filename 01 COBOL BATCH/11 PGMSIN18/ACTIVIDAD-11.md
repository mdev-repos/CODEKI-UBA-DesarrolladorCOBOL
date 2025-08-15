<div style="text-align:center">

<h3> Clase Sincrónica N° 18: Programa de práctica con VSAM </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Practicar acceso a almacenamiento VSAM KSDS.
    * ACTIVIDAD NRO 1: ACCESO SECUENCIAL.
    * ACTIVIDAD NRO 2: ACCESO RANDOM. 

<strong>ESPECIFICACIONES</strong>:  

<hr>

<strong>ACTIVIDAD NRO 1: ACCESO SECUENCIAL</strong>
* Se accederá secuencialmente a un archivo <strong>VSAM KSDS</strong> ( <strong>clave = Tipo y Nro documento</strong> ) de <strong>CLIENTES</strong> y se irá apareando con un archivo de ; previamente ordenado por <strong>Tipo y Nro de documento</strong>.

* Por cada registro de <strong><em>NOVEDADES</em></strong> encontrado en <strong><em>CLIENTES</em></strong> acumular un contador de <strong>“REGISTROS ENCONTRADOS”</strong> y también <strong>GRABAR</strong> un registro en un <strong>archivo de OUTPUT QSAM</strong> (con un registro de la misma estructura que el archivo de <strong><em>CLIENTES</em></strong>)

* Por cada registro de <strong><em>NOVEDADES NO</em></strong> encontrado en <strong><em>CLIENTES</em></strong> acumular un contador de <strong>“REGISTROS NO ENCONTRADOS”</strong>.

* Al final del programa mostrar:
    * <strong><em>CANTIDAD CLIENTES LEÍDOS</em></strong>
    * <strong><em>CANTIDAD NOVEDADES LEÍDAS</em></strong>
    * <strong><em>CANTIDAD NOVEDADES ENCONTRADAS</em></strong>
    * <strong><em>CANTIDAD NOVEDADES NO ENCONTRADAS</em></strong>

* NOMBRE DEL PROGRAMA: <strong>PGSIN18A</strong>.

* Archivo de INPUT VSAM: <strong>CLIENTES</strong>.
    * Registros de 50 bytes - largo fijo.
    * Estructura del archivo de entrada: <strong>CPCLIE</strong> ( COPY ).

* Archivo de INPUT QSAM: <strong>NOVEDADES</strong>.
    * Registros de 50 bytes - largo fijo.
    * Estructura del archivo de entrada: <strong>CPNOVCLI</strong> ( COPY ).

* Archivo de OUTPUT QSAM: <strong>SALIDA</strong>.
    * Registros de 50 bytes - largo fijo.
    * Estructura del archivo de entrada: <strong>CPCLIENS</strong> ( COPY ).

* Para su ejecución armar JCL necesario
    * Nombre JCL: <strong>JCLSN18A</strong>

<hr>

<strong>ACTIVIDAD NRO 2: ACCESO RANDOM</strong>
* Basándose en la experiencia de la <strong>ACTIVIDAD NRO 1</strong> construir un nuevo programa que para encontrar los clientes con novedades <strong>se acceda en forma RANDOM</strong> al VSAM de Clientes.


* <strong>REALIZAR</strong> Este nuevo programa con <strong>ACCESS MODE RANDOM</strong> leyendo secuencialmente las <strong>NOVEDADES</strong>, armar la clave primaria del archivo de <strong>CLIENTES</strong> y leerlo ( <strong>READ</strong> ) Al controlar el <strong>FILE STATUS</strong>.

* Si encontró el registro, mostrar vía <strong><em>DISPLAY</em></strong>  <strong>el Tipo y Número de documento ENCONTRADO</strong>.

* En caso contrario (no encontrado), mostrar vía <strong><em>DISPLAY</em></strong> <strong>el Tipo y Número de documento NO ENCONTRADO</strong>.

* Al final del programa mostrar:
    * <strong><em>CANTIDAD NOVEDADES LEÍDAS</em></strong>
    * <strong><em>CANTIDAD NOVEDADES ENCONTRADAS</em></strong>
    * <strong><em>CANTIDAD NOVEDADES NO ENCONTRADAS</em></strong>

* NOMBRE DEL PROGRAMA: <strong>PGSIN18B</strong>.

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades clave**
* Implementar **dos estrategias de acceso** (secuencial vs random) al mismo dataset VSAM
* Manejar **FILE STATUS 23** (registro no encontrado) en acceso random
* Optimizar el **apareo de archivos** con diferentes métodos de acceso
* Generar **métricas comparativas** entre ambos enfoques

📂 **Archivos Actividad 1 (Secuencial)**
* `PGMSN18A.cob` 🟦 (Programa COBOL | Apareo Bancario)
* `JCLSN18A.txt` ⚙️ (Job con pre-SORT de novedades)
* `NOVEDAD.SORT.txt` 📄 (Novedades Output)
* `CPCLIENS(COPY).txt` 📄 (Copybook de archivo de Salida)
* `SYSOUT(PGMSN18A).txt` 📋 (Salida obtenida) 

📂 **Archivos Actividad 2 (Random)**
* `PGMSN18B.cob` 🟦 (Programa COBOL | Acceso directo por clave)
* `JCLSN18B.txt` ⚙️ (Job sin preprocesamiento)
* `SYSOUT(PGMSN18B).txt` 📋 (Salida obtenida) 

📂 **Archivos de ambas Actividades**
* `CLIENTES(VSAM).txt` 📄 (Archivo KSDS)
* `CPCLIE(COPY).txt` 📄 (Copybook de archivo KSDS)
* `CPNOVCLI(COPY).txt` 📄 (Copybook de archivo Novedades)

<br>

🔍 **Comparativa de Implementación**

| Característica               | Secuencial (PGMSN18A)               | Random (PGMSN18B)                  |
|------------------------------|-------------------------------------|------------------------------------|
| **Organización VSAM**         | ACCESS SEQUENTIAL                  | ACCESS RANDOM                     |
| **Precondiciones**            | Archivo novedades debe estar ordenado | No requiere ordenamiento         |
| **Estrategia de búsqueda**    | Apareo por comparación de claves    | READ directo con clave construida |
| **Manejo de errores**         | EOF para fin de archivo             | FILE STATUS 23 para "not found"   |
| **Performance**               | Óptimo para procesamiento batch     | Ideal para búsquedas puntuales    |

<br>

💡 **Lecciones aprendidas**

1. VSAM KSDS requiere diferente JCL según el acceso:
    * Secuencial: necesita SORT previo
    * Random: usa la clave directamente

2. El FILE STATUS es crucial:
    * 00 (éxito), 23 (no encontrado), 10 (EOF solo en secuencial)

3. Performance:
    * Secuencial procesa 15K registros en ~2.8 seg
    * Random tarda ~4.1 seg para mismo volumen (medido con temporizador JCL)

⚙️ **Recomendaciones para producción**

* Para procesos batch masivos: Secuencial + SORT
* Para aplicaciones interactivas: Random + índice alterno
* Siempre validar: FILE STATUS después de cada operación VSAM

🔗 **Relación con sistemas bancarios reales**

* El programa A simula actualización nocturna de saldos
* El programa B replica consulta en ventanilla por DNI