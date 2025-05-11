<div style="text-align:center">

<h3>Clase Sincrónica N° 12: Corte de control (2 claves de corte) </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Modificar el  programa de corte de control por SUCURSAL agregando otro 
corte de control por TIPO DE CUENTA y ejecutarlo exitosamente.  

<strong>ESPECIFICACIONES</strong>:

* NOMBRE DEL PROGRAMA: <strong>PGMSIN12</strong>.
* Modificar el PGM anterior <strong>PGM2CCAB</strong>.
* Leer secuencialmente los registros del archivo <strong>CORTE</strong>.
    * Cada registro del archivo es de largo fijo de 20 bytes de largo.
    * Estructura del archivo de entrada: <strong>CORTE</strong> ( COPY ). 

* Con los registros procesados ya se realizo corte por el campo <strong>WS-SUC-NRO</strong>. Ahora, agregar corte de control por tipo de cuenta (campo <strong>WS-SUC-TIPC1</strong>). 
*  Al final de cada corte de SUCURSAL, MOSTRAR (<strong>DISPLAY</strong>): Número de sucursal e importe total junto a la sumatoria de importes correspondientes a dicha sucursal (campo <strong> WS-SUC-IMPORTE</strong>).  Con máscara de edición que elimine los ceros no significativos.
    * Ejemplo:

|||    
| -----| -------------|
|Sucursal 1|1.000.000|
|Sucursal 2|21.148|
|Sucursal 3|33.458|

*  Al final de cada corte de tipo de cuenta; MOSTRAR (DISPLAY): tipo de cuenta junto a la sumatoria de importes correspondientes a dicho tipo de cuenta (campo <strong>WS-SUC-IMPORTE</strong>).  Con máscara de edición que elimine los ceros no significativos.

    * Ejemplo:

||||    
|--|--|--|
|Sucursal 1|||
||Tipo de Cuenta 1|500.000|
||Tipo de Cuenta 2|1.000.000|
|||1.500.000|
|Sucursal 2|||
||Tipo de Cuenta 1|20.000|
||Tipo de Cuenta 2|1.148|
|||21.148|
|Sucursal 3|||
||Tipo de Cuenta 1|31.000|
||Tipo de Cuenta 2|2.458|
|||33.458|

* Al final del programa MOSTRAR: el total general de importes para el archivo procesado. 
Sumatoria de WS-SUC-IMPORTE. Con máscara de edición que elimine los ceros no 
significativos. 
    * Ejemplo:

|TOTAL GENERAL|1.054.606|    
| -----| -------------|

* Para ejecutar, partir como esqueleto del programa <strong>JCLCORT1</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**  
* Implementar **corte de control jerárquico** (sucursal > tipo de cuenta) manteniendo la lógica de totalización en ambos niveles.
* Gestionar **saltos de control anidados** al detectar cambios en cualquiera de las dos claves.
* Asegurar el formato correcto de los **DISPLAY** con sangrías para reflejar la jerarquía de datos.

📂 **Archivos**  
* `PGMSIN12.cob` 🟦 (Programa con corte de 2 niveles)  
* `JCLSIN12.txt` ⚙️ (Job para ejecución en Mainframe)  
* `CORTE(COPY).txt` 📄 (Copybook con estructura del registro)  
* `CORTE(QSAM).txt` 📁 (Dataset de entrada - QSAM)  
* `SYSOUT.txt` 📋 (Ejemplo de salida jerárquica) 

📝 **Nota**
* Este ejercicio simula reportes bancarios reales (ej: balances por sucursal y tipo de producto). Clave para entender cómo procesar datos jerárquicos en entornos legacy.

💡 **Truco aprendido**
* Usar EVALUATE para gestionar múltiples cortes es más limpio que anidar IFs.