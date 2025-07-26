<div style="text-align:center">

<h3> Clase Asincrónica N° 9: Arreglos multidimensionales </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un programa con el nuevo concepto de <strong>ARREGLOS MULTIDIMENSIONALES</strong> EMBEBIDOS, en este caso, haciendo especial hincapié en el trabajo de una sola dimensión (<strong>VECTOR</strong>). 

<strong>ESPECIFICACIONES</strong>:
* Esta actividad propone trabajar con datos alineados según un orden específico:
    * Vector (1 dimensión)
    * Tabla (2 dimensiones)
    * Cubo (3 dimensiones)

* Son los únicos tipos de almacenamiento multidimensional embebido en código COBOL que son reconocidos por el compilador COBOL.
    * En esta práctica, en particular, se trabajará con un <strong>VECTOR</strong>.

* NOMBRE DEL PROGRAMA: <strong>PGMTACAB</strong>.

* Se construirá un vector dentro de la Working Storage del programa según las indicaciones
que se detallan:
    * ESTRUCTURA DEL ÍTEM DEL VECTOR:
        * CÓD-PRODUCTO PIC 99
        * DENOMINACIÓN PIC X(30)
        * PRECIO PIC 9(3)V99

* Leer los 10 productos (FILE = PRODUCTO) que se comercializarán a partir del próximo mes y cargarlos en un vector definido en Working. Está validado previamente que el archivo de PRODUCTOS tendrá 10 registros.

* Archivo de productos <strong>PRODUCT1</strong>.
    * Formato de registros: 
        * 01 REG-PRODUCTO.
            * 03 CÓD-PRODUCTO PIC 99.
            * 03 DENOMINACIÓN PIC X(30).

* Archivo de precios <strong>PRECIO</strong>.
    * Formato de registros. 
        * 01 REG-PRECIOS.
            * 03 CÓD-PRODUCTO PIC 99.
            * 03 PRECIO PIC 9(3)V99.

* Por cada registro de precio leído; buscar el producto en el vector:
    * Si se encuentra el PRODUCTO, agregar el precio en el ítem que corresponda.
    * Si no se encuentra el PRODUCTO mostrar por: DISPLAY ‘PRODUCTO NO ENCONTRADO’ y CÒD PRECIO.
    * Controlar el fin de archivo de precios. Si se llega al final del archivo de Precios y
no se encuentra el producto; realizar mismo DISPLAY que punto 2.

* Al final del programa, luego de haber leído todos los precios y completado los ITEMS del VECTOR, recorrer dicho vector (item x item) y mostrar (DISPLAY) todos los precios para cada producto (CÓDIGO DE PRODUCTO; DENOMINACIÓN; PRECIO).


</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Implementar **búsqueda secuencial** en arreglos COBOL
* Manejar **acoplamiento de datos** desde dos archivos distintos
* Optimizar el **mapeo productos-precios** en memoria

📂 **Archivos**  
* `PGMTACAB.cob` 🟦 (Programa COBOL | Vector embebido)  
* `JCLTACAB.txt` ⚙️ (Job para ejecución)  
* `PRODUCT1(QSAM).txt` 📁 (Catálogo de productos)  
* `PRECIO(QSAM).txt` 💰 (Listado de precios)  
* `SYSOUT.txt` 📋 (Salida obtenida)

📝 **Nota**
* Patrón esencial para:
    * Sistemas de inventarios
    * Catálogos de productos
    * Procesos de pricing

💡 **Técnica aprendida** 
* Uso de OCCURS con VECTORES.