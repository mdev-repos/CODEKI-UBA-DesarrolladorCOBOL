<div style="text-align:center">

<h3> Clase Sincrónica N° 16: Apareo </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Construir un programa con el nuevo concepto de APAREO de archivos.

<strong>ESPECIFICACIONES</strong>:
* Actualizar los saldos de las cuentas con los movimientos recibidos para un día determinado.

* NOMBRE DEL PROGRAMA: <strong>PGMAPCAB</strong>.

* Leer archivo <strong>CLIENTE</strong>.
    * Formato de registros. <strong>CLIENTE</strong> ( COPY ).

* Leer archivo de movimientos <strong>MOVIMICC</strong>.
    * Formato de registros. <strong>MOVIMCC</strong> ( COPY ).

* Aparear el archivo maestro de CLIENTE/CUENTA con los movimientos recibidos para un día determinado y actualizar el saldo.

* Si el movimiento corresponde a una clave CLIENTE/CUENTA no encontrada, emitir aviso por consola (DISPLAY). 

* Tener en cuenta que el campo importe del movimiento; tiene signo por lo tanto se reciben débitos (-) y créditos (+). 

* Indicar las precondiciones que requiere el programa para su ejecución.

* Para ejecutar, partir como esqueleto del programa <strong>JCLAPAR</strong> ( <em>JCL PROVISTO POR LA CÁTEDRA</em> ).

</div>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución</h3>

</div>

🎯 **Dificultades**
* Implementar lógica de **apareo** (match/merge) entre archivos ordenados
* Manejar **actualización de saldos** con importes con signo (+/-)
* Gestionar **registros huérfanos** (movimientos sin cliente)
* Garantizar **precondiciones** (archivos ordenados por clave)

📂 **Archivos**  
* `PGMAPCAB.cob` 🟦 (Programa COBOL | Apareo bancario)  
* `JCLAPCAB.txt` ⚙️ (Job con asignación de archivos)  
* `CLIENTE(COPY).txt` 📄 (Copybook maestro)  
* `MOVIMCC(COPY).txt` 📄 (Copybook de movimientos)  
* `CLIENTE(QSAM).txt` 📁 (Dataset maestro)  
* `MOVIMCC(QSAM).txt` 📁 (Dataset de transacciones)  
* `SALIDA(QSAM).txt` 💾 (Clientes actualizados)  
* `SYSOUT.txt` 📋 (Salida obtenida) 

💡 **Precondiciones** 
* Ambos archivos deben estar ordenados.
* Tipos de datos compatibles entre campos de apareo
* Secuencia abierta para escritura en archivo SALIDA

📝 **Nota**
* Ejercicio fundamental para:
    * Procesos batch bancarios (actualización nocturna de saldos)
    * Reconciliaciones entre sistemas
    * Migraciones de datos