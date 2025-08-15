<div style="text-align:center">

<h3> Introducción a CICS: Programa Básico de Mensajería </h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar primer programa <strong>CICS</strong> en COBOL
* Enviar mensaje a pantalla usando <strong>EXEC CICS SEND</strong>
* Comprender estructura básica de transacción pseudo-conversacional

Al finalizar esta práctica; el estudiante habrá creado su primer programa CICS funcional con capacidad de comunicación con el usuario.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa CICS para demostración de envío de mensajes a terminal.

* TRANSACCIÓN: <strong>ECAB</strong> (Personalizada con user ID)
* PROGRAMA: <strong>PGMBACAB</strong>
* TECNOLOGÍA: <strong>CICS COBOL</strong>
* FUNCIÓN PRINCIPAL:
    * Mostrar mensaje "HOLA CICS!!!!"
    * Utilizar verbos CICS básicos
    * Estructura pseudo-conversacional

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Solución Implementada</h3>

</div>

🎯 **Dificultades**
* Primer contacto con <strong>entorno CICS</strong>
* Configuración de <strong>transacción personalizada</strong>
* Comprensión de <strong>flujo pseudo-conversacional</strong>
* Manejo de <strong>áreas de comunicación</strong> CICS

📂 **Archivos de Solución**  
* `PGMBACAB.cob` 🖥️ (Programa COBOL-CICS)  
* `TRA(ECAB)-01.png` 📷 (Captura pantalla ejecución)  
* `TRA(ECAB)-02.png` 📷 (Captura pantalla resultados)  

💻 **Código Clave**
```cobol
2000-PROCESO.                              
                                           
    EXEC CICS                              
         SEND TEXT                         
         FROM  (WS-MENS)                   
         LENGTH(WS-LONG)                   
         ERASE                             
    END-EXEC.                              
                                           
2000-F-PROCESO.                            
    EXIT.                                  