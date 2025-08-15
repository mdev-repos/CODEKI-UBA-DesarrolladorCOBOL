<div style="text-align:center">

<h3>Práctica CICS: Consulta de Clientes VSAM</h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar programa CICS para <strong>consultar registros</strong> en archivo VSAM
* Desarrollar <strong>mapa interactivo</strong> con validación de datos
* Manejar <strong>errores</strong> y casos especiales (campos vacíos/inválidos)

Al finalizar esta práctica; el estudiante dominará operaciones de lectura VSAM en entorno CICS con manejo profesional de errores.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa de consulta de clientes con integración al menú principal.

* TRANSACCIÓN: <strong>ACAB</strong> (Personalizada)
* PROGRAMA: <strong>PGMPRCAB</strong>
* MAPA: <strong>MAP1CAB</strong> (Generado con BMS)
* ARCHIVO VSAM: <strong>KC03CAB.CURSOS.PERSONA.KSDS.VSAM</strong>
* ESTRUCTURA: <strong>CPPERSON</strong> (COPYBOOK)
* FUNCIONALIDAD:
    * Consulta por tipo y número de documento
    * Validación de datos de entrada
    * Manejo de errores VSAM
    * Integración con menú principal (PF4)

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Implementación Técnica</h3>

</div>

🎯 **Componentes Clave**
* <strong>Mapeo de teclas</strong> (PF4 para invocar desde menú)
* <strong>Validación de campos</strong> antes de consulta VSAM
* <strong>Manejo de estados</strong> de archivo (00, 10, 23)

📂 **Archivos de Solución**  
* `PGMPRCAB.cob` 🖥️ (Programa COBOL-CICS)  
* `MAP1CAB.asm` 🗺️ (Definición del mapa BMS)  
* `TRA(ACAB)-01.png` 📷 (Captura de pantalla funcional)  

💻 **Estructura del Programa**
```cobol
 PROGRAMA PGMMECAB --> OPCION PF4 (CONSULTA) --> XCTL A PGMPRCAB
 
 2240-PF4.                                                       
                                                                 
*    PF4 --> TECLA DE CONSULTA DE CLIENTE                        
                                                                 
*    HACER XCTL A LA TRANSACCIóN ACAB. PROGRAMA PGMPRCAB.        
                                                                 
     MOVE TIPDOCI TO WS-USER-TIPDOC.                             
                                                                 
     MOVE NUMDOCI TO WS-USER-NRODOC.                             
                                                                 
     MOVE TIPDOCI TO WS-TIP-DOC.                                 
                                                                 
     EXEC CICS XCTL                                              
         PROGRAM('PGMPRCAB')                                     
         COMMAREA(WS-COMMAREA)                                   
     END-EXEC.                                                   
                                                                 
 2240-F-PF4.                                                     
     EXIT.                                                       


 PGMPRCAB --> READ DATASET --> RETURN TRANSID (BCAB)
 
 1000-INICIO.                              
                                           
     MOVE DFHCOMMAREA TO WS-COMMAREA.      
                                           
     INITIALIZE WS-RESP.                   
                                           
*    READ DEL ARCHIVO VSAM PERSONA (CLIENTES) 
                                           
     EXEC CICS                             
          READ DATASET (CT-DATASET)        
          RIDFLD  (WS-USER-DATA)           
          INTO (REG-PERSONA)               
          LENGTH (CT-DATASET-LEN)          
          EQUAL                            
          RESP (WS-RESP)                   
     END-EXEC.                             
                                           
 1000-F-INICIO.                            
     EXIT.                                 