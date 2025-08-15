<div style="text-align:center">

<h3>Clase Asincrónica 21: Desarrollo de Menú CICS</h3>

</div>

<div style="text-align:justify">

<strong>OBJETIVO</strong>: 
* Implementar programa <strong>CICS</strong> con estructura de menú interactivo
* Crear y vincular <strong>mapa BMS</strong> con opciones modificables
* Manejar <strong>diferentes transacciones</strong> desde un programa central

Al finalizar esta práctica; el estudiante dominará la creación de interfaces básicas en CICS usando mapas BMS y flujos pseudo-conversacionales.

<br>

<strong>ESPECIFICACIONES</strong>:  
Programa menú para sistema de gestión de clientes con múltiples funcionalidades.

* TRANSACCIÓN: <strong>BCAB</strong> (Personalizada con user ID)
* PROGRAMA: <strong>PGMMECAB</strong>
* MAPA: <strong>MAP2CAB</strong> (Generado con BMS)
* FUNCIONALIDADES:
    * Alta de clientes
    * Baja de clientes
    * Modificación de datos
    * Consulta de información
    * Limpieza de pantalla
    * Salida del sistema

</div>

<br>

<hr>

<div style="text-align:center">

<h3>🛠️ Implementación</h3>

</div>

🎯 **Componentes Clave**
* <strong>Mapa BMS</strong> con campos editables (amarillo) y mensajes de error
* <strong>Lógica de control</strong> para cada opción del menú
* <strong>Comunicación pseudo-conversacional</strong> entre transacciones

📂 **Archivos de Solución**  
* `PGMMECAB.cob` 🖥️ (Programa principal COBOL-CICS)  
* `MAP2CAB.asm` 🗺️ (Definición del mapa en Assembler BMS)  
* `TRA(BCAB)-01.png` 📷 (Captura del menú funcional)  

💻 **Estructura del Programa**
```cobol
 1200-SEND-MAP.                          
                                         
     EXEC CICS                           
          SEND MAP    (WS-MAP)           
               MAPSET (WS-MAPSET)        
               FROM   (MAP2CABO)         
               LENGTH (WS-LONG)          
               ERASE                     
               FREEKB                    
               RESP   (WS-RESP)          
     END-EXEC.                           
                                         
     EVALUATE WS-RESP                    
        WHEN DFHRESP(NORMAL)             
           EXEC CICS                     
              RETURN                     
              TRANSID  ('BCAB')          
              COMMAREA (WS-COMMAREA)     
           END-EXEC.
     ---- CONTINUA EVALUACION....
                 
 1400-RECEIVE-MAP.                        
                                         
    EXEC CICS                            
         RECEIVE MAP (WS-MAP)            
                 MAPSET (WS-MAPSET)      
                 INTO (MAP2CABI)         
                 RESP(WS-RESP)           
    END-EXEC.                            
                                         
    EVALUATE WS-RESP                     
      WHEN DFHRESP(NORMAL)               
         CONTINUE                                        
    ---- CONTINUA EVALUACION....