      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMAPCAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-MAYO-08                                  
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD CLASE SINCRONICA 16 | PRIMER APAREO DE ARCHIVOS    *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA APAREA EL ARCHIVO MAESTRO CLIENTES CON EL ARCH- *
      *  -IVO MOVIMIENTOS PARA ACTUALIZAR LOS SALDOS.                  *
      *  SI EL MOVIMIENTO CORRESPONDE A UNA CUENTA INEXISTENTE EN EL   *
      *  ARCHIVO MAESTRO SE EMITIRA UN AVISO POR CONSOLA (DISPLAY).    *
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.                                           
      *----------------------------------------------------------------*
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.                                            
      *----------------------------------------------------------------*
       FILE-CONTROL.                                                    
                                                                        
           SELECT CLIENTES ASSIGN TO CLIENTES                           
                                    FILE STATUS IS FS-CLIENTES.         
                                                                        
           SELECT MOVTOS ASSIGN TO MOVTOS                               
                                    FILE STATUS IS FS-MOVTOS.           
                                                                        
           SELECT SALIDA ASSIGN TO SALIDA                               
                                    FILE STATUS IS FS-SALIDA.           
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
       FD   CLIENTES                                                    
            RECORDING MODE IS F.                                        
       01   REG-CLIENTES                                    PIC X(30).  
                                                                        
       FD   MOVTOS                                                      
            RECORDING MODE IS F.                                        
       01   REG-MOVTOS                                      PIC X(80).  
                                                                        
       FD   SALIDA                                                      
            RECORDING MODE IS F.                                        
       01   REG-SALIDA                                      PIC X(30).  
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMAPCAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-CLIENTES                   PIC X(08)  VALUE 'CLIENTES'.
           02 CT-MOVTOS                     PIC X(08)  VALUE 'MOVTOS  '.
           02 CT-SALIDA                     PIC X(08)  VALUE 'SALIDA  '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZ9.                    
                                                                        
      *----------------------------------------------------------------*
      *           A U X I L I A R E S  P A R A  E R R O R E S          *
      *----------------------------------------------------------------*
                                                                        
       01 AUXILIARES.                                                   
           02 W-N-ERROR                     PIC 9(02)  VALUE ZEROS.     
           02 AUX-ERR-TIPO                  PIC 9(02)  VALUE ZEROS.     
           02 AUX-ERR-ACCION                PIC X(10)  VALUE SPACES.    
           02 AUX-ERR-NOMBRE                PIC X(18)  VALUE SPACES.    
           02 AUX-ERR-STATUS                PIC X(04)  VALUE SPACES.    
           02 AUX-ERR-MENSAJE               PIC X(50)  VALUE SPACES.    
           02 AUX-ERR-RUTINA                PIC X(10)  VALUE SPACES.    
                                                                        
      *----------------------------------------------------------------*
      *                 A R E A  D E  C O N T A D O R E S              *
      *----------------------------------------------------------------*
                                                                        
       01 CNT-CONTADORES.                                               
           02 CNT-CLIENTES-LEIDOS           PIC 9(03)  VALUE ZEROS.     
           02 CNT-MOVTOS-LEIDOS             PIC 9(03)  VALUE ZEROS.     
           02 CNT-REG-GRABADOS              PIC 9(03)  VALUE ZEROS.     
           02 CNT-MOVTOS-REG                PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-CLIENTE.                                             
          02 CLIENTE-TIPO                   PIC 9(02)  VALUE ZEROS.     
          02 CLIENTE-CUENTA                 PIC 9(08)  VALUE ZEROS.     
                                                                        
       01 WS-CLAVE-MOVTO.                                               
          02 MOVTO-TIPO                     PIC 9(02)  VALUE ZEROS.     
          02 MOVTO-CUENTA                   PIC 9(08)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-CLIENTES                   PIC X(02).                  
              88 FS-CLIENTES-OK                        VALUE '00'.      
              88 FS-CLIENTES-EOF                       VALUE '10'.      
                                                                        
           02 FS-MOVTOS                     PIC X(02).                  
              88 FS-MOVTOS-OK                          VALUE '00'.      
              88 FS-MOVTOS-EOF                         VALUE '10'.      
                                                                        
           02 FS-SALIDA                     PIC X(02).                  
              88 FS-SALIDA-OK                          VALUE '00'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CLIENTE.                                                
                                                                        
           COPY MOVIMCC.                                                
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           IF FS-CLIENTES-OK                                            
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
                UNTIL FS-CLIENTES-EOF                                   
           END-IF.                                                      
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           INITIALIZE WS-VARIABLES                                      
                      CNT-CONTADORES                                    
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO.            
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS                                  
              THRU 1200-F-ABRIR-ARCHIVOS.                               
                                                                        
           PERFORM 1400-LEER-CLIENTES                                   
              THRU 1400-F-LEER-CLIENTES.                                
                                                                        
           PERFORM 1600-LEER-MOVTOS                                     
              THRU 1600-F-LEER-MOVTOS.                                  
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
              WHEN WS-CLAVE-CLIENTE = WS-CLAVE-MOVTO                    
                 PERFORM 2200-ACTUALIZAR-SALDO                          
                    THRU 2200-F-ACTUALIZAR-SALDO                        
                                                                        
                 PERFORM 1600-LEER-MOVTOS                               
                    THRU 1600-F-LEER-MOVTOS                             
                                                                        
              WHEN WS-CLAVE-CLIENTE > WS-CLAVE-MOVTO                    
                 DISPLAY ' '                                            
             DISPLAY '* CLAVE DE MOVIMIENTO NO ENCONTRADA EN CLIENTE *' 
                 DISPLAY ' - NRO DE MOVIMIENTO: ' WS-MOV-NRO            
                 DISPLAY ' - TIPO DE CUENTA: ' WS-MOV-TIPO              
                 DISPLAY ' - NRO DE CUENTA: ' WS-MOV-CUENTA             
                                                                        
                 PERFORM 1600-LEER-MOVTOS                               
                    THRU 1600-F-LEER-MOVTOS                             
                                                                        
              WHEN WS-CLAVE-CLIENTE < WS-CLAVE-MOVTO                    
                 PERFORM 2400-GRABAR-SALIDA                             
                    THRU 2400-F-GRABAR-SALIDA                           
                                                                        
                 PERFORM 1400-LEER-CLIENTES                             
                    THRU 1400-F-LEER-CLIENTES                           
           END-EVALUATE.                                                
                                                                        
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                       3 0 0 0 - F I N                          *
      *----------------------------------------------------------------*
                                                                        
       3000-FIN.                                                        
                                                                        
           MOVE '3000-FIN'                    TO WS-PARRAFO.            
                                                                        
           PERFORM 3200-CERRAR-ARCHIVOS                                 
              THRU 3200-F-CERRAR-ARCHIVOS.                              
                                                                        
           PERFORM 3400-MOSTRAR-TOTALES                                 
              THRU 3400-F-MOSTRAR-TOTALES.                              
                                                                        
       3000-F-FIN.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             M O D U L O S  S E C U N D A R I O S               *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *            1 2 0 0 - A B R I R - A R C H I V O S               *
      *----------------------------------------------------------------*
                                                                        
       1200-ABRIR-ARCHIVOS.                                             
                                                                        
           MOVE '1200-ABRIR-ARCHIVOS'         TO WS-PARRAFO.            
                                                                        
           OPEN INPUT   CLIENTES                                        
                        MOVTOS                                          
                OUTPUT  SALIDA.                                         
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-MOVTOS-OK                                          
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-MOVTOS                  TO AUX-ERR-NOMBRE         
              MOVE FS-MOVTOS                  TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-SALIDA-OK                                          
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-SALIDA                  TO AUX-ERR-NOMBRE         
              MOVE FS-SALIDA                  TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-ARCHIVOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 4 0 0 - L E E R - C L I E N T E S              *
      *----------------------------------------------------------------*
                                                                        
       1400-LEER-CLIENTES.                                              
                                                                        
           MOVE '1400-LEER-CLIENTES'          TO WS-PARRAFO.            
                                                                        
           READ CLIENTES INTO WS-REG-CLIENTE.                           
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-CLIENTES-OK                                      
                    ADD 1                     TO CNT-CLIENTES-LEIDOS    
                    MOVE WS-CLI-TIPO          TO CLIENTE-TIPO           
                    MOVE WS-CLI-CUENTA        TO CLIENTE-CUENTA         
                                                                        
               WHEN FS-CLIENTES-EOF                                     
                    SET FS-CLIENTES-EOF       TO TRUE                   
                    MOVE HIGH-VALUES          TO WS-CLAVE-CLIENTE       
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-CLIENTES          TO AUX-ERR-NOMBRE         
                    MOVE FS-CLIENTES          TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1400-F-LEER-CLIENTES.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 6 0 0 - L E E R - M O V T O S                  *
      *----------------------------------------------------------------*
                                                                        
       1600-LEER-MOVTOS.                                                
                                                                        
           MOVE '1400-LEER-MOVTOS'            TO WS-PARRAFO.            
                                                                        
           READ MOVTOS INTO WS-REG-MOVIMI.                              
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-MOVTOS-OK                                        
                    ADD 1                     TO CNT-MOVTOS-LEIDOS      
                    MOVE WS-MOV-TIPO          TO MOVTO-TIPO             
                    MOVE WS-MOV-CUENTA        TO MOVTO-CUENTA           
                                                                        
               WHEN FS-MOVTOS-EOF                                       
                    SET FS-MOVTOS-EOF         TO TRUE                   
                    MOVE HIGH-VALUES          TO WS-CLAVE-MOVTO         
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-MOVTOS            TO AUX-ERR-NOMBRE         
                    MOVE FS-MOVTOS            TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1600-F-LEER-MOVTOS.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *           2 2 0 0 - A C T U A L I Z A R - S A L D O            *
      *----------------------------------------------------------------*
                                                                        
       2200-ACTUALIZAR-SALDO.                                           
                                                                        
           MOVE '2200-ACTUALIZAR-SALDO'       TO WS-PARRAFO.            
                                                                        
           ADD 1                              TO CNT-MOVTOS-REG.        
                                                                        
           COMPUTE WS-CLI-SALDO = WS-CLI-SALDO + WS-MOV-IMPORTE.        
                                                                        
       2200-F-ACTUALIZAR-SALDO.                                         
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 4 0 0 - G R A B A R - S A L I D A              *
      *----------------------------------------------------------------*
                                                                        
       2400-GRABAR-SALIDA.                                              
                                                                        
           MOVE '2400-GRABAR-SALIDA'          TO WS-PARRAFO.            
                                                                        
           WRITE REG-SALIDA FROM WS-REG-CLIENTE.                        
                                                                        
           EVALUATE TRUE                                                
             WHEN FS-SALIDA-OK                                          
               ADD 1 TO CNT-REG-GRABADOS                                
                                                                        
              WHEN OTHER                                                
                MOVE CT-WRITE TO AUX-ERR-ACCION                         
                MOVE CT-SALIDA TO AUX-ERR-NOMBRE                        
                MOVE FS-SALIDA TO AUX-ERR-STATUS                        
                MOVE WS-PARRAFO TO AUX-ERR-MENSAJE                      
                MOVE 10 TO W-N-ERROR                                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                THRU 9000-F-SALIDA-ERRORES                              
           END-EVALUATE.                                                
                                                                        
       2400-F-GRABAR-SALIDA.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE CLIENTES                                               
                 MOVTOS                                                 
                 SALIDA.                                                
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-MOVTOS-OK                                          
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-MOVTOS                  TO AUX-ERR-NOMBRE         
              MOVE FS-MOVTOS                  TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-SALIDA-OK                                          
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-SALIDA                  TO AUX-ERR-NOMBRE         
              MOVE FS-SALIDA                  TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       3200-F-CERRAR-ARCHIVOS.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            3 4 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3400-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3400-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-CLIENTES-LEIDOS           TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMAPCAB               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REG. DE CLIENTES LEIDOS:                '         
                                                      WS-MASCARA'    *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-REG-GRABADOS              TO WS-MASCARA.            
           DISPLAY '* REG. GRABADOS EN LA SALIDA:             '         
                                                      WS-MASCARA'    *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-MOVTOS-LEIDOS             TO WS-MASCARA.            
           DISPLAY '* REG. DE MOVIMIENTOS LEIDOS:             '         
                                                      WS-MASCARA'    *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-MOVTOS-REG                TO WS-MASCARA.            
           DISPLAY '* REG. DE MOVIMIENTOS PROCESADOS:         '         
                                                      WS-MASCARA'    *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
                                                                        
       3400-F-MOSTRAR-TOTALES.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             9 0 0 0 - S A L I D A - E R R O R E S              *
      *----------------------------------------------------------------*
                                                                        
       9000-SALIDA-ERRORES.                                             
                                                                        
           MOVE '9000-SALIDA-ERRORES'         TO WS-PARRAFO.            
                                                                        
           DISPLAY '************************************' UPON CONSOLE  
           DISPLAY '*          PROGRAMA: ' CT-PROGRAMA    UPON CONSOLE  
           DISPLAY '************************************' UPON CONSOLE  
                                                                        
           EVALUATE W-N-ERROR                                           
               WHEN 10                                                  
                 DISPLAY ' ERROR DE ARCHIVO             ' UPON CONSOLE  
                 DISPLAY ' ACCION.....: ' AUX-ERR-ACCION  UPON CONSOLE  
                 DISPLAY ' ARCHIVO....: ' AUX-ERR-NOMBRE  UPON CONSOLE  
                 DISPLAY ' F-STATUS...: ' AUX-ERR-STATUS  UPON CONSOLE  
                 DISPLAY ' MENSAJE....: ' AUX-ERR-MENSAJE UPON CONSOLE  
           END-EVALUATE.                                                
                                                                        
           GOBACK.                                                      
                                                                        
       9000-F-SALIDA-ERRORES.                                           
           EXIT.                                                        