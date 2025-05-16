      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMSN18A.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-MAYO-16                                  
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD CLASE SINCRONICA 18 | PGM CON VISAM ( ACT 01 )     *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA APAREA EL ARCHIVO VSAM KSDS CLIENTES ( CON CLA- *
      *  -VE 'TIPO Y NRO DE CDOCUMENTO' ) CON EL ARCHIVO QSAM ( NOVED- *
      *  -ADES ).                                                      *
      *  POR CADA REGISTRO DE NOVEDADES ENCONTRADO EN CLIENTES SE ACU- *
      *  -MULARA EN UNA VARIABLE (REG-ENCONTRADO) Y SE GRABARA ESE RE- *
      *  -GISTRO EN UN ARCHIVO DE SALIDA QSAM.                         *
      *  POR CADA REGISTRO NO ENCONTRADO, SE ACUMULARA EN UNA VARIABL- *
      *  -E (REG-NO-ENCONTRADO).                                       *
      *  AL FINALIZAR EL PROGRAMA, SE MOSTRARA:                        *
      *    - CANTIDAD CLIENTES LEÍDOS                                  *
      *    - CANTIDAD NOVEDADES LEÍDAS                                 *
      *    - CANTIDAD NOVEDADES ENCONTRADAS                            *
      *    - CANTIDAD NOVEDADES NO ENCONTRADAS                         *
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
                                                                        
           SELECT CLIENTES  ASSIGN TO CLIENTES                          
                            ORGANIZATION IS INDEXED                     
                            ACCESS IS SEQUENTIAL                        
                            RECORD KEY IS    KEY-CLAVE                  
                            FILE STATUS IS FS-CLIENTES.                 
                                                                        
           SELECT NOVEDAD   ASSIGN TO NOVEDAD                           
                            ACCESS IS SEQUENTIAL                        
                            FILE STATUS IS FS-NOVEDAD.                  
                                                                        
           SELECT SALIDA    ASSIGN TO SALIDA                            
                            FILE STATUS IS FS-SALIDA.                   
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      * CLIENTES ( ARCHIVO VSAM )                                       
                                                                        
       FD   CLIENTES.                                                   
                                                                        
       01 REG-CLIENTES.                                                 
          03 KEY-CLAVE    PIC X(13).                                    
          03 FILLER       PIC X(05).                                    
          03 CLI-CLAVE    PIC 9(03).                                    
          03 FILLER       PIC X(29).                                    
                                                                        
       FD   NOVEDAD                                                     
            RECORDING MODE IS F.                                        
       01   REG-NOVEDAD                                     PIC X(50).  
                                                                        
       FD   SALIDA                                                      
            RECORDING MODE IS F.                                        
       01   REG-SALIDA                                      PIC X(50).  
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMSN18A'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-CLIENTES                   PIC X(08)  VALUE 'CLIENTES'.
           02 CT-NOVEDAD                    PIC X(08)  VALUE 'NOVEDAD '.
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
           02 CNT-NOVEDAD-LEIDOS            PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-ENCONTRADOS       PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-NO-ENCONTRADOS    PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-CLIENTE.                                             
          02 CLIENTE-TIPO-DOC               PIC X(02)  VALUE ZEROS.     
          02 CLIENTE-NRO-DOC                PIC 9(11)  VALUE ZEROS.     
                                                                        
       01 WS-CLAVE-NOVEDAD.                                             
          02 NOVEDAD-TIPO-DOC               PIC X(02)  VALUE ZEROS.     
          02 NOVEDAD-NRO-DOC                PIC 9(11)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-CLIENTES                   PIC X(02).                  
              88 FS-CLIENTES-OK                        VALUE '00'.      
              88 FS-CLIENTES-EOF                       VALUE '10'.      
                                                                        
           02 FS-NOVEDAD                    PIC X(02).                  
              88 FS-NOVEDAD-OK                         VALUE '00'.      
              88 FS-NOVEDAD-EOF                        VALUE '10'.      
                                                                        
           02 FS-SALIDA                     PIC X(02).                  
              88 FS-SALIDA-OK                          VALUE '00'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CPCLIE.                                                 
           COPY CPNOVCLI.                                               
           COPY CPCLIENS.                                               
                                                                        
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
                                                                        
           PERFORM 1600-LEER-NOVEDAD                                    
              THRU 1600-F-LEER-NOVEDAD.                                 
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
              WHEN WS-CLAVE-CLIENTE = WS-CLAVE-NOVEDAD                  
                 ADD 1 TO CNT-NOVEDAD-ENCONTRADOS                       
                                                                        
                 PERFORM 2200-GRABAR-SALIDA                             
                    THRU 2200-F-GRABAR-SALIDA                           
                                                                        
                 PERFORM 1600-LEER-NOVEDAD                              
                    THRU 1600-F-LEER-NOVEDAD                            
                                                                        
              WHEN WS-CLAVE-CLIENTE > WS-CLAVE-NOVEDAD                  
                 DISPLAY ' '                                            
             DISPLAY '* CLAVE DE NOVEDAD NO ENCONTRADA EN CLIENTE *'    
                 ADD 1 TO CNT-NOVEDAD-NO-ENCONTRADOS                    
                                                                        
                 DISPLAY ' - TIPO DE DOCUMENTO: ' NOV-TIP-DOC           
                 DISPLAY ' - NRO DE DOCUMENTO: ' NOV-NRO-DOC            
                                                                        
                 PERFORM 1600-LEER-NOVEDAD                              
                    THRU 1600-F-LEER-NOVEDAD                            
                                                                        
              WHEN WS-CLAVE-CLIENTE < WS-CLAVE-NOVEDAD                  
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
                        NOVEDAD                                         
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
                                                                        
           IF NOT FS-NOVEDAD-OK                                         
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-NOVEDAD                 TO AUX-ERR-NOMBRE         
              MOVE FS-NOVEDAD                 TO AUX-ERR-STATUS         
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
                                                                        
           READ CLIENTES INTO REG-CLIENTE.                              
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-CLIENTES-OK                                      
                    ADD 1                     TO CNT-CLIENTES-LEIDOS    
                    MOVE CLI-TIP-DOC          TO CLIENTE-TIPO-DOC       
                    MOVE CLI-NRO-DOC          TO CLIENTE-NRO-DOC        
                                                                        
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
      *               1 6 0 0 - L E E R - N O V E D A D                *
      *----------------------------------------------------------------*
                                                                        
       1600-LEER-NOVEDAD.                                               
                                                                        
           MOVE '1600-LEER-NOVEDAD'           TO WS-PARRAFO.            
                                                                        
           READ NOVEDAD INTO WS-REG-NOVCLIE.                            
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-NOVEDAD-OK                                       
                    ADD 1                     TO CNT-NOVEDAD-LEIDOS     
                    MOVE NOV-TIP-DOC          TO NOVEDAD-TIPO-DOC       
                    MOVE NOV-NRO-DOC          TO NOVEDAD-NRO-DOC        
                                                                        
               WHEN FS-NOVEDAD-EOF                                      
                    SET FS-NOVEDAD-EOF        TO TRUE                   
                    MOVE HIGH-VALUES          TO WS-CLAVE-NOVEDAD       
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-NOVEDAD           TO AUX-ERR-NOMBRE         
                    MOVE FS-NOVEDAD           TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1600-F-LEER-NOVEDAD.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 2 0 0 - G R A B A R - S A L I D A              *
      *----------------------------------------------------------------*
                                                                        
       2200-GRABAR-SALIDA.                                              
                                                                        
           MOVE '2200-GRABAR-SALIDA'          TO WS-PARRAFO.            
                                                                        
           WRITE REG-SALIDA FROM WS-REG-NOVCLIE.                        
                                                                        
           IF NOT FS-SALIDA-OK                                          
              MOVE CT-WRITE TO AUX-ERR-ACCION                           
              MOVE CT-SALIDA TO AUX-ERR-NOMBRE                          
              MOVE FS-SALIDA TO AUX-ERR-STATUS                          
              MOVE WS-PARRAFO TO AUX-ERR-MENSAJE                        
              MOVE 10 TO W-N-ERROR                                      
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
              THRU 9000-F-SALIDA-ERRORES                                
           END-IF.                                                      
                                                                        
       2200-F-GRABAR-SALIDA.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE CLIENTES                                               
                 NOVEDAD                                                
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
                                                                        
           IF NOT FS-NOVEDAD-OK                                         
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-NOVEDAD                 TO AUX-ERR-NOMBRE         
              MOVE FS-NOVEDAD                 TO AUX-ERR-STATUS         
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
           DISPLAY '*                PROGRAMA PGMSN18A               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
                                                                        
           DISPLAY '* REG. DE CLIENTES LEIDOS:                '         
                                                     WS-MASCARA '    *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-NOVEDAD-LEIDOS            TO WS-MASCARA.            
           DISPLAY '* REG. DE NOVEDAD LEIDOS:                 '         
                                                     WS-MASCARA '    *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-NOVEDAD-ENCONTRADOS       TO WS-MASCARA.            
           DISPLAY '* NOVEDADES ENCONTRADAS:                  '         
                                                     WS-MASCARA '    *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-NOVEDAD-NO-ENCONTRADOS    TO WS-MASCARA.            
           DISPLAY '* NOVEDADES NO ENCONTRADAS:               '         
                                                     WS-MASCARA '    *'.
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