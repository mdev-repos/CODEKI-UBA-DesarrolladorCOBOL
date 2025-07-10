      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMB2CAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-07                                 
                                                                        
      *----------------------------------------------------------------*
      * ACTIVIDAD CLASE ASINCRONICA 16 | DB2 CURSOR + CORTE DE CONTROL *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PGM UTILIZA UN CURSOR PARA EJECUTAR UNA QUERY, RECUPERAR  *
      * UNA LISTA ORDENADA POR SUCURSALES Y REALIZA UN CORTE DE CON-   *
      * -TROL. AL FINALIZAR EL PGM POR SYSOUT SE OBTIENE:              *
      *    - LA CANTIDAD DE CUENTAS POR SUCURSAL.                      *
      *    - EL TOTAL DE CUENTAS TRAIDAS DE BBDD.                      *
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.                                           
      *----------------------------------------------------------------*
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
                                                                        
      *----------------------------------------------------------------*
      *INPUT-OUTPUT SECTION.                                            
      *----------------------------------------------------------------*
      *FILE-CONTROL.                                                    
                                                                        
      * PGM SIN ARCHIVOS.                                               
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
      *FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMB2CAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-CURSOR                     PIC X(08)  VALUE 'CURSOR  '.
           02 CT-FETCH                      PIC X(08)  VALUE 'FETCH   '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE  +100. 
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZ9.                    
                                                                        
       01 EF-END-FETCH.                                                 
           02 EF-SUCURSAL                   PIC X(02).                  
              88 EF-SUCURSAL-FALSE                     VALUE '00'.      
              88 EF-SUCURSAL-TRUE                      VALUE '10'.      
                                                                        
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
           02 CNT-REGISTROS-LEIDOS          PIC 9(03)  VALUE ZEROS.     
           02 CNT-PARCIAL-SUCURSAL          PIC 9(03)  VALUE ZEROS.     
           02 CNT-TOTAL-SUCURSAL            PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-ACT.                                                 
           02 WS-SUC-ACT           PIC S9(2)V USAGE COMP-3  VALUE ZEROS.
                                                                        
       01 WS-CLAVE-ANT.                                                 
           02 WS-SUC-ANT           PIC S9(2)V USAGE COMP-3  VALUE ZEROS.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
      * PGM SIN LECTURA / ESCRITURA DE ARCHIVOS                         
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
      * PGM SIN COPYS                                                   
                                                                        
      *----------------------------------------------------------------*
      *                      S Q L C A  |  D B 2                       *
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
      *          I N C L U D E  D E  T A B L A S  |  D B 2             *
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
             INCLUDE TBCURCTA                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TBCURCLI                                           
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
      *      D E C L A R A C I O N  D E  C U R S O R  |  D B 2         *
      *----------------------------------------------------------------*
                                                                        
           EXEC SQL                                                     
             DECLARE ITEM_CURSOR CURSOR FOR                             
               SELECT A.TIPCUEN,                                        
                      A.NROCUEN,                                        
                      A.SUCUEN,                                         
                      A.NROCLI,                                         
                      B.NOMAPE,                                         
                      A.SALDO,                                          
                      A.FECSAL                                          
               FROM KC02787.TBCURCTA A                                  
               INNER JOIN KC02787.TBCURCLI B                            
                 ON A.NROCLI = B.NROCLI                                 
               WHERE A.SALDO > 0                                        
               ORDER BY A.SUCUEN                                        
           END-EXEC.                                                    
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           IF EF-SUCURSAL-FALSE                                         
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
                UNTIL EF-SUCURSAL-TRUE                                  
           END-IF.                                                      
                                                                        
           PERFORM 2400-MOSTRAR-ULTIMO                                  
              THRU 2400-F-MOSTRAR-ULTIMO.                               
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           INITIALIZE WS-VARIABLES                                      
                      CNT-CONTADORES                                    
                                                                        
           MOVE '00' TO EF-SUCURSAL.                                    
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO.            
                                                                        
           PERFORM 1200-ABRIR-CURSOR                                    
              THRU 1200-F-ABRIR-CURSOR.                                 
                                                                        
           PERFORM 2200-FETCH-CURSOR                                    
              THRU 2200-F-FETCH-CURSOR.                                 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT.          
                                                                        
           MOVE WS-SUC-ACT                    TO WS-MASCARA.            
                                                                        
           DISPLAY 'SUCURSAL ' WS-MASCARA.                              
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
             WHEN WS-CLAVE-ACT IS EQUAL TO WS-CLAVE-ANT                 
               ADD 1 TO CNT-PARCIAL-SUCURSAL                            
               ADD 1 TO CNT-TOTAL-SUCURSAL                              
                                                                        
             WHEN WS-CLAVE-ACT IS NOT EQUAL TO WS-CLAVE-ANT             
               MOVE CNT-PARCIAL-SUCURSAL TO WS-MASCARA                  
               DISPLAY "  -- CANTIDAD DE CUENTAS: " WS-MASCARA          
               MOVE ZEROS           TO CNT-PARCIAL-SUCURSAL             
                                                                        
               ADD 1 TO CNT-PARCIAL-SUCURSAL                            
               ADD 1 TO CNT-TOTAL-SUCURSAL                              
                                                                        
               MOVE WS-CLAVE-ACT    TO WS-CLAVE-ANT                     
                                                                        
               MOVE WS-SUC-ACT      TO WS-MASCARA                       
               DISPLAY ' '                                              
               DISPLAY 'SUCURSAL ' WS-MASCARA                           
           END-EVALUATE.                                                
                                                                        
           PERFORM 2200-FETCH-CURSOR                                    
              THRU 2200-F-FETCH-CURSOR.                                 
                                                                        
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                       3 0 0 0 - F I N                          *
      *----------------------------------------------------------------*
                                                                        
       3000-FIN.                                                        
                                                                        
           MOVE '3000-FIN'                    TO WS-PARRAFO.            
                                                                        
           PERFORM 3200-CERRAR-CURSOR                                   
              THRU 3200-F-CERRAR-CURSOR.                                
                                                                        
           PERFORM 3400-MOSTRAR-TOTALES                                 
              THRU 3400-F-MOSTRAR-TOTALES.                              
                                                                        
       3000-F-FIN.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             M O D U L O S  S E C U N D A R I O S               *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               1 2 0 0 - A B R I R - C U R S O R                *
      *----------------------------------------------------------------*
                                                                        
       1200-ABRIR-CURSOR.                                               
                                                                        
           MOVE '1200-ABRIR-CURSOR'           TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
              OPEN ITEM_CURSOR                                          
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              SET  EF-SUCURSAL-TRUE     TO TRUE                         
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-OPEN              TO AUX-ERR-ACCION               
              MOVE CT-CURSOR            TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-CURSOR.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 2 0 0 - F E T C H - C U R S O R                *
      *----------------------------------------------------------------*
                                                                        
       2200-FETCH-CURSOR.                                               
                                                                        
           MOVE '2200-FETCH-CURSOR'           TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
              FETCH  ITEM_CURSOR                                        
                     INTO                                               
                        :DCLTBCURCTA.WS-TIPCUEN,                        
                        :DCLTBCURCTA.WS-NROCUEN,                        
                        :DCLTBCURCTA.WS-SUCUEN,                         
                        :DCLTBCURCTA.WS-NROCLI,                         
                        :DCLTBCURCLI.WS-CLI-NOMAPE,                     
                        :DCLTBCURCTA.WS-SALDO,                          
                        :DCLTBCURCTA.WS-FECSAL                          
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE = 0                                           
               ADD 1 TO CNT-REGISTROS-LEIDOS                            
               MOVE WS-SUCUEN            TO WS-CLAVE-ACT                
                                                                        
             WHEN SQLCODE EQUAL +100                                    
               SET  EF-SUCURSAL-TRUE     TO TRUE                        
                                                                        
             WHEN OTHER                                                 
               SET  EF-SUCURSAL-TRUE     TO TRUE                        
               MOVE SQLCODE              TO CT-SQLCODE-EDIT             
                                                                        
               MOVE CT-FETCH             TO AUX-ERR-ACCION              
               MOVE CT-CURSOR            TO AUX-ERR-NOMBRE              
               MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS              
               MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE             
               MOVE 10                   TO W-N-ERROR                   
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
           END-EVALUATE.                                                
                                                                        
       2200-F-FETCH-CURSOR.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 4 0 0 - M O S T R A R - U L T I M O              *
      *----------------------------------------------------------------*
                                                                        
       2400-MOSTRAR-ULTIMO.                                             
                                                                        
           MOVE '2400-MOSTRAR-ULTIMO'         TO WS-PARRAFO.            
                                                                        
           MOVE CNT-PARCIAL-SUCURSAL          TO WS-MASCARA.            
                                                                        
           DISPLAY "  -- CANTIDAD DE CUENTAS: " WS-MASCARA.             
           DISPLAY ' '.                                                 
           DISPLAY ' '.                                                 
                                                                        
           MOVE CNT-TOTAL-SUCURSAL            TO WS-MASCARA.            
           DISPLAY '-----------------------------'.                     
           DISPLAY '         TOTAL DE CUENTAS ' WS-MASCARA.             
           DISPLAY '-----------------------------'.                     
           DISPLAY ' '.                                                 
                                                                        
       2400-F-MOSTRAR-ULTIMO.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               3 2 0 0 - C E R R A R - C U R S O R              *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-CURSOR.                                              
                                                                        
           MOVE '3200-CERRAR-CURSOR'          TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
              CLOSE ITEM_CURSOR                                         
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-CLOSE             TO AUX-ERR-ACCION               
              MOVE CT-CURSOR            TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
       3200-F-CERRAR-CURSOR.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            3 4 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3400-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3400-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-REGISTROS-LEIDOS          TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMB2CAB               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* CANTIDAD DE REGISTROS LEIDOS:           '         
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