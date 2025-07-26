      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMSIN31.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-10.                                
                                                                        
      *----------------------------------------------------------------*
      *     ACTIVIDAD CLASE SINCRONICA 31 | APAREO CON CURSOR DB2      *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PROGRAMA APAREA EL ARCHIVO KC02788.ALU9999.NOVCTA CON UN  *
      * CURSOR DE LA TABLA TBCURCTA ( SOLO SUCURSAL 1 ).               *
      *   - CUANDO HAY APAREO OK:                                      *
      *     + BUSCA EL CLIENTE EN LA TABLA TBCURCLI. SI NO LO ENCUEN-  *
      * -TRA DISPLAY 'CLIENTE NO ENCONTRADO'. SI LO ENCUENTRA SE SUMA  *
      * EL MOVIMIENTO DE SALDO DEL ARCHIVO AL SALDO DE LA CUENTA EN LA *
      * TABLA Y HACE DISPLAY DE: TIPO DE CUENTA | NRO. DE CUENTA | NRO.*
      * - CLI | NOMBRE DEL CLIENTE | SALDO ACTUALIZADO.                *
      *   - CUANDO EL APAREO ESTA EN EL ARCHIVO PERO NO EN LA TABLA:   *
      * DISPLAY 'NOVEDAD NO ENCONTRADA'.                               *
      *   - CUANDO EL APAREO NO ESTA EN EL ARCHIVO PERO SI EN AL TABLA *
      * 'CUENTA SIN NOVEDAD'.                                          *
      *                                                                *
      * AL FINAL DEL PGM MUESTRA ESTADISTICA DE REGISTROS LEIDOS, CLI- *
      * -ENTES ENCONTRADOS Y NO ENCONTRADOS.                           *
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
                                                                        
           SELECT NOVEDAD ASSIGN TO NOVEDAD                             
                                    FILE STATUS IS FS-NOVEDAD.          
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
       FD   NOVEDAD                                                     
            RECORDING MODE IS F.                                        
       01   REG-NOVEDAD                                     PIC X(23).  
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMSIN31'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-EVALUATE                   PIC X(08)  VALUE 'EVALUATE'.
           02 CT-NOVEDAD                    PIC X(08)  VALUE 'NOVEDAD '.
           02 CT-CURSOR                     PIC X(08)  VALUE 'CURSOR  '.
           02 CT-FETCH                      PIC X(08)  VALUE 'FETCH   '.
           02 CT-QUERY                      PIC X(08)  VALUE 'QUERY   '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE +100.  
           02 CT-FOUND                      PIC S9(9) COMP VALUE 0.     
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZZ9.                   
           02 WS-SALDO-EDIT                 PIC $$$$.$$9,00-.           
           02 WS-SALDO-ACT                  PIC S9(5)V9(2) USAGE COMP-3.
           02 WS-QUERY-OK                   PIC 9      VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *           A U X I L I A R E S  P A R A  E R R O R E S          *
      *----------------------------------------------------------------*
                                                                        
       01 AUXILIARES.                                                   
           02 W-N-ERROR                     PIC 9(02)  VALUE ZEROS.     
           02 AUX-ERR-TIPO                  PIC 9(02)  VALUE ZEROS.     
           02 CT-NOVEDAD                    PIC X(08)  VALUE 'NOVEDAD '.
           02 CT-CURSOR                     PIC X(08)  VALUE 'CURSOR  '.
           02 CT-FETCH                      PIC X(08)  VALUE 'FETCH   '.
           02 CT-QUERY                      PIC X(08)  VALUE 'QUERY   '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE +100.  
           02 CT-FOUND                      PIC S9(9) COMP VALUE 0.     
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZZ9.                   
           02 WS-SALDO-EDIT                 PIC $$$$.$$9,00-.           
           02 WS-SALDO-ACT                  PIC S9(5)V9(2) USAGE COMP-3.
           02 WS-QUERY-OK                   PIC 9      VALUE ZEROS.     
                                                                        
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
           02 CNT-NOV-LEIDA                 PIC 9(03)  VALUE ZEROS.     
           02 CNT-CLI-ENC                   PIC 9(03)  VALUE ZEROS.     
           02 CNT-CLI-NOENC                 PIC 9(03)  VALUE ZEROS.     
           02 CNT-SALDO-ACT                 PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-NOVEDAD.                                             
          02 NOV-TIPCUEN                    PIC X(02)  VALUE ZEROS.     
                                                                        
       01 WS-CLAVE-CURSOR.                                              
          02 CUR-TIPCUEN                    PIC X(02)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
           02 FS-NOVEDAD                    PIC X(02).                  
              88 FS-NOVEDAD-OK                         VALUE '00'.      
              88 FS-NOVEDAD-EOF                        VALUE '10'.      
                                                                        
           02 CS-CURSOR                     PIC X(02).                  
              88 CS-CURSOR-OK                          VALUE '00'.      
              88 CS-CURSOR-EOC                         VALUE '10'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY NOVCTA.                                                 
                                                                        
      ******************************************************************
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
               SELECT TIPCUEN,                                          
                      NROCUEN,                                          
                      SUCUEN,                                           
                      NROCLI,                                           
                      SALDO,                                            
                      FECSAL                                            
                FROM KC02787.TBCURCTA                                   
               WHERE SUCUEN = 1                                         
            ORDER BY TIPCUEN, SUCUEN, NROCLI                            
           END-EXEC.                                                    
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           IF NOT FS-NOVEDAD-EOF                                        
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
                UNTIL FS-NOVEDAD-EOF                                    
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
                                                                        
           PERFORM 1400-ABRIR-CURSOR                                    
              THRU 1400-F-ABRIR-CURSOR.                                 
                                                                        
           PERFORM 1500-INICIAR-VARHOST                                 
              THRU 1500-F-INICIAR-VARHOST.                              
                                                                        
           PERFORM 1600-LEER-NOVEDAD                                    
              THRU 1600-F-LEER-NOVEDAD.                                 
                                                                        
           PERFORM 2200-FETCH-CURSOR                                    
              THRU 2200-F-FETCH-CURSOR.                                 
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
              WHEN WS-CLAVE-NOVEDAD IS EQUAL TO WS-CLAVE-CURSOR         
                 IF WS-NROCUEN IS EQUAL TO WS-CTA-NROCUEN               
                    PERFORM 2400-BUSCAR-CLIENTE                         
                       THRU 2400-F-BUSCAR-CLIENTE                       
                                                                        
                    IF WS-QUERY-OK IS EQUAL TO 0                        
                       PERFORM 2600-MOSTRAR-DATOS                       
                          THRU 2600-F-MOSTRAR-DATOS                     
                                                                        
                       MOVE 0 TO WS-QUERY-OK                            
                    ELSE                                                
                       MOVE 0 TO WS-QUERY-OK                            
                    END-IF                                              
                                                                        
                    PERFORM 1600-LEER-NOVEDAD                           
                       THRU 1600-F-LEER-NOVEDAD                         
                                                                        
                    PERFORM 2200-FETCH-CURSOR                           
                       THRU 2200-F-FETCH-CURSOR                         
                 END-IF                                                 
                                                                        
                 IF  WS-NROCUEN IS GREATER THAN WS-CTA-NROCUEN          
                    DISPLAY '****************************************'  
                    DISPLAY '* CUENTA SIN NOVEDAD '                     
                    DISPLAY '  - TIPO DE CUENTA: ' CUR-TIPCUEN          
                    DISPLAY '  - NRO. SUCURSAL: ' WS-CTA-SUCUEN         
                    DISPLAY '  - NRO. DE CLIENTE: ' WS-CTA-NROCLI       
                    DISPLAY '****************************************'  
                                                                        
                    PERFORM 2200-FETCH-CURSOR                           
                       THRU 2200-F-FETCH-CURSOR                         
                 END-IF                                                 
                                                                        
                 IF  WS-NROCUEN IS LESS THAN WS-CTA-NROCUEN             
                    DISPLAY '****************************************'  
                    DISPLAY '* NOVEDAD NO ENCONTRADA '                  
                    DISPLAY '  - TIPO DE CUENTA: ' NOV-TIPCUEN          
                    DISPLAY '  - NRO. SUCURSAL: ' WS-SUCUEN             
                    DISPLAY '  - NRO. DE CLIENTE: ' WS-NROCLI           
                    DISPLAY '****************************************'  
                                                                        
                    PERFORM 1600-LEER-NOVEDAD                           
                       THRU 1600-F-LEER-NOVEDAD                         
                 END-IF                                                 
                                                                        
              WHEN WS-CLAVE-NOVEDAD IS GREATER THAN WS-CLAVE-CURSOR     
                 DISPLAY '****************************************'     
                 DISPLAY '* CUENTA SIN NOVEDAD '                        
                 DISPLAY '  - TIPO DE CUENTA: ' CUR-TIPCUEN             
                 DISPLAY '  - NRO. SUCURSAL: ' WS-CTA-SUCUEN            
                 DISPLAY '  - NRO. DE CLIENTE: ' WS-CTA-NROCLI          
                 DISPLAY '****************************************'     
                                                                        
                 PERFORM 2200-FETCH-CURSOR                              
                    THRU 2200-F-FETCH-CURSOR                            
                                                                        
              WHEN WS-CLAVE-NOVEDAD IS LESS THAN WS-CLAVE-CURSOR        
                 DISPLAY '****************************************'     
                 DISPLAY '* NOVEDAD NO ENCONTRADA '                     
                 DISPLAY '  - TIPO DE CUENTA: ' NOV-TIPCUEN             
                 DISPLAY '  - NRO. SUCURSAL: ' WS-SUCUEN                
                 DISPLAY '  - NRO. DE CLIENTE: ' WS-NROCLI              
                 DISPLAY '****************************************'     
                                                                        
                 PERFORM 1600-LEER-NOVEDAD                              
                    THRU 1600-F-LEER-NOVEDAD                            
                                                                        
              WHEN OTHER                                                
                 MOVE HIGH-VALUES          TO WS-CLAVE-CURSOR           
                 MOVE HIGH-VALUES          TO WS-CLAVE-NOVEDAD          
                                                                        
                 MOVE CT-EVALUATE          TO AUX-ERR-ACCION            
                 MOVE CT-EVALUATE          TO AUX-ERR-NOMBRE            
                 MOVE CT-EVALUATE          TO AUX-ERR-STATUS            
                 MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE           
                 MOVE 10                   TO W-N-ERROR                 
                                                                        
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
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

           PERFORM 3400-CERRAR-CURSOR                                   
              THRU 3400-F-CERRAR-CURSOR.                                
                                                                        
           PERFORM 3600-MOSTRAR-TOTALES                                 
              THRU 3600-F-MOSTRAR-TOTALES.                              
                                                                        
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
                                                                        
           OPEN INPUT NOVEDAD.                                          
                                                                        
           IF NOT FS-NOVEDAD-OK                                         
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-NOVEDAD                 TO AUX-ERR-NOMBRE         
              MOVE FS-NOVEDAD                 TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-ARCHIVOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 4 0 0 - A B R I R - C U R S O R                *
      *----------------------------------------------------------------*
                                                                        
       1400-ABRIR-CURSOR.                                               
                                                                        
           MOVE '1400-ABRIR-CURSOR'           TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
              OPEN ITEM_CURSOR                                          
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE HIGH-VALUES          TO WS-CLAVE-CURSOR              
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-OPEN              TO AUX-ERR-ACCION               
              MOVE CT-CURSOR            TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
       1400-F-ABRIR-CURSOR.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *           1 5 0 0 - I N I C I A R - V A R H O S T              *
      *----------------------------------------------------------------*
                                                                        
       1500-INICIAR-VARHOST.                                            
                                                                        
           MOVE '1300-INICIAR-VARHOST'        TO WS-PARRAFO.            
                                                                        
           INITIALIZE WS-CTA-TIPCUEN                                    
                      WS-CTA-NROCUEN                                    
                      WS-CTA-SUCUEN                                     
                      WS-CTA-NROCLI                                     
                      WS-CTA-SALDO                                      
                      WS-CTA-FECSAL                                     
              REPLACING ALPHANUMERIC BY SPACES                          
                             NUMERIC BY ZEROS.                          
                                                                        
       1500-F-INICIAR-VARHOST.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 6 0 0 - L E E R - N O V E D A D                *
      *----------------------------------------------------------------*
                                                                        
       1600-LEER-NOVEDAD.                                               
                                                                        
           MOVE '1600-LEER-NOVEDAD'           TO WS-PARRAFO.            
                                                                        
           READ NOVEDAD INTO WS-REG-CTA.                                
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-NOVEDAD-OK                                       
                    ADD 1                     TO CNT-NOV-LEIDA          
                    MOVE WS-TIPCUEN           TO NOV-TIPCUEN            
                                                                        
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
      *               2 2 0 0 - F E T C H - C U R S O R                *
      *----------------------------------------------------------------*
                                                                        
       2200-FETCH-CURSOR.                                               
                                                                        
           MOVE '2200-FETCH-CURSOR'           TO WS-PARRAFO.            
                                                                        
           PERFORM 1500-INICIAR-VARHOST                                 
              THRU 1500-F-INICIAR-VARHOST.                              
                                                                        
           EXEC SQL                                                     
              FETCH  ITEM_CURSOR                                        
                     INTO                                               
                        :DCLTBCURCTA.WS-CTA-TIPCUEN,                    
                        :DCLTBCURCTA.WS-CTA-NROCUEN,                    
                        :DCLTBCURCTA.WS-CTA-SUCUEN,                     
                        :DCLTBCURCTA.WS-CTA-NROCLI,                     
                        :DCLTBCURCTA.WS-CTA-SALDO,                      
                        :DCLTBCURCTA.WS-CTA-FECSAL                      
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL CT-FOUND                             
               MOVE WS-CTA-TIPCUEN       TO WS-CLAVE-CURSOR             
                                                                        
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               SET CS-CURSOR-EOC         TO TRUE                        
               MOVE HIGH-VALUES          TO WS-CLAVE-CURSOR             
                                                                        
             WHEN OTHER                                                 
               MOVE HIGH-VALUES          TO WS-CLAVE-CURSOR             
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
      *              2 4 0 0 - B U S C A R - C L I E N T E             *
      *----------------------------------------------------------------*
                                                                        
       2400-BUSCAR-CLIENTE.                                             
                                                                        
           MOVE '2400-BUSCAR-CLIENTE'         TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
             SELECT NOMAPE                                              
               INTO :DCLTBCURCLI.WS-CLI-NOMAPE                          
               FROM KC02787.TBCURCLI                                    
              WHERE NROCLI = :WS-CTA-NROCLI                             
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL TO CT-FOUND                          
               ADD 1                          TO CNT-CLI-ENC            
                                                                        
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               MOVE 1                         TO WS-QUERY-OK            
               ADD 1                          TO CNT-CLI-NOENC          
               DISPLAY '-- CLIENTE NO ENCONTRADO --'                    
                                                                        
             WHEN OTHER                                                 
               MOVE SQLCODE              TO CT-SQLCODE-EDIT             
               MOVE CT-QUERY             TO AUX-ERR-ACCION              
               MOVE CT-QUERY             TO AUX-ERR-NOMBRE              
               MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS              
               MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE             
               MOVE 10                   TO W-N-ERROR                   
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
           END-EVALUATE.                                                
                                                                        
       2400-F-BUSCAR-CLIENTE.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 6 0 0 - M O S T R A R - D A T O S               *
      *----------------------------------------------------------------*
                                                                        
       2600-MOSTRAR-DATOS.                                              
                                                                        
           MOVE '2600-MOSTRAR-DATOS'          TO WS-PARRAFO.            
                                                                        
           COMPUTE WS-SALDO-ACT = WS-SALDO + WS-CTA-SALDO.              
                                                                        
           DISPLAY '****************************************'           
           DISPLAY '  - TIPO DE CUENTA: ' WS-CTA-TIPCUEN.               
           MOVE WS-CTA-NROCUEN TO WS-MASCARA.                           
           DISPLAY '  - NRO. DE CUENTA: ' WS-MASCARA.                   
           MOVE WS-CTA-NROCLI  TO WS-MASCARA.                           
           DISPLAY '  - NRO. DE CLIENTE: ' WS-MASCARA.                  
           DISPLAY '  - NOMBRE: ' WS-CLI-NOMAPE.                        
           MOVE WS-CTA-SALDO   TO WS-SALDO-EDIT.                        
           DISPLAY '     * SALDO ANTERIOR: ' WS-SALDO-EDIT.             
           MOVE WS-SALDO       TO WS-SALDO-EDIT.                        
           DISPLAY '     + MOVIMIENTO: ' WS-SALDO-EDIT.                 
           MOVE WS-SALDO-ACT   TO WS-SALDO-EDIT.                        
           DISPLAY '  - SALDO ACTUALIZADO: ' WS-SALDO-EDIT.             
           DISPLAY '****************************************'           
                                                                        
           MOVE 0 TO WS-SALDO-ACT.                                      
           ADD 1  TO CNT-SALDO-ACT.                                     
                                                                        
       2600-F-MOSTRAR-DATOS.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE NOVEDAD.                                               
                                                                        
           IF NOT FS-NOVEDAD-OK                                         
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-NOVEDAD                 TO AUX-ERR-NOMBRE         
              MOVE FS-NOVEDAD                 TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       3200-F-CERRAR-ARCHIVOS.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 4 0 0 - C E R R A R - C U R S O R               *
      *----------------------------------------------------------------*
                                                                        
       3400-CERRAR-CURSOR.                                              
                                                                        
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
                                                                        
       3400-F-CERRAR-CURSOR.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            3 6 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3600-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3600-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-NOV-LEIDA                 TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMSIN31               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* NOVEDADES LEIDAS:                       '         
                                                      WS-MASCARA '   *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-CLI-ENC                   TO WS-MASCARA.            
           DISPLAY '* CLIENTES ENCONTRADOS:                   '         
                                                      WS-MASCARA '   *'.
                                                                        
           MOVE CNT-CLI-NOENC                 TO WS-MASCARA.            
           DISPLAY '* CLIENTES NO ENCONTRADOS:                '         
                                                      WS-MASCARA '   *'.
                                                                        
           MOVE CNT-SALDO-ACT                 TO WS-MASCARA.            
           DISPLAY '* SALDOS ACTUALIZADOS:                    '         
                                                      WS-MASCARA '   *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.
                                                                        
       3600-F-MOSTRAR-TOTALES.                                          
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