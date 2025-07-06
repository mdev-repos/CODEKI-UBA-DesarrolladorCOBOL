      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMSIN28.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-05                                 
                                                                        
      *----------------------------------------------------------------*
      *     ACTIVIDAD CLASE SINCRONICA 28 | PGM CON DB2 CON CURSOR     *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PGM UTILIZA UN CURSOR PARA EJECUTAR UNA QUERY Y CONSTRUIR *
      * UN LISTADO COMO ARCHIVO DE SALIDA ( CLIENTES ).                *
      * AL FINAL DEL PGM HACE DISPLAY DE                               *
      *    - CANTIDAD DE REGISTROS LEIDOS                              *
      *    - CANTIDAD DE REGISTROS IMPRESOS                            *
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
                                                                        
      * PGM CON ARCHIVO DE SALIDA  FBA ( CLIENTES )                     
                                                                        
           SELECT CLIENTES  ASSIGN TO CLIENTES                          
                            FILE STATUS IS FS-CLIENTES.                 
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      * CLIENTES ( ARCHIVO FBA )                                        
                                                                        
       FD CLIENTES                                                      
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 LINEA-IMPRESION                   PIC X(132).                 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMSIN28'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-FETCH                      PIC X(08)  VALUE 'FETCH   '.
           02 CT-CLIENTES                   PIC X(08)  VALUE 'CLIENTES'.
           02 CT-CURSOR                     PIC X(08)  VALUE 'CURSOR  '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE  +100. 
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZ9.                    
                                                                        
       01  WS-FECHA-SISTEMA.                                            
           02 WS-ANIO                       PIC 9(04) VALUE ZEROS.      
           02 WS-MES                        PIC 9(02) VALUE ZEROS.      
           02 WS-DIA                        PIC 9(02) VALUE ZEROS.      
                                                                        
       01  WS-CONTROL-LINEAS.                                           
           02 WS-MAX-LINEAS                 PIC 9(02) VALUE 10.         
           02 WS-LINEA-ACTUAL               PIC 9(02) VALUE ZEROS.      
                                                                        
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
           02 CNT-CLIENTES-IMPRESOS         PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
      * PGM SIN APAREO                                                  
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-CLIENTES                   PIC X(02).                  
              88 FS-CLIENTES-OK                        VALUE '00'.      
              88 FS-CLIENTES-EOF                       VALUE '10'.      
                                                                        
      * UTILIZO FS-CLIENTES-EOF PARA CORTAR EL BUCLE. ARCHIVO DE SALIDA 
      * NO POSEE EOF...                                                 
                                                                        
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
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
      *    A R E A  D E  F O R M A T O  D E  A R CH I V O  F B A       *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *                F O R M A T O  D E  H E A D E R                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-HEADER-INICIAL.                                           
           02  FILLER          PIC X(10) VALUE SPACES.                  
           02  FILLER          PIC X(20) VALUE "LISTADO DE CLIENTES ".  
           02  FILLER          PIC X(22) VALUE "CON SALDO MAYOR A CERO".
           02  FILLER          PIC X(10) VALUE " - FECHA: ".            
           02  WS-FECHA-TITULO PIC X(10).                               
           02  FILLER          PIC X(60) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  C A B E C E R A               *
      *----------------------------------------------------------------*
                                                                        
       01  WS-CABECERA-COLUMNAS.                                        
           02  FILLER          PIC X(17) VALUE "| TIPO DE CUENTA ".     
           02  FILLER          PIC X(17) VALUE "| NRO. DE CUENTA ".     
           02  FILLER          PIC X(15) VALUE "| NRO. DE SUC. ".       
           02  FILLER          PIC X(18) VALUE "| NRO. DE CLIENTE ".    
           02  FILLER          PIC X(17) VALUE "|       NOMBRE Y ".     
           02  FILLER          PIC X(16) VALUE "APELLIDO        ".      
           02  FILLER          PIC X(14) VALUE "|    SALDO    ".        
           02  FILLER          PIC X(17) VALUE "| FECHA DE ACT. |".     
           02  FILLER          PIC X(01) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *           F O R M A T O  D E  S U B C A B E C E R A            *
      *----------------------------------------------------------------*
                                                                        
       01  WS-SUBCABECERA.                                              
           02  FILLER          PIC X(17) VALUE "|                ".     
           02  FILLER          PIC X(17) VALUE "|                ".     
           02  FILLER          PIC X(15) VALUE "|              ".       
           02  FILLER          PIC X(18) VALUE "|                 ".    
           02  FILLER          PIC X(17) VALUE "|                ".     
           02  FILLER          PIC X(16) VALUE "                ".      
           02  FILLER          PIC X(14) VALUE "|           ".          
           02  FILLER          PIC X(17) VALUE "|               |".     
           02  FILLER          PIC X(01) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  D E T A L L E                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-DETALLE.                                                  
           02  FILLER          PIC X(08) VALUE "|       ".              
           02  DET-TIP-CUEN    PIC X(02) VALUE SPACES.                  
           02  FILLER          PIC X(07) VALUE "       ".               
           02  FILLER          PIC X(06) VALUE "|     ".                
           02  DET-NRO-CUEN    PIC ZZZZ9.                               
           02  FILLER          PIC X(06) VALUE "      ".                
           02  FILLER          PIC X(07) VALUE "|     ".                
           02  DET-NRO-SUC     PIC Z9.                                  
           02  FILLER          PIC X(06) VALUE "      ".                
           02  FILLER          PIC X(08) VALUE "|       ".              
           02  DET-NRO-CLI     PIC ZZ9.                                 
           02  FILLER          PIC X(07) VALUE "       ".               
           02  FILLER          PIC X(02) VALUE "| ".                    
           02  DET-NOM-APE     PIC X(30) VALUE ZEROS.                   
           02  FILLER          PIC X(01) VALUE " ".                     
           02  FILLER          PIC X(02) VALUE "| ".                    
           02  DET-SALDO       PIC $ZZ.ZZ9,99-.                         
           02  FILLER          PIC X(01) VALUE " ".                     
           02  FILLER          PIC X(04) VALUE "|   ".                  
           02  DET-FECHA       PIC X(10) VALUE SPACES.                  
           02  FILLER          PIC X(04) VALUE "  | ".                  
                                                                        
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
                                                                        
           PERFORM 2600-GRABAR-CIERRE                                   
              THRU 2600-F-GRABAR-CIERRE.                                
                                                                        
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
                                                                        
           PERFORM 2200-FETCH-CURSOR                                    
              THRU 2200-F-FETCH-CURSOR.                                 
                                                                        
           PERFORM 1500-OBTENER-FECHA                                   
              THRU 1500-F-OBTENER-FECHA.                                
                                                                        
           PERFORM 1600-GRABAR-TITULOS                                  
              THRU 1600-F-GRABAR-TITULOS.                               
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           PERFORM 2400-GRABAR-REGISTRO                                 
              THRU 2400-F-GRABAR-REGISTRO.                              
                                                                        
           PERFORM 2200-FETCH-CURSOR                                    
              THRU 2200-F-FETCH-CURSOR.                                 
                                                                        
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                       3 0 0 0 - F I N                          *
      *----------------------------------------------------------------*
                                                                        
       3000-FIN.                                                        
                                                                        
           MOVE '3000-FIN'                    TO WS-PARRAFO.            
                                                                        
           PERFORM 3200-CERRAR-ARCHIVOS                                 
              THRU 3200-F-CERRAR-ARCHIVOS.                              
                                                                        
           PERFORM 3300-CERRAR-CURSOR                                   
              THRU 3300-F-CERRAR-CURSOR.                                
                                                                        
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
                                                                        
           OPEN OUTPUT  CLIENTES.                                       
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
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
              SET  FS-CLIENTES-EOF      TO TRUE                         
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
      *               1 5 0 0 - O B T E N E R - F E C H A              *
      *----------------------------------------------------------------*
                                                                        
       1500-OBTENER-FECHA.                                              
                                                                        
           MOVE '1500-OBTENER-FECHA'          TO WS-PARRAFO.            
                                                                        
           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.                  
                                                                        
           STRING WS-DIA   DELIMITED BY SIZE                            
                  '-'      DELIMITED BY SIZE                            
                  WS-MES   DELIMITED BY SIZE                            
                  '-'      DELIMITED BY SIZE                            
                  WS-ANIO  DELIMITED BY SIZE                            
              INTO WS-FECHA-TITULO                                      
           END-STRING.                                                  
                                                                        
       1500-F-OBTENER-FECHA.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 6 0 0 - G R A B A R - T I T U L O S            *
      *----------------------------------------------------------------*
                                                                        
       1600-GRABAR-TITULOS.                                             
                                                                        
           MOVE '1600-GRABAR-TITULOS'         TO WS-PARRAFO.            
                                                                        
           MOVE 0 TO WS-LINEA-ACTUAL.                                   
                                                                        
           WRITE LINEA-IMPRESION FROM WS-HEADER-INICIAL                 
              AFTER ADVANCING PAGE.                                     
                                                                        
           WRITE LINEA-IMPRESION FROM WS-CABECERA-COLUMNAS              
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           WRITE LINEA-IMPRESION FROM WS-SUBCABECERA                    
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           PERFORM 2800-EVALUAR-GRABACION                               
              THRU 2800-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 3 TO WS-LINEA-ACTUAL.                                    
                                                                        
       1600-F-GRABAR-TITULOS.                                           
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
               DISPLAY 'ACCESO DB2 OK '                                 
               ADD 1 TO CNT-CLIENTES-LEIDOS                             
                                                                        
             WHEN SQLCODE EQUAL +100                                    
               SET  FS-CLIENTES-EOF      TO TRUE                        
                                                                        
             WHEN OTHER                                                 
               SET  FS-CLIENTES-EOF      TO TRUE                        
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
      *              2 4 0 0 - G R A B A R - R E G I S T R O           *
      *----------------------------------------------------------------*
                                                                        
       2400-GRABAR-REGISTRO.                                            
                                                                        
           MOVE '2400-GRABAR-REGISTRO'        TO WS-PARRAFO.            
                                                                        
           IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                          
              PERFORM 1600-GRABAR-TITULOS                               
                 THRU 1600-F-GRABAR-TITULOS                             
           END-IF.                                                      
                                                                        
           MOVE WS-TIPCUEN    TO  DET-TIP-CUEN.                         
           MOVE WS-NROCUEN    TO  DET-NRO-CUEN.                         
           MOVE WS-SUCUEN     TO  DET-NRO-SUC.                          
           MOVE WS-NROCLI     TO  DET-NRO-CLI.                          
           MOVE WS-CLI-NOMAPE TO  DET-NOM-APE.                          
           MOVE WS-SALDO      TO  DET-SALDO.                            
           MOVE WS-FECSAL     TO  DET-FECHA.                            
                                                                        
           WRITE LINEA-IMPRESION FROM WS-DETALLE                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           PERFORM 2800-EVALUAR-GRABACION                               
              THRU 2800-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 1 TO WS-LINEA-ACTUAL.                                    
           ADD 1 TO CNT-CLIENTES-IMPRESOS.                              
                                                                        
       2400-F-GRABAR-REGISTRO.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 6 0 0 - G R A B A R - C I E R R E               *
      *----------------------------------------------------------------*
                                                                        
       2600-GRABAR-CIERRE.                                              
                                                                        
           MOVE '2600-GRABAR-CIERRE'          TO WS-PARRAFO.            
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE 'FINAL LISTADO CLIENTES' TO LINEA-IMPRESION.            
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
       2600-F-GRABAR-CIERRE.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *           2 8 0 0 - E V A L U A R - G R A B A C I O N          *
      *----------------------------------------------------------------*
                                                                        
       2800-EVALUAR-GRABACION.                                          
                                                                        
           MOVE '2800-EVALUAR-GRABACION'      TO WS-PARRAFO.            
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-WRITE                   TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       2800-F-EVALUAR-GRABACION.                                        
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE CLIENTES.                                              
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       3200-F-CERRAR-ARCHIVOS.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               3 3 0 0 - C E R R A R - C U R S O R              *
      *----------------------------------------------------------------*
                                                                        
       3300-CERRAR-CURSOR.                                              
                                                                        
           MOVE '3300-CERRAR-CURSOR'          TO WS-PARRAFO.            
                                                                        
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
                                                                        
       3300-F-CERRAR-CURSOR.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            3 4 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3400-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3400-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-CLIENTES-LEIDOS           TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMSIN28               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REG. DE CLIENTES LEIDOS:                '         
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                 '.
           MOVE CNT-CLIENTES-IMPRESOS TO WS-MASCARA.                    
           DISPLAY '* REG. DE CLIENTES IMPRESOS:              '         
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                 '.
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