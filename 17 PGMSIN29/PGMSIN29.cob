      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMSIN29.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-07                                 
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD CLASE SINCRONICA 29 | PGM CON DB2 + ACTUALIZACION  *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PGM UTILIZA LEE UN ARCHIVO QSAM DE NOVEDAD Y REALIZA LA   *
      * CONSULTA SQL NECESARIA PARA DAR DE ALTA O MODIFICAR LOS REGIS- *
      * -TROS EN LA TABLA TBCURCLI.                                    *
      * DE ENCONTRAR UN ERROR DE VALIDACION SE MOSTRARAN POR PANTALLA  *
      * MEDIANTE DISPLAY.                                              *
      * AL FINALIZAR EL PGM SE INFORMARA:                              *
      *   - TOTAL NOVEDAD LEÍDAS                                       *
      *   - TOTAL NOVEDAD CON ERROR                                    *
      *   - TOTAL ALTA NOVEDAD                                         *
      *   - TOTAL MODIFICACIÓN NOVEDAD                                 *
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
                                                                        
      * PGM CON ARCHIVO DE ENTRADA (QSAM) NOVEDAD                       
                                                                        
           SELECT NOVEDAD ASSIGN TO NOVEDAD                             
                            FILE STATUS IS FS-NOVEDAD.                  
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------* 
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      * NOVEDAD ( ARCHIVO QSAM )                                        
                                                                        
       FD NOVEDAD                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-NOVEDAD                       PIC X(80).                  
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMSIN29'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-NOVEDAD                    PIC X(08)  VALUE 'NOVEDAD '.
           02 CT-QUERYDB2                   PIC X(08)  VALUE 'QUERY   '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE  +100. 
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZ9.                    
           02 WS-NUM-NOV                    PIC 999.                    
           02 WS-AUX-NRODOC                 PIC S9(11)V USAGE COMP-3.   
           02 WS-AUX-NROCLI                 PIC S9(03)V USAGE COMP-3.   
           02 WS-MAX-DIA                    PIC 99.                     
           02 WS-FECHA.                                                 
              03 WS-FECHA-ANIO              PIC 9(04).                  
              03 WS-FECHA-MES               PIC 9(02).                  
              03 WS-FECHA-DIA               PIC 9(02).                  
           02 WS-NOMBRE-EMPIEZA             PIC X(06).                  
           02 WS-FLAG-ERRNOV                PIC 9.                      
           02 WS-ERRNOV-TIPNOV              PIC 9.                      
           02 WS-ERRNOV-TIPDOC              PIC 9.                      
           02 WS-ERRNOV-NRODOC              PIC 9.                      
           02 WS-ERRNOV-NROCLI              PIC 9.                      
           02 WS-ERRNOV-CLINOM              PIC 9.                      
           02 WS-ERRNOV-CLIFEC              PIC 9.                      
           02 WS-ERRNOV-CLISEX              PIC 9.                      
           02 WS-FLAG-ERRCLI                PIC 9.                      
           02 WS-ERRCLI-NOEXISTE            PIC 9.                      
           02 WS-ERRCLI-YAEXISTE            PIC 9.                      
                                                                        
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
           02 CNT-NOVEDAD-LEIDA             PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-ERROR             PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-ALTA              PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-MOD               PIC 9(03)  VALUE ZEROS.     
           02 CNT-SQL-ERROR                 PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *     C L A V E  D E  A P A R E O  |  C O N  Q U E R Y  S Q L    *
      *----------------------------------------------------------------*
                                                                        
      * PGM SIN APAREO                                                  
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-NOVEDAD                    PIC X(02).                  
              88 FS-NOVEDAD-OK                         VALUE '00'.      
              88 FS-NOVEDAD-EOF                        VALUE '10'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY NOVECLIE.                                               
                                                                        
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
             INCLUDE TBCURCLI                                           
           END-EXEC.                                                    
                                                                        
           EXEC SQL                                                     
             INCLUDE TBCURCTA                                           
           END-EXEC.                                                    
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       

           IF FS-NOVEDAD-OK                                             
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
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           PERFORM 1400-LEER-NOVEDAD                                    
              THRU 1400-F-LEER-NOVEDAD.                                 
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           PERFORM 2800-VALIDAR-CLIENTE                                 
              THRU 2800-F-VALIDAR-CLIENTE.                              
                                                                        
           PERFORM 2900-VALIDAR-NOVEDAD                                 
              THRU 2900-F-VALIDAR-NOVEDAD.                              
                                                                        
           IF WS-FLAG-ERRNOV IS NOT EQUAL TO 1 AND                      
              WS-FLAG-ERRCLI IS NOT EQUAL TO 1                          
             EVALUATE TRUE                                              
      *        ALTA DE CLIENTE EN LA TABLA                              
               WHEN NOV-TIP-NOV = 'AL'                                  
                 PERFORM 2200-QUERY-ALTA                                
                    THRU 2200-F-QUERY-ALTA                              
                                                                        
      *        MODIFICACION DE NUMERO DE CLIENTE                        
               WHEN NOV-TIP-NOV = 'CL'                                  
                 PERFORM 2300-QUERY-NUMERO                              
                    THRU 2300-F-QUERY-NUMERO                            
                                                                        
      *        MODIFICACION DE NOMBRE DE CLIENTE                        
               WHEN NOV-TIP-NOV = 'CN'                                  
                 PERFORM 2400-QUERY-NOMBRE                              
                    THRU 2400-F-QUERY-NOMBRE                            
                                                                        
      *        MODIFICACION DE SEXO DE CLIENTE                          
               WHEN NOV-TIP-NOV = 'CX'                                  
                 PERFORM 2500-QUERY-SEXO                                
                    THRU 2500-F-QUERY-SEXO                              
                                                                        
      *        MANEJO DE ERRORES                                        
               WHEN OTHER                                               
                 MOVE CT-NOVEDAD           TO AUX-ERR-ACCION            
                 MOVE CT-NOVEDAD           TO AUX-ERR-NOMBRE            
                 MOVE FS-NOVEDAD           TO AUX-ERR-STATUS            
                 MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE           
                 MOVE 10                   TO W-N-ERROR                 
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
             END-EVALUATE                                               
           ELSE                                                         
             PERFORM 2600-MOSTRAR-ERROR                                 
                THRU 2600-F-MOSTRAR-ERROR                               
           END-IF.                                                      
                                                                        
           PERFORM 1400-LEER-NOVEDAD                                    
              THRU 1400-F-LEER-NOVEDAD.                                 
                                                                        
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
      *           1 3 0 0 - I N I C I A R - V A R H O S T              *
      *----------------------------------------------------------------*
                                                                        
       1300-INICIAR-VARHOST.                                            
                                                                        
           MOVE '1300-INICIAR-VARHOST'        TO WS-PARRAFO.            
                                                                        
           INITIALIZE WS-CLI-TIPDOC                                     
                      WS-CLI-NRODOC                                     
                      WS-CLI-NROCLI                                     
                      WS-CLI-NOMAPE                                     
                      WS-CLI-FECNAC                                     
                      WS-CLI-SEXO                                       
              REPLACING ALPHANUMERIC BY SPACES                          
                             NUMERIC BY ZEROS.                          
                                                                        
       1300-F-INICIAR-VARHOST.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 4 0 0 - L E E R - N O V E D A D                *
      *----------------------------------------------------------------*
                                                                        
       1400-LEER-NOVEDAD.                                               
                                                                        
           MOVE '1400-LEER-NOVEDAD'           TO WS-PARRAFO.            
                                                                        
           READ NOVEDAD INTO WS-REG-NOVECLI.                            
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-NOVEDAD-OK                                       
                    ADD 1                     TO CNT-NOVEDAD-LEIDA      
                    ADD 1                     TO WS-NUM-NOV             
                                                                        
               WHEN FS-NOVEDAD-EOF                                      
                    SET FS-NOVEDAD-EOF        TO TRUE                   
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-NOVEDAD           TO AUX-ERR-NOMBRE         
                    MOVE FS-NOVEDAD           TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR               
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
           END-EVALUATE.                                                
                                                                        
       1400-F-LEER-NOVEDAD.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                2 2 0 0 - Q U E R Y - A L T A                   *
      *----------------------------------------------------------------*
                                                                        
       2200-QUERY-ALTA.                                                 
                                                                        
           MOVE '2200-QUERY-ALTA'             TO WS-PARRAFO.            
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           MOVE NOV-TIP-DOC                   TO WS-CLI-TIPDOC.         
                                                                        
           MOVE NOV-NRO-DOC                   TO WS-AUX-NRODOC.         
           MOVE WS-AUX-NRODOC                 TO WS-CLI-NRODOC.         
                                                                        
           MOVE NOV-CLI-NRO                   TO WS-AUX-NROCLI.         
           MOVE WS-AUX-NROCLI                 TO WS-CLI-NROCLI.         
                                                                        
           MOVE NOV-CLI-NOMBRE                TO WS-CLI-NOMAPE.         
                                                                        
           MOVE NOV-CLI-FENAC                 TO WS-FECHA.              
                                                                        
           STRING WS-FECHA-ANIO  DELIMITED BY SIZE                      
                  '-'      DELIMITED BY SIZE                            
                  WS-FECHA-MES   DELIMITED BY SIZE                      
                  '-'      DELIMITED BY SIZE                            
                  WS-FECHA-DIA   DELIMITED BY SIZE                      
              INTO WS-CLI-FECNAC                                        
           END-STRING.                                                  
                                                                        
           MOVE NOV-CLI-SEXO                  TO WS-CLI-SEXO.           
                                                                        
           EXEC SQL INSERT                                              
             INTO KC02787.TBCURCLI                                      
               (TIPDOC, NRODOC, NROCLI, NOMAPE, FECNAC, SEXO)           
             VALUES (:WS-CLI-TIPDOC,                                    
                     :WS-CLI-NRODOC,                                    
                     :WS-CLI-NROCLI,                                    
                     :WS-CLI-NOMAPE,                                    
                     :WS-CLI-FECNAC,                                    
                     :WS-CLI-SEXO)                                      
           END-EXEC.                                                    

           DISPLAY '--------------------------'                         
                   ' DB2 | SQL --------------------------'.             
                                                                        
           MOVE SQLCODE TO CT-SQLCODE-EDIT.                             
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               DISPLAY ' * ERROR EN ALTA --> ' CT-SQLCODE-EDIT          
               ADD 1 TO CNT-SQL-ERROR                                   
                                                                        
             WHEN SQLCODE IS EQUAL TO 0                                 
               DISPLAY ' * ALTA EXITOSA '                               
               DISPLAY '   - CLIENTE: '                                 
               DISPLAY '   TIPO DOC: ' WS-CLI-TIPDOC                    
               DISPLAY '   NRO DOC: ' WS-CLI-NRODOC                     
               ADD 1 TO CNT-NOVEDAD-ALTA                                
                                                                        
             WHEN OTHER                                                 
              DISPLAY '  * ERROR DB2 --> '  CT-SQLCODE-EDIT             
              ADD 1 TO CNT-SQL-ERROR                                    
           END-EVALUATE.                                                
                                                                        
           DISPLAY '-------------------------------------'              
                   '--------------------------'.                        
                                                                        
       2200-F-QUERY-ALTA.                                               
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 3 0 0 - Q U E R Y - N U M E R O                *
      *----------------------------------------------------------------*
                                                                        
       2300-QUERY-NUMERO.                                               
                                                                        
           MOVE '2300-QUERY-NUMERO'           TO WS-PARRAFO.            
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           MOVE NOV-TIP-DOC                   TO WS-CLI-TIPDOC.         
                                                                        
           MOVE NOV-NRO-DOC                   TO WS-AUX-NRODOC.         
           MOVE WS-AUX-NRODOC                 TO WS-CLI-NRODOC.         
                                                                        
           MOVE NOV-CLI-NRO                   TO WS-AUX-NROCLI.         
           MOVE WS-AUX-NROCLI                 TO WS-CLI-NROCLI.         
                                                                        
           EXEC SQL                                                     
             UPDATE KC02787.TBCURCLI                                    
             SET NROCLI = :WS-CLI-NROCLI                                
             WHERE TIPDOC = :WS-CLI-TIPDOC                              
               AND NRODOC = :WS-CLI-NRODOC                              
           END-EXEC.                                                    
                                                                        
           DISPLAY '--------------------------'                         
                   ' DB2 | SQL --------------------------'.             
                                                                        
           MOVE SQLCODE TO CT-SQLCODE-EDIT.                             
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               DISPLAY ' * ERROR EN MODIFICACION --> ' CT-SQLCODE-EDIT  
               ADD 1 TO CNT-SQL-ERROR                                   
                                                                        
             WHEN SQLCODE IS EQUAL TO 0                                 
               DISPLAY ' * NRO CLIENTE MODIFICADO CORRECTAMENTE: '      
               DISPLAY '   - CLIENTE: '                                 
               DISPLAY '   TIPO DOC: ' WS-CLI-TIPDOC                    
               DISPLAY '   NRO DOC: ' WS-CLI-NRODOC                     
               ADD 1 TO CNT-NOVEDAD-MOD                                 
                                                                        
             WHEN OTHER                                                 
              DISPLAY '  * ERROR DB2 --> '  CT-SQLCODE-EDIT             
              ADD 1 TO CNT-SQL-ERROR                                    
           END-EVALUATE.                                                
                                                                        
           DISPLAY '-------------------------------------'              
                   '--------------------------'.                        
                                                                        
       2300-F-QUERY-NUMERO.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 4 0 0 - Q U E R Y - N O M B R E                *
      *----------------------------------------------------------------*
                                                                        
       2400-QUERY-NOMBRE.                                               
                                                                        
           MOVE '2400-QUERY-NOMBRE'           TO WS-PARRAFO.            
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           MOVE NOV-TIP-DOC                   TO WS-CLI-TIPDOC.         
           MOVE NOV-NRO-DOC                   TO WS-AUX-NRODOC.         
           MOVE WS-AUX-NRODOC                 TO WS-CLI-NRODOC.         
           MOVE NOV-CLI-NOMBRE                TO WS-CLI-NOMAPE.         
                                                                        
           EXEC SQL                                                     
             UPDATE KC02787.TBCURCLI                                    
             SET NOMAPE = :WS-CLI-NOMAPE                                
             WHERE TIPDOC = :WS-CLI-TIPDOC                              
               AND NRODOC = :WS-CLI-NRODOC                              
           END-EXEC.                                                    
                                                                        
           DISPLAY '--------------------------'                         
                   ' DB2 | SQL --------------------------'.             
                                                                        
           MOVE SQLCODE TO CT-SQLCODE-EDIT.                             
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               DISPLAY ' * ERROR EN MODIFICACION --> ' CT-SQLCODE-EDIT  
               ADD 1 TO CNT-SQL-ERROR                                   
                                                                        
             WHEN SQLCODE IS EQUAL TO 0                                 
               DISPLAY ' * NOMBRE MODIFICADO CORRECTAMENTE: '           
               DISPLAY '   - CLIENTE: '                                 
               DISPLAY '   TIPO DOC: ' WS-CLI-TIPDOC                    
               DISPLAY '   NRO DOC: ' WS-CLI-NRODOC                     
               ADD 1 TO CNT-NOVEDAD-MOD                                 
                                                                        
             WHEN OTHER                                                 
              DISPLAY '  * ERROR DB2 --> '  CT-SQLCODE-EDIT             
              ADD 1 TO CNT-SQL-ERROR                                    
           END-EVALUATE.                                                
                                                                        
           DISPLAY '-------------------------------------'              
                   '--------------------------'.                        

       2400-F-QUERY-NOMBRE.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                2 5 0 0 - Q U E R Y - S E X O                   *
      *----------------------------------------------------------------*
                                                                        
       2500-QUERY-SEXO.                                                 
                                                                        
           MOVE '2500-QUERY-SEXO'             TO WS-PARRAFO.            
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           MOVE NOV-TIP-DOC                   TO WS-CLI-TIPDOC.         
           MOVE NOV-NRO-DOC                   TO WS-AUX-NRODOC.         
           MOVE WS-AUX-NRODOC                 TO WS-CLI-NRODOC.         
           MOVE NOV-CLI-SEXO                  TO WS-CLI-SEXO.           
                                                                        
           EXEC SQL                                                     
             UPDATE KC02787.TBCURCLI                                    
             SET SEXO = :WS-CLI-SEXO                                    
             WHERE TIPDOC = :WS-CLI-TIPDOC                              
               AND NRODOC = :WS-CLI-NRODOC                              
           END-EXEC.                                                    

           DISPLAY '--------------------------'                         
                   ' DB2 | SQL --------------------------'.             
                                                                        
           MOVE SQLCODE TO CT-SQLCODE-EDIT.                             
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               DISPLAY ' * ERROR EN MODIFICACION --> ' CT-SQLCODE-EDIT  
               ADD 1 TO CNT-SQL-ERROR                                   
                                                                        
             WHEN SQLCODE IS EQUAL TO 0                                 
               DISPLAY ' * SEXO MODIFICADO CORRECTAMENTE: '             
               DISPLAY '   - CLIENTE: '                                 
               DISPLAY '   TIPO DOC: ' WS-CLI-TIPDOC                    
               DISPLAY '   NRO DOC: ' WS-CLI-NRODOC                     
               ADD 1 TO CNT-NOVEDAD-MOD                                 
                                                                        
             WHEN OTHER                                                 
              DISPLAY '  * ERROR DB2 --> '  CT-SQLCODE-EDIT             
              ADD 1 TO CNT-SQL-ERROR                                    
           END-EVALUATE.                                                
                                                                        
           DISPLAY '-------------------------------------'              
                   '--------------------------'.                        
                                                                        
       2500-F-QUERY-SEXO.                                               
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 6 0 0 - M O S T R A R - E R R O R              *
      *----------------------------------------------------------------*
                                                                        
       2600-MOSTRAR-ERROR.                                              
                                                                        
           MOVE '2600-MOSTRAR-ERROR'          TO WS-PARRAFO.            
                                                                        
           MOVE WS-NUM-NOV                    TO WS-MASCARA.            
                                                                        
           DISPLAY '-------------------'                                
                   ' ERROR  | NOVEDAD NRO ' WS-MASCARA                  
                   ' ------------------'.                               
                                                                        
           IF WS-ERRNOV-TIPNOV IS EQUAL TO 1                            
             DISPLAY '  - TIPO DE NOVEDAD INVALIDA'                     
             MOVE 0 TO WS-ERRNOV-TIPNOV                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRNOV-TIPDOC IS EQUAL TO 1                            
             DISPLAY '  - TIPO DE DOCUMENTO INVALIDO'                   
             MOVE 0 TO WS-ERRNOV-TIPDOC                                 
           END-IF.                                                      

           IF WS-ERRNOV-NRODOC IS EQUAL TO 1                            
             DISPLAY '  - NUMERO DE DOCUMENTO INVALIDO'                 
             MOVE 0 TO WS-ERRNOV-NRODOC                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRNOV-NROCLI IS EQUAL TO 1                            
             DISPLAY '  - NUMERO DE CLIENTE INVALIDO'                   
             MOVE 0 TO WS-ERRNOV-NROCLI                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRNOV-CLINOM IS EQUAL TO 1                            
             DISPLAY '  - NOMBRE DE CLIENTE INVALIDO'                   
             MOVE 0 TO WS-ERRNOV-CLINOM                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRNOV-CLIFEC IS EQUAL TO 1                            
             DISPLAY '  - FECHA DE NACIMIENTO DE CLIENTE INVALIDA'      
             MOVE 0 TO WS-ERRNOV-CLIFEC                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRNOV-CLISEX IS EQUAL TO 1                            
             DISPLAY '  - SEXO DE CLIENTE INVALIDO'                     
             MOVE 0 TO WS-ERRNOV-CLISEX                                 
           END-IF.                                                      
                                                                        
           IF WS-ERRCLI-NOEXISTE IS EQUAL TO 1                          
             DISPLAY '  - EL CLIENTE A MODIFICAR NO EXISTE'             
             MOVE 0 TO WS-ERRCLI-NOEXISTE                               
           END-IF.                                                      
                                                                        
           IF WS-ERRCLI-YAEXISTE IS EQUAL TO 1                          
             DISPLAY '  - YA EXISTE EL CLIENTE QUE SE QUIERE AGREGAR'   
             MOVE 0 TO WS-ERRCLI-YAEXISTE                               
           END-IF.                                                      
                                                                        
           DISPLAY '-------------------------------------'              
                   '--------------------------'.                        
                                                                        
           MOVE 0 TO WS-FLAG-ERRNOV.                                    
           MOVE 0 TO WS-FLAG-ERRCLI.                                    
                                                                        
           ADD 1 TO CNT-NOVEDAD-ERROR.                                  
                                                                        
       2600-F-MOSTRAR-ERROR.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 8 0 0 - V A L I D A R - C L I E N T E            *
      *----------------------------------------------------------------*
                                                                        
       2800-VALIDAR-CLIENTE.                                            

           MOVE '2800-VALIDAR-CLIENTE'        TO WS-PARRAFO.            
                                                                        
           PERFORM 1300-INICIAR-VARHOST                                 
              THRU 1300-F-INICIAR-VARHOST.                              
                                                                        
           MOVE NOV-TIP-DOC                   TO WS-CLI-TIPDOC.         
           MOVE NOV-NRO-DOC                   TO WS-AUX-NRODOC.         
           MOVE WS-AUX-NRODOC                 TO WS-CLI-NRODOC.         
                                                                        
           EXEC SQL                                                     
             SELECT NROCLI                                              
               INTO :WS-AUX-NROCLI                                      
               FROM KC02787.TBCURCLI                                    
              WHERE TIPDOC = :WS-CLI-TIPDOC AND NRODOC = :WS-CLI-NRODOC 
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN NOV-TIP-NOV = 'AL'                                    
             AND SQLCODE IS NOT EQUAL TO CT-NOT-FOUND                   
               MOVE 1 TO WS-FLAG-ERRCLI                                 
               MOVE 1 TO WS-ERRCLI-YAEXISTE                             
                                                                        
             WHEN (NOV-TIP-NOV = 'CL' OR                                
                   NOV-TIP-NOV = 'CN' OR                                
                   NOV-TIP-NOV = 'CX')                                  
             AND SQLCODE IS EQUAL TO CT-NOT-FOUND                       
               MOVE 1 TO WS-FLAG-ERRCLI                                 
               MOVE 1 TO WS-ERRCLI-NOEXISTE                             
                                                                        
             WHEN OTHER                                                 
               IF SQLCODE IS EQUAL TO 0 OR                              
                  SQLCODE IS EQUAL TO +100                              
                 NEXT SENTENCE                                          
               ELSE                                                     
                 DISPLAY '--------------------------'                   
                         ' DB2 | SQL --------------------------'        
                                                                        
                 IF SQLCODE IS EQUAL TO -911                            
                   DISPLAY 'ERROR GRAVE: DEADLOCK EN DB2. ABORTANDO...' 
                   PERFORM 9000-SALIDA-ERRORES                          
                      THRU 9000-F-SALIDA-ERRORES                        
                 ELSE                                                   
                   IF SQLCODE IS EQUAL TO -904                          
                     DISPLAY 'ERROR GRAVE: TIMEOUT EN DB2. ABORTANDO...'
                     PERFORM 9000-SALIDA-ERRORES                        
                        THRU 9000-F-SALIDA-ERRORES                      
                   ELSE                                                 
                       MOVE SQLCODE TO CT-SQLCODE-EDIT                  
                       DISPLAY 'ERROR DB2 NO CRÍTICO: ' CT-SQLCODE-EDIT 
                       ADD 1 TO CNT-SQL-ERROR                           
                   END-IF                                               
                 END-IF                                                 
               DISPLAY '-------------------------------------'          
                       '--------------------------'                     
               END-IF                                                   
           END-EVALUATE.                                                
                                                                        
       2800-F-VALIDAR-CLIENTE.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 9 0 0 - V A L I D A R - N O V E D A D            *
      *----------------------------------------------------------------*
                                                                        
       2900-VALIDAR-NOVEDAD.                                            
                                                                        
           MOVE '2900-VALIDAR-NOVEDAD'        TO WS-PARRAFO.            
                                                                        
           EVALUATE TRUE                                                
             WHEN NOV-TIP-NOV = 'AL'                                    
      *      TODOS LOS CAMPOS VALIDADOS                                 
               PERFORM 2910-VALIDAR-TIPDOC                              
                  THRU 2910-F-VALIDAR-TIPDOC                            
                                                                        
               PERFORM 2920-VALIDAR-NRODOC                              
                  THRU 2920-F-VALIDAR-NRODOC                            
                                                                        
               PERFORM 2930-VALIDAR-NROCLI                              
                  THRU 2930-F-VALIDAR-NROCLI                            
                                                                        
               PERFORM 2940-VALIDAR-CLINOM                              
                  THRU 2940-F-VALIDAR-CLINOM                            
                                                                        
               PERFORM 2950-VALIDAR-CLIFEC                              
                  THRU 2950-F-VALIDAR-CLIFEC                            
                                                                        
               PERFORM 2960-VALIDAR-CLISEX                              
                  THRU 2960-F-VALIDAR-CLISEX                            
                                                                        
             WHEN NOV-TIP-NOV = 'CL'                                    
      *      NUMERO                                                     
               PERFORM 2930-VALIDAR-NROCLI                              
                  THRU 2930-F-VALIDAR-NROCLI                            
                                                                        
             WHEN NOV-TIP-NOV = 'CN'                                    
      *      NOMBRE                                                     
               PERFORM 2940-VALIDAR-CLINOM                              
                  THRU 2940-F-VALIDAR-CLINOM                            
                                                                        
             WHEN NOV-TIP-NOV = 'CX'                                    
      *      SEXO                                                       
               PERFORM 2960-VALIDAR-CLISEX                              
                  THRU 2960-F-VALIDAR-CLISEX                            

             WHEN OTHER                                                 
      *      TIPO DE MOVIMIENTO INVALIDO.                               
               ADD  1 TO CNT-NOVEDAD-ERROR                              
               MOVE 1 TO WS-FLAG-ERRNOV                                 
               MOVE 1 TO WS-ERRNOV-TIPNOV                               
           END-EVALUATE.                                                
                                                                        
       2900-F-VALIDAR-NOVEDAD.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 1 0 - V A L I D A R - T I P D O C             *
      *----------------------------------------------------------------*
                                                                        
       2910-VALIDAR-TIPDOC.                                             
                                                                        
           MOVE '2910-VALIDAR-TIPDOC'         TO WS-PARRAFO.            
                                                                        
           IF NOV-TIP-DOC IS NOT EQUAL TO 'DU' AND                      
              NOV-TIP-DOC IS NOT EQUAL TO 'PA' AND                      
              NOV-TIP-DOC IS NOT EQUAL TO 'PE'                          
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-TIPDOC       
           END-IF.                                                      
                                                                        
       2910-F-VALIDAR-TIPDOC.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 2 0 - V A L I D A R - N R O D O C             *
      *----------------------------------------------------------------*
                                                                        
        2920-VALIDAR-NRODOC.                                            
                                                                        
           MOVE '2920-VALIDAR-NRODOC'         TO WS-PARRAFO.            
                                                                        
           IF NOV-NRO-DOC IS NOT NUMERIC                                
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-NRODOC       
           END-IF.                                                      
                                                                        
       2920-F-VALIDAR-NRODOC.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 3 0 - V A L I D A R - N R O C L I             *
      *----------------------------------------------------------------*
                                                                        
        2930-VALIDAR-NROCLI.                                            
                                                                        
           MOVE '2930-VALIDAR-NROCLI'         TO WS-PARRAFO.            

           IF NOV-CLI-NRO IS NOT NUMERIC                                
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-NROCLI       
           END-IF.                                                      
                                                                        
       2930-F-VALIDAR-NROCLI.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 4 0 - V A L I D A R - C L I N O M             *
      *----------------------------------------------------------------*
                                                                        
        2940-VALIDAR-CLINOM.                                            
                                                                        
           MOVE '2940-VALIDAR-CLINOM'         TO WS-PARRAFO.            
                                                                        
           MOVE NOV-CLI-NOMBRE                TO WS-NOMBRE-EMPIEZA.     
                                                                        
           IF WS-NOMBRE-EMPIEZA IS NOT EQUAL TO 'MAZZIT'                
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-CLINOM       
           END-IF.                                                      
                                                                        
       2940-F-VALIDAR-CLINOM.                                           
           EXIT.                                                        

      *----------------------------------------------------------------*
      *              2 9 5 0 - V A L I D A R - C L I F E C             *
      *----------------------------------------------------------------*
                                                                        
        2950-VALIDAR-CLIFEC.                                            
                                                                        
           MOVE '2950-VALIDAR-CLIFEC'         TO WS-PARRAFO.            
                                                                        
           MOVE NOV-CLI-FENAC                 TO WS-FECHA.              
                                                                        
           PERFORM 2952-VALIDAR-ANIO                                    
              THRU 2952-F-VALIDAR-ANIO.                                 
                                                                        
       2950-F-VALIDAR-CLIFEC.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 5 2 - V A L I D A R - A N I O                 *
      *----------------------------------------------------------------*
                                                                        
        2952-VALIDAR-ANIO.                                              
                                                                        
           MOVE '2952-VALIDAR-ANIO'           TO WS-PARRAFO.            
                                                                        
           IF WS-FECHA-ANIO < 1910 OR WS-FECHA-ANIO > 2012              
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-CLIFEC       
           ELSE                                                         
             PERFORM 2954-VALIDAR-MES                                   
                THRU 2954-F-VALIDAR-MES                                 
           END-IF.                                                      
                                                                        
       2952-F-VALIDAR-ANIO.                                             
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                2 9 5 4 - V A L I D A R - M E S                 *
      *----------------------------------------------------------------*
                                                                        
        2954-VALIDAR-MES.                                               
                                                                        
           MOVE '2954-VALIDAR-MES'            TO WS-PARRAFO.            
                                                                        
           EVALUATE TRUE                                                
             WHEN WS-FECHA-MES IS EQUAL TO '01'                         
               OR WS-FECHA-MES IS EQUAL TO '03'                         
               OR WS-FECHA-MES IS EQUAL TO '05'                         
               OR WS-FECHA-MES IS EQUAL TO '07'                         
               OR WS-FECHA-MES IS EQUAL TO '08'                         
               OR WS-FECHA-MES IS EQUAL TO '10'                         
               OR WS-FECHA-MES IS EQUAL TO '12'                         
                 MOVE 31                      TO WS-MAX-DIA             
                 PERFORM 2956-VALIDAR-DIA                               
                    THRU 2956-F-VALIDAR-DIA                             
             WHEN WS-FECHA-MES IS EQUAL TO '04'                         
               OR WS-FECHA-MES IS EQUAL TO '06'                         
               OR WS-FECHA-MES IS EQUAL TO '09'                         
               OR WS-FECHA-MES IS EQUAL TO '11'                         
                 MOVE 30                      TO WS-MAX-DIA             
                 PERFORM 2956-VALIDAR-DIA                               
                    THRU 2956-F-VALIDAR-DIA                             
                                                                        
             WHEN WS-FECHA-MES IS EQUAL TO '02'                         
                 PERFORM 2955-VALIDAR-BISIESTO                          
                    THRU 2955-F-VALIDAR-BISIESTO                        
             WHEN OTHER                                                 
               MOVE 1                         TO WS-FLAG-ERRNOV         
               MOVE 1                         TO WS-ERRNOV-CLIFEC       
           END-EVALUATE.                                                
                                                                        
       2954-F-VALIDAR-MES.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            2 9 5 5 - V A L I D A R - B I S I E S T O           *
      *----------------------------------------------------------------*
                                                                        
        2955-VALIDAR-BISIESTO.                                          
                                                                        
           MOVE '2955-VALIDAR-BISIESTO'       TO WS-PARRAFO.            
                                                                        
           IF FUNCTION MOD(WS-FECHA-ANIO, 4) = 0                        
                                                                        
             IF FUNCTION MOD(WS-FECHA-ANIO, 100) = 0                    
                                                                        
               IF FUNCTION MOD(WS-FECHA-ANIO, 400) = 0                  
                 MOVE 29                          TO WS-MAX-DIA         
                 PERFORM 2956-VALIDAR-DIA                               
                    THRU 2956-F-VALIDAR-DIA                             
                                                                        
               ELSE                                                     
                 MOVE 28                          TO WS-MAX-DIA         
                 PERFORM 2956-VALIDAR-DIA                               
                    THRU 2956-F-VALIDAR-DIA                             
               END-IF                                                   
                                                                        
             ELSE                                                       
               MOVE 29                          TO WS-MAX-DIA           
               PERFORM 2956-VALIDAR-DIA                                 
                  THRU 2956-F-VALIDAR-DIA                               
             END-IF                                                     
                                                                        
           ELSE                                                         
             MOVE 28                          TO WS-MAX-DIA             
             PERFORM 2956-VALIDAR-DIA                                   
                THRU 2956-F-VALIDAR-DIA                                 
           END-IF.                                                      
                                                                        
       2955-F-VALIDAR-BISIESTO.                                         
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 9 5 6 - V A L I D A R - D I A                  *
      *----------------------------------------------------------------*
                                                                        
        2956-VALIDAR-DIA.                                               
                                                                        
           MOVE '2956-VALIDAR-DIA'            TO WS-PARRAFO.            
                                                                        
           IF WS-FECHA-DIA < 01 OR WS-FECHA-DIA > WS-MAX-DIA            
             MOVE 1                           TO WS-FLAG-ERRNOV         
             MOVE 1                           TO WS-ERRNOV-CLIFEC       
           END-IF.                                                      
                                                                        
       2956-F-VALIDAR-DIA.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 9 6 0 - V A L I D A R - C L I S E X             *
      *----------------------------------------------------------------*
                                                                        
        2960-VALIDAR-CLISEX.                                            
                                                                        
           MOVE '2960-VALIDAR-CLISEX'         TO WS-PARRAFO.            
                                                                        
           IF NOV-CLI-SEXO IS NOT EQUAL TO 'F' AND                      
              NOV-CLI-SEXO IS NOT EQUAL TO 'M' AND                      
              NOV-CLI-SEXO IS NOT EQUAL TO 'O'                          
                                                                        
              MOVE 1 TO WS-FLAG-ERRNOV                                  
              MOVE 1 TO WS-ERRNOV-CLISEX                                
           END-IF.                                                      
                                                                        
       2960-F-VALIDAR-CLISEX.                                           
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
      *            3 4 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3400-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3400-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-NOVEDAD-LEIDA             TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMSIN29               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REG. DE NOVEDAD LEIDAS:                '          
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                *'.
           MOVE CNT-NOVEDAD-ERROR TO WS-MASCARA.                        
           DISPLAY '* REG. DE NOVEDAD ERRONEAS:              '          
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                *'.
           MOVE CNT-SQL-ERROR     TO WS-MASCARA.                        
           DISPLAY '* NRO. DE QUERYS SQL ERRONEAS:           '          
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                *'.
           MOVE CNT-NOVEDAD-ALTA  TO WS-MASCARA.                        
           DISPLAY '* NRO. ALTA DE CLIENTES EN BBDD:         '          
                                                    WS-MASCARA '     *'.
           DISPLAY '*                                                *'.
           MOVE CNT-NOVEDAD-MOD TO WS-MASCARA.                          
           DISPLAY '* NRO. MODIFICACION DE CLIENTES EN BBDD: '          
                                                    WS-MASCARA '     *'.
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