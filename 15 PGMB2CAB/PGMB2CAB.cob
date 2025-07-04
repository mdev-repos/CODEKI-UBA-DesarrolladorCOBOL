      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMB2CAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-04                                 
                                                                        
      *----------------------------------------------------------------*
      *     ACTIVIDAD CLASE SINCRONICA 27 | PGM CON DB2 SIN CURSOR     *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PGM LEE UN ARCHIVO VSAM GENERADO PREVIAMENTE ( KC03CAB.-  *
      * -NOVECLI ) Y A PARTIR DE SUS REGISTROS GENERA UNA QUERY INSERT *
      * EN LA TABLA TBCURCLI.                                          *
      * AL FINAL DEL PGM HACE DISPLAY DE                               *
      *    - CANTIDAD DE NOVEDADES LEIDAS                              *
      *    - CANTIDAD DE NOVEDADES INSERTADAS                          *
      *    - CANTIDAD DE NOVEDADES ERRONEAS                            *
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
                                                                        
      * PGM CON VSAM ( NOVEDAD )                                        
                                                                        
           SELECT NOVEDAD   ASSIGN TO NOVEDAD                           
                            ORGANIZATION IS INDEXED                     
                            ACCESS IS SEQUENTIAL                        
                            RECORD KEY IS KEY-CLAVE                     
                            FILE STATUS IS FS-NOVEDAD.                  
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      * NOVEDAD ( ARCHIVO VSAM )                                        
                                                                        
       FD NOVEDAD.                                                      
                                                                        
       01 REG-NOVEDAD.                                                  
          03 KEY-CLAVE    PIC X(17).                                    
          03 FILLER       PIC X(227).                                   
                                                                        
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
           02 CT-NOVEDAD                    PIC X(08)  VALUE 'NOVEDAD'. 
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE  +100. 
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
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
           02 CNT-NOVEDAD-LEIDAS            PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-INSERTADAS        PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOVEDAD-ERRONEAS          PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
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
                                                                        
           COPY TBVCLIEN.                                               
                                                                        
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
                                                                        
           PERFORM 1400-LEER-NOVEDAD                                    
              THRU 1400-F-LEER-NOVEDAD.                                 
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           PERFORM 2200-QUERY-INSERT                                    
              THRU 2200-F-QUERY-INSERT.                                 
                                                                        
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
                                                                        
           OPEN INPUT   NOVEDAD.                                        
                                                                        
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
      *               1 4 0 0 - L E E R - N O V E D A D                *
      *----------------------------------------------------------------*
                                                                        
       1400-LEER-NOVEDAD.                                               
                                                                        
           MOVE '1400-LEER-NOVEDAD'           TO WS-PARRAFO.            
                                                                        
           READ NOVEDAD INTO WK-TBCLIE.                                 
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-NOVEDAD-OK                                       
                    ADD 1                     TO CNT-NOVEDAD-LEIDAS     
                                                                        
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
      *               2 2 0 0 - Q U E R Y - I N S E R T                *
      *----------------------------------------------------------------*
                                                                        
       2200-QUERY-INSERT.                                               
                                                                        
           MOVE '2200-QUERY-INSERT'           TO WS-PARRAFO.            
                                                                        
           MOVE WK-CLI-TIPO-DOCUMENTO         TO WS-CLI-TIPDOC.         
           MOVE WK-CLI-NRO-DOCUMENTO          TO WS-CLI-NRODOC.         
           MOVE WK-CLI-NRO-CLIENTE            TO WS-CLI-NROCLI.         
           MOVE WK-CLI-APELLIDO-CLIENTE       TO WS-CLI-NOMAPE.         
           MOVE WK-CLI-FECHA-NACIMIENTO       TO WS-CLI-FECNAC.         
           MOVE WK-CLI-SEXO                   TO WS-CLI-SEXO.           
                                                                        
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
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE = CT-NOT-FOUND                                
               ADD 1 TO CNT-NOVEDAD-ERRONEAS                            
               MOVE SQLCODE TO CT-SQLCODE-EDIT                          
               DISPLAY 'ERROR INSERT  : ' CT-SQLCODE-EDIT               
                                                                        
             WHEN SQLCODE = 0                                           
               ADD 1 TO CNT-NOVEDAD-INSERTADAS                          
               DISPLAY 'INSERT OK  ' WS-CLI-TIPDOC  WS-CLI-NRODOC       
                                                                        
             WHEN OTHER                                                 
              ADD 1 TO CNT-NOVEDAD-ERRONEAS                             
              MOVE SQLCODE TO CT-SQLCODE-EDIT                           
              DISPLAY 'ERROR DB2 '  CT-SQLCODE-EDIT                     
           END-EVALUATE.                                                
                                                                        
       2200-F-QUERY-INSERT.                                             
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
                                                                        
           MOVE CNT-NOVEDAD-LEIDAS            TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMB2CAB               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REG. DE NOVEDAD LEIDAS:                '          
                                                     WS-MASCARA '    *'.
           DISPLAY '*                                                 '.
           MOVE CNT-NOVEDAD-INSERTADAS TO WS-MASCARA.                   
           DISPLAY '* REG. DE NOVEDAD INSERTADAS:            '          
                                                     WS-MASCARA '    *'.
           DISPLAY '*                                                 '.
           MOVE CNT-NOVEDAD-ERRONEAS TO WS-MASCARA.                     
           DISPLAY '* REG. DE NOVEDAD ERRONEAS:              '          
                                                     WS-MASCARA '    *'.
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