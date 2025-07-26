      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMB7CAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-JULIO-16.                                
                                                                        
      *----------------------------------------------------------------*
      *  ACTIVIDAD CLASE ASINCRONICA 18 | APAREO CON DOS CURSORES DB2  *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ESTE PROGRAMA APAREA DOS CURSORES PROVENIENTES DE LAS TABLAS   *
      * TBCURCLI Y TBCURCTA A PARTIR DE NROCLI.                        *
      *   - POR CADA CLIENTE DEL CURSOR TBCURCTA ENCONTRADO EN EL DE   *
      * TBCURCLI SE GENERA UN REGISTRO EN UN LISTADO (FBA).            *
      *   - EN CUALQUIER OTRO CASO, EL SISTEMA INFORMA POR DISPLAY EL  *
      * RESULTADO.                                                     *
      *   - AL FINAL DEL PGM SE MUESTRAN ESTADISTICAS DE               *
      *     + LEIDOS EN TBCURCTA                                       *
      *     + LEIDOS EN TBCURCLI                                       *
      *     + ENCONTRADOS                                              *
      *     + NO ENCONTRADOS                                           *
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
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
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
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMB7CAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-CLIENTES                   PIC X(08)  VALUE 'CLIENTES'.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-EVALUATE                   PIC X(08)  VALUE 'EVAUATE '.
           02 CT-CURSOR-CTA                 PIC X(08)  VALUE 'CUR CTA '.
           02 CT-CURSOR-CLI                 PIC X(08)  VALUE 'CUR CLI '.
           02 CT-FETCH                      PIC X(08)  VALUE 'FETCH   '.
           02 CT-NOT-FOUND                  PIC S9(9) COMP VALUE +100.  
           02 CT-FOUND                      PIC S9(9) COMP VALUE 0.     
           02 CT-SQLCODE-EDIT               PIC ++++++9999 VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-MASCARA                    PIC ZZZ9.                   
                                                                        
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
           02 CNT-CURCTA-LEIDO              PIC 9(03)  VALUE ZEROS.     
           02 CNT-CURCLI-LEIDO              PIC 9(03)  VALUE ZEROS.     
           02 CNT-ENCONTRADO                PIC 9(03)  VALUE ZEROS.     
           02 CNT-NOENCONTRADO              PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-CURCLI.                                              
          02 CURCLI-NROCLI                  PIC X(02)  VALUE ZEROS.     

       01 WS-CLAVE-CURCTA.                                              
          02 CURCTA-NROCLI                  PIC X(02)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *              A R E A  D E  F I L E - S T A T U S               *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-CLIENTES                   PIC X(02).                  
              88 FS-CLIENTES-OK                        VALUE '00'.      
              88 FS-CLIENTES-EOF                       VALUE '10'.      
                                                                        
      *----------------------------------------------------------------*
      *            A R E A  D E  C U R S O R - S T A T U S             *
      *----------------------------------------------------------------*
                                                                        
       01 CS-CURSOR-STATUS.                                             
           02 CS-CURSOR-CLI                 PIC X(02).                  
              88 CS-CURSOR-CLI-OK                      VALUE '00'.      
              88 CS-CURSOR-CLI-EOC                     VALUE '10'.      
                                                                        
           02 CS-CURSOR-CTA                 PIC X(02).                  
              88 CS-CURSOR-CTA-OK                      VALUE '00'.      
              88 CS-CURSOR-CTA-EOC                     VALUE '10'.      
                                                                        
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
                                                                        
      * CURSOR TBCURCLI                                                 
                                                                        
           EXEC SQL                                                     
             DECLARE CURSOR-CLI CURSOR FOR                              
               SELECT TIPDOC,                                           
                      NRODOC,                                           
                      NROCLI,                                           
                      NOMAPE                                            
                FROM KC02787.TBCURCLI                                   
            ORDER BY NROCLI                                             
           END-EXEC.                                                    
                                                                        
      * CURSOR TBCURCTA                                                 
                                                                        
           EXEC SQL                                                     
             DECLARE CURSOR-CTA CURSOR FOR                              
               SELECT NROCLI,                                           
                      SUCUEN                                            
                FROM KC02787.TBCURCTA                                   
            ORDER BY NROCLI                                             
           END-EXEC.                                                    
                                                                        
      *----------------------------------------------------------------*
      *     A R E A  D E  F O R M A T O  D E  A R C H I V O  F B A     *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *                F O R M A T O  D E  H E A D E R                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-HEADER-INICIAL.                                           
           02  FILLER          PIC X(10) VALUE SPACES.                  
           02  FILLER          PIC X(20) VALUE "CLIENTES DE LA TABLA".  
           02  FILLER          PIC X(22) VALUE " TBCURCTA ENCONTRADOS ".
           02  FILLER          PIC X(20) VALUE "EN LA TABLA TBCURCLI".  
           02  FILLER          PIC X(10) VALUE " - FECHA: ".            
           02  WS-FECHA-TITULO PIC X(10).                               
           02  FILLER          PIC X(40) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  C A B E C E R A               *
      *----------------------------------------------------------------*
                                                                        
       01  WS-CABECERA-COLUMNAS.                                        
           02  FILLER          PIC X(18) VALUE "| NRO. DE CLIENTE ".    
           02  FILLER          PIC X(20) VALUE "| TIPO DE DOCUMENTO ".  
           02  FILLER          PIC X(20) VALUE "| NRO. DE DOCUMENTO ".  
           02  FILLER          PIC X(17) VALUE "|       NOMBRE Y ".     
           02  FILLER          PIC X(16) VALUE "APELLIDO        ".      
           02  FILLER          PIC X(20) VALUE "| NRO. DE SUCURSAL |".  
           02  FILLER          PIC X(21) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *          F O R M A T O  D E  S U B - C A B E C E R A           *
      *----------------------------------------------------------------*
                                                                        
       01  WS-SUBCABECERA.                                              
           02  FILLER          PIC X(18) VALUE "|                 ".    
           02  FILLER          PIC X(20) VALUE "|                   ".  
           02  FILLER          PIC X(20) VALUE "|                   ".  
           02  FILLER          PIC X(17) VALUE "|                ".     
           02  FILLER          PIC X(16) VALUE "                ".      
           02  FILLER          PIC X(20) VALUE "|                  |".  
           02  FILLER          PIC X(21) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  D E T A L L E                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-DETALLE.                                                  
           02  FILLER          PIC X(08) VALUE "|       ".              
           02  DET-NRO-CLI     PIC ZZ9.                                 
           02  FILLER          PIC X(07) VALUE "       ".               
           02  FILLER          PIC X(09) VALUE "|        ".             
           02  DET-TIP-DOC     PIC X(02).                               
           02  FILLER          PIC X(09) VALUE "         ".             
           02  FILLER          PIC X(05) VALUE "|   ".                  
           02  DET-NRO-DOC     PIC 9(11).                               
           02  FILLER          PIC X(04) VALUE "    ".                  
           02  FILLER          PIC X(02) VALUE "| ".                    
           02  DET-NOM-APE     PIC X(30).                               
           02  FILLER          PIC X(01) VALUE " ".                     
           02  FILLER          PIC X(09) VALUE "|        ".             
           02  DET-NRO-SUC     PIC 9(02).                               
           02  FILLER          PIC X(09) VALUE "        |".             
           02  FILLER          PIC X(21) VALUE SPACES.                  
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           IF NOT CS-CURSOR-CLI-EOC AND NOT CS-CURSOR-CTA-EOC           
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
                UNTIL CS-CURSOR-CLI-EOC AND CS-CURSOR-CTA-EOC           
           END-IF.                                                      
                                                                        
           PERFORM 2700-GRABAR-CIERRE                                   
              THRU 2700-F-GRABAR-CIERRE.                                
                                                                        
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
                                                                        
           PERFORM 1100-ABRIR-ARCHIVOS                                  
              THRU 1100-F-ABRIR-ARCHIVOS.                               
                                                                        
           PERFORM 1200-ABRIR-CURSORES                                  
              THRU 1200-F-ABRIR-CURSORES.                               
                                                                        
           PERFORM 2200-FETCH-CLI                                       
              THRU 2200-F-FETCH-CLI.                                    
                                                                        
           PERFORM 2400-FETCH-CTA                                       
              THRU 2400-F-FETCH-CTA.                                    
                                                                        
           PERFORM 1800-OBTENER-FECHA                                   
              THRU 1800-F-OBTENER-FECHA.                                
                                                                        
           PERFORM 2500-GRABAR-TITULOS                                  
              THRU 2500-F-GRABAR-TITULOS.                               
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
              WHEN WS-CLAVE-CURCLI IS EQUAL TO WS-CLAVE-CURCTA          
                 ADD 1 TO CNT-ENCONTRADO                                
                                                                        
                 PERFORM 2600-GRABAR-REGISTRO                           
                    THRU 2600-F-GRABAR-REGISTRO                         
                                                                        
                 PERFORM 2200-FETCH-CLI                                 
                    THRU 2200-F-FETCH-CLI                               
                                                                        
                 PERFORM 2400-FETCH-CTA                                 
                    THRU 2400-F-FETCH-CTA                               
                                                                        
              WHEN WS-CLAVE-CURCLI IS GREATER THAN WS-CLAVE-CURCTA      
                 MOVE WS-CTA-NROCLI   TO WS-MASCARA                     
           DISPLAY '**************************************************' 
           DISPLAY '*  CLIENTE ' WS-MASCARA                             
                                  ' EN TABLA TBCURCTA NO ENCONTRADO  *' 
           DISPLAY '**************************************************' 
                 ADD 1 TO CNT-NOENCONTRADO                              
                                                                        
                 PERFORM 2400-FETCH-CTA                                 
                    THRU 2400-F-FETCH-CTA                               
                                                                        
              WHEN WS-CLAVE-CURCLI IS LESS THAN WS-CLAVE-CURCTA         
                 MOVE WS-CLI-NROCLI   TO WS-MASCARA                     
           DISPLAY '**************************************************' 
           DISPLAY '*       CLIENTE ' WS-MASCARA                        
                                       ' SIN CUENTA EN TBCURCTA      *' 
           DISPLAY '**************************************************' 
                                                                        
                 PERFORM 2200-FETCH-CLI                                 
                    THRU 2200-F-FETCH-CLI                               
                                                                        
              WHEN OTHER                                                
                 MOVE HIGH-VALUES          TO WS-CLAVE-CURCTA           
                 MOVE HIGH-VALUES          TO WS-CLAVE-CURCLI           
                                                                        
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

           PERFORM 3400-CERRAR-CURSORES                                 
              THRU 3400-F-CERRAR-CURSORES.                              
                                                                        
           PERFORM 3600-MOSTRAR-TOTALES                                 
              THRU 3600-F-MOSTRAR-TOTALES.                              
                                                                        
       3000-F-FIN.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             M O D U L O S  S E C U N D A R I O S               *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *             1 1 0 0 - A B R I R - A R C H I V O S              *
      *----------------------------------------------------------------*
                                                                        
       1100-ABRIR-ARCHIVOS.                                             
                                                                        
           MOVE '1100-ABRIR-ARCHIVOS'         TO WS-PARRAFO.            
                                                                        
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
                                                                        
       1100-F-ABRIR-ARCHIVOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             1 2 0 0 - A B R I R - C U R S O R E S              *
      *----------------------------------------------------------------*
                                                                        
       1200-ABRIR-CURSORES.                                             
                                                                        
           MOVE '1200-ABRIR-CURSORES'         TO WS-PARRAFO.            
                                                                        
           EXEC SQL                                                     
              OPEN CURSOR-CTA                                           
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE HIGH-VALUES          TO WS-CLAVE-CURCTA              
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-OPEN              TO AUX-ERR-ACCION               
              MOVE CT-CURSOR-CTA        TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
              OPEN CURSOR-CLI                                           
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE HIGH-VALUES          TO WS-CLAVE-CURCLI              
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-OPEN              TO AUX-ERR-ACCION               
              MOVE CT-CURSOR-CLI        TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    

                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-CURSORES.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                  1 4 0 0 - V A R H O S T - CLI                 *
      *----------------------------------------------------------------*
                                                                        
       1400-VARHOST-CLI.                                                
                                                                        
           MOVE '1400-VARHOST-CLI'            TO WS-PARRAFO.            
                                                                        
           INITIALIZE WS-CLI-TIPDOC                                     
                      WS-CLI-NRODOC                                     
                      WS-CLI-NROCLI                                     
                      WS-CLI-NOMAPE                                     
              REPLACING ALPHANUMERIC BY SPACES                          
                             NUMERIC BY ZEROS.                          
                                                                        
       1400-F-VARHOST-CLI.                                              
           EXIT.                                                        

      *----------------------------------------------------------------*
      *                  1 6 0 0 - V A R H O S T - CTA                 *
      *----------------------------------------------------------------*
                                                                        
       1600-VARHOST-CTA.                                                
                                                                        
           MOVE '1600-VARHOST-CTA'            TO WS-PARRAFO.            
                                                                        
           INITIALIZE WS-CTA-NROCLI                                     
                      WS-CTA-SUCUEN                                     
              REPLACING ALPHANUMERIC BY SPACES                          
                             NUMERIC BY ZEROS.                          
                                                                        
       1600-F-VARHOST-CTA.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 8 0 0 - O B T E N E R - F E C H A              *
      *----------------------------------------------------------------*
                                                                        
       1800-OBTENER-FECHA.                                              
                                                                        
           MOVE '1800-OBTENER-FECHA'          TO WS-PARRAFO.            
                                                                        
           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.                  

           STRING WS-DIA   DELIMITED BY SIZE                            
                  '/'      DELIMITED BY SIZE                            
                  WS-MES   DELIMITED BY SIZE                            
                  '/'      DELIMITED BY SIZE                            
                  WS-ANIO  DELIMITED BY SIZE                            
              INTO WS-FECHA-TITULO                                      
           END-STRING.                                                  
                                                                        
       1800-F-OBTENER-FECHA.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                 2 2 0 0 - F E T C H - C L I                    *
      *----------------------------------------------------------------*
                                                                        
       2200-FETCH-CLI.                                                  
                                                                        
           MOVE '2200-FETCH-CLI'              TO WS-PARRAFO.            
                                                                        
           PERFORM 1400-VARHOST-CLI                                     
              THRU 1400-F-VARHOST-CLI.                                  
                                                                        
           EXEC SQL                                                     
              FETCH  CURSOR-CLI                                         
                     INTO                                               
                        :WS-CLI-TIPDOC,                                 
                        :WS-CLI-NRODOC,                                 
                        :WS-CLI-NROCLI,                                 
                        :WS-CLI-NOMAPE                                  
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL CT-FOUND                             
               MOVE WS-CLI-NROCLI        TO WS-CLAVE-CURCLI             
               ADD 1 TO CNT-CURCLI-LEIDO                                
                                                                        
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               SET CS-CURSOR-CLI-EOC     TO TRUE                        
               MOVE HIGH-VALUES          TO WS-CLAVE-CURCLI             
                                                                        
             WHEN OTHER                                                 
               MOVE HIGH-VALUES          TO WS-CLAVE-CURCLI             
               MOVE SQLCODE              TO CT-SQLCODE-EDIT             
                                                                        
               MOVE CT-FETCH             TO AUX-ERR-ACCION              
               MOVE CT-CURSOR-CLI        TO AUX-ERR-NOMBRE              
               MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS              
               MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE             
               MOVE 10                   TO W-N-ERROR                   
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
           END-EVALUATE.                                                
                                                                        
       2200-F-FETCH-CLI.                                                
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                 2 4 0 0 - F E T C H - C T A                    *
      *----------------------------------------------------------------*
                                                                        
       2400-FETCH-CTA.                                                  
                                                                        
           MOVE '2400-FETCH-CTA'              TO WS-PARRAFO.            
                                                                        
           PERFORM 1600-VARHOST-CTA                                     
              THRU 1600-F-VARHOST-CTA.                                  
                                                                        
           EXEC SQL                                                     
              FETCH  CURSOR-CTA                                         
                     INTO                                               
                        :WS-CTA-NROCLI,                                 
                        :WS-CTA-SUCUEN                                  
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
             WHEN SQLCODE IS EQUAL CT-FOUND                             
               MOVE WS-CTA-NROCLI        TO WS-CLAVE-CURCTA             
               ADD 1 TO CNT-CURCTA-LEIDO                                
                                                                        
             WHEN SQLCODE IS EQUAL TO CT-NOT-FOUND                      
               SET CS-CURSOR-CTA-EOC     TO TRUE                        
               MOVE HIGH-VALUES          TO WS-CLAVE-CURCTA             
                                                                        
             WHEN OTHER                                                 
               MOVE HIGH-VALUES          TO WS-CLAVE-CURCTA             
               MOVE SQLCODE              TO CT-SQLCODE-EDIT             
                                                                        
               MOVE CT-FETCH             TO AUX-ERR-ACCION              
               MOVE CT-CURSOR-CTA        TO AUX-ERR-NOMBRE              
               MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS              
               MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE             
               MOVE 10                   TO W-N-ERROR                   
                                                                        
                 PERFORM 9000-SALIDA-ERRORES                            
                    THRU 9000-F-SALIDA-ERRORES                          
           END-EVALUATE.                                                
                                                                        
       2400-F-FETCH-CTA.                                                
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 5 0 0 - G R A B A R - T I T U L O S             *
      *----------------------------------------------------------------*
                                                                        
       2500-GRABAR-TITULOS.                                             
                                                                        
           MOVE '2500-GRABAR-TITULOS'         TO WS-PARRAFO.            
                                                                        
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
                                                                        
       2500-F-GRABAR-TITULOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            2 6 0 0 - G R A B A R - R E G I S T R O             *
      *----------------------------------------------------------------*
                                                                        
       2600-GRABAR-REGISTRO.                                            
                                                                        
           MOVE '2600-GRABAR-REGISTRO'        TO WS-PARRAFO.            
                                                                        
           IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                          
              PERFORM 2500-GRABAR-TITULOS                               
                 THRU 2500-F-GRABAR-TITULOS                             
           END-IF.                                                      
                                                                        
           MOVE WS-CLI-NROCLI       TO DET-NRO-CLI                      
           MOVE WS-CLI-TIPDOC       TO DET-TIP-DOC                      
           MOVE WS-CLI-NRODOC       TO DET-NRO-DOC                      
           MOVE WS-CLI-NOMAPE       TO DET-NOM-APE                      
           MOVE WS-CTA-SUCUEN       TO DET-NRO-SUC                      
                                                                        
           WRITE LINEA-IMPRESION FROM WS-DETALLE                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           PERFORM 2800-EVALUAR-GRABACION                               
              THRU 2800-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 1 TO WS-LINEA-ACTUAL.                                    
                                                                        
       2600-F-GRABAR-REGISTRO.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              2 7 0 0 - G R A B A R - C I E R R E               *
      *----------------------------------------------------------------*
                                                                        
       2700-GRABAR-CIERRE.                                              
                                                                        
           MOVE '2700-GRABAR-CIERRE'          TO WS-PARRAFO.            
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE 'FINAL LISTADO CLIENTES' TO LINEA-IMPRESION.            
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
       2700-F-GRABAR-CIERRE.                                            
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
      *            3 2 0 0 - C E R R A R - A R C H I V O S             *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'          TO WS-PARRAFO.          
                                                                        
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
      *            3 4 0 0 - C E R R A R - C U R S O R E S             *
      *----------------------------------------------------------------*

       3400-CERRAR-CURSORES.                                            
                                                                        
           MOVE '3200-CERRAR-CURSORES'          TO WS-PARRAFO.          
                                                                        
           EXEC SQL                                                     
              CLOSE CURSOR-CLI                                          
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-CLOSE             TO AUX-ERR-ACCION               
              MOVE CT-CURSOR-CLI        TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
              CLOSE CURSOR-CTA                                          
           END-EXEC.                                                    

           IF SQLCODE NOT EQUAL ZEROS                                   
              MOVE SQLCODE              TO CT-SQLCODE-EDIT              
                                                                        
              MOVE CT-CLOSE             TO AUX-ERR-ACCION               
              MOVE CT-CURSOR-CTA        TO AUX-ERR-NOMBRE               
              MOVE CT-SQLCODE-EDIT      TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
                PERFORM 9000-SALIDA-ERRORES                             
                   THRU 9000-F-SALIDA-ERRORES                           
                                                                        
           END-IF.                                                      
                                                                        
       3400-F-CERRAR-CURSORES.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            3 6 0 0 - M O S T R A R - T O T A L E S             *
      *----------------------------------------------------------------*
                                                                        
       3600-MOSTRAR-TOTALES.                                            
                                                                        
           MOVE '3600-MOSTRAR-TOTALES'        TO WS-PARRAFO.            
                                                                        
           MOVE CNT-CURCTA-LEIDO              TO WS-MASCARA.            
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMB7CAB               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '* CUENTAS LEIDAS (TBCURCTA):              '         
                                                      WS-MASCARA '   *'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-CURCLI-LEIDO              TO WS-MASCARA.            
           DISPLAY '* CLIENTES LEIDOS (TBCURCLI):             '         
                                                      WS-MASCARA '   *'.
                                                                        
           MOVE CNT-ENCONTRADO                TO WS-MASCARA.            
           DISPLAY '* CLIENTES ENCONTRADOS:                   '         
                                                      WS-MASCARA '   *'.
                                                                        
           MOVE CNT-NOENCONTRADO              TO WS-MASCARA.            
           DISPLAY '* CLIENTES NO ENCONTRADOS:                '         
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