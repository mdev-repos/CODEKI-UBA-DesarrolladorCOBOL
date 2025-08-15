      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMVSCAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-MAYO-28.                                 
                                                                        
      *----------------------------------------------------------------*
      * ACTIVIDAD CLASE SINCRONICA 22 | LECTURA VSAM + SALIDA ARCH FBA *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA REALIZA LA LECTURA DEL ARCHIVO VSAM KSDS CLIEN- *
      *  -TES PARA LUEGO IMPRIMIR CADA UNO DE LOS REGISTROS LEIDOS EN  *
      *  UN ARCHIVO DE SALIDA FBA DE 132 BYTES.                        *
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
                           ORGANIZATION IS INDEXED                      
                           ACCESS IS SEQUENTIAL                         
                           RECORD KEY IS    KEY-CLAVE                   
                           FILE STATUS IS FS-CLIENTES.                  
                                                                        
           SELECT LISTADO ASSIGN TO LISTADO                             
                                    FILE STATUS IS FS-LISTADO.          
                                                                        
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
                                                                        
       FD   LISTADO                                                     
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
       01   LINEA-IMPRESION                                 PIC X(132). 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *    A R E A  D E  F O R M A T O  D E  A R CH I V O  F B A       *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *                F O R M A T O  D E  H E A D E R                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-HEADER-INICIAL.                                           
           02  FILLER          PIC X(10) VALUE SPACES.                  
           02  FILLER          PIC X(19) VALUE "DETALLE DE CLIENTES".   
           02  FILLER          PIC X(11) VALUE SPACES.                  
           02  FILLER          PIC X(07) VALUE "FECHA: ".               
           02  WS-FECHA        PIC X(10).                               
           02  FILLER          PIC X(21) VALUE SPACES.                  
           02  WS-PAGINA       PIC Z9.                                  
           02  FILLER          PIC X(52) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  C A B E C E R A               *
      *----------------------------------------------------------------*
                                                                        
       01  WS-CABECERA-COLUMNAS.                                        
           02  FILLER          PIC X(08) VALUE "| TIPO |".              
           02  FILLER          PIC X(14) VALUE "  DOCUMENTO  |".        
           02  FILLER          PIC X(06) VALUE " SUC |".                
           02  FILLER          PIC X(16) VALUE "  TIPO CUENTA  |".      
           02  FILLER          PIC X(07) VALUE " NRO | ".               
           02  FILLER          PIC X(22) VALUE "        SALDO        |".
           02  FILLER          PIC X(13) VALUE "    FECHA   |".         
           02  FILLER          PIC X(15) VALUE "     SEXO     |".       
           02  FILLER          PIC X(20) VALUE " NOMBRE Y APELLIDO |".  
           02  FILLER          PIC X(11) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *           F O R M A T O  D E  S U B C A B E C E R A            *
      *----------------------------------------------------------------*
                                                                        
       01  WS-SUBCABECERA.                                              
           02  FILLER          PIC X(08) VALUE "|      |".              
           02  FILLER          PIC X(14) VALUE "             |".        
           02  FILLER          PIC X(06) VALUE "     |".                
           02  FILLER          PIC X(16) VALUE "               |".      
           02  FILLER          PIC X(07) VALUE "     | ".               
           02  FILLER          PIC X(22) VALUE "                     |".
           02  FILLER          PIC X(13) VALUE "            |".         
           02  FILLER          PIC X(15) VALUE "              |".       
           02  FILLER          PIC X(20) VALUE "                   |".  
           02  FILLER          PIC X(11) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  D E T A L L E                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-DETALLE.                                                  
           02  FILLER          PIC X(03) VALUE "|  ".                   
           02  DET-TIPO-DOC    PIC X(02) VALUE SPACES.                  
           02  FILLER          PIC X(04) VALUE "  | ".                  
           02  DET-NRO-DOC     PIC 9(11) VALUE ZEROS.                   
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-NRO-SUC     PIC X(02) VALUE SPACES.                  
           02  FILLER          PIC X(05) VALUE "  |  ".                 
           02  DET-TIPO-CTA    PIC X(12) VALUE SPACES.                  
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-NRO-CLI     PIC 9(03) VALUE ZEROS.                   
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-SALDO-CLI   PIC $ZZZ.ZZZ.ZZZ.ZZ9,99-.                
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-FECHA-CLI   PIC X(10) VALUE SPACES.                  
           02  FILLER          PIC X(05) VALUE " |   ".                 
           02  DET-SEXO-CLI    PIC X(09) VALUE SPACES.                  
           02  FILLER          PIC X(06) VALUE "  |   ".                
           02  DET-NOMAPE-CLI  PIC X(15) VALUE SPACES.                  
           02  FILLER          PIC X(02) VALUE " |".                    
           02  FILLER          PIC X(11) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMVSCAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-CLIENTES                   PIC X(08)  VALUE 'ENTRADA '.
           02 CT-LISTADO                    PIC X(08)  VALUE 'LISTADO '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01  WS-VARIABLES.                                                
           02 WS-PARRAFO                    PIC X(50).                  
                                                                        
       01  WS-FECHA-SISTEMA.                                            
           02 WS-ANIO                       PIC 9(04) VALUE ZEROS.      
           02 WS-MES                        PIC 9(02) VALUE ZEROS.      
           02 WS-DIA                        PIC 9(02) VALUE ZEROS.      
                                                                        
       01  WS-CONTROL-PAGINA.                                           
           02  WS-MAX-LINEAS                PIC 9(02)  VALUE 60.        
           02  WS-LINEA-ACTUAL              PIC 9(02)  VALUE ZEROS.     
           02  WS-PAGINA-ACTUAL             PIC 9(02)  VALUE ZEROS.     
                                                                        
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
           02 CNT-LISTADO-GRABADOS          PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-CLIENTES                   PIC X(02).                  
              88 FS-CLIENTES-OK                        VALUE '00'.      
              88 FS-CLIENTES-EOF                       VALUE '10'.      
                                                                        
           02 FS-LISTADO                    PIC X(02).                  
              88 FS-LISTADO-OK                         VALUE '00'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CPCLIE.                                                 
                                                                        
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
                                                                        
           PERFORM 2600-IMPRIMIR-CIERRE                                 
              THRU 2600-F-IMPRIMIR-CIERRE.                              
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           INITIALIZE WS-VARIABLES.                                     
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO.            
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS                                  
              THRU 1200-F-ABRIR-ARCHIVOS.                               
                                                                        
           PERFORM 1400-LEER-CLIENTES                                   
              THRU 1400-F-LEER-CLIENTES.                                
                                                                        
           PERFORM 1500-OBTENER-FECHA                                   
              THRU 1500-F-OBTENER-FECHA.                                
                                                                        
           PERFORM 1600-IMPRIMIR-CABECERA                               
              THRU 1600-F-IMPRIMIR-CABECERA.                            
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           PERFORM 2200-IMPRIMIR-REGISTRO                               
              THRU 2200-F-IMPRIMIR-REGISTRO.                            
                                                                        
           PERFORM 1400-LEER-CLIENTES                                   
              THRU 1400-F-LEER-CLIENTES.                                
                                                                        
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
                                                                        
           OPEN INPUT CLIENTES                                          
                OUTPUT LISTADO.                                         

           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-LISTADO-OK                                         
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-LISTADO                 TO AUX-ERR-NOMBRE         
              MOVE FS-LISTADO                 TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-ARCHIVOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 4 0 0 - L E E R - E N T R A D A                *
      *----------------------------------------------------------------*
                                                                        
       1400-LEER-CLIENTES.                                              
                                                                        
           MOVE '1400-LEER-CLIENTES'          TO WS-PARRAFO.            
                                                                        
           READ CLIENTES INTO REG-CLIENTE.                              
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-CLIENTES-OK                                      
                    ADD 1 TO CNT-CLIENTES-LEIDOS                        
                                                                        
               WHEN FS-CLIENTES-EOF                                     
                    SET FS-CLIENTES-EOF       TO TRUE                   
                                                                        
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
      *             1 5 0 0 - O B T E N E R - F E C H A                *
      *----------------------------------------------------------------*
                                                                        
       1500-OBTENER-FECHA.                                              
                                                                        
           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.                  
                                                                        
           STRING WS-DIA   DELIMITED BY SIZE                            
                  "/"      DELIMITED BY SIZE                            
                  WS-MES   DELIMITED BY SIZE                            
                  "/"      DELIMITED BY SIZE                            
                  WS-ANIO  DELIMITED BY SIZE                            
               INTO  WS-FECHA                                           
           END-STRING.                                                  
                                                                        
       1500-F-OBTENER-FECHA.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *           1 6 0 0 - I M P R I M I R - C A B E C E R A          *
      *----------------------------------------------------------------*
                                                                        
       1600-IMPRIMIR-CABECERA.                                          
                                                                        
           MOVE '1600-IMPRIMIR-CABECERA'             TO WS-PARRAFO.     
                                                                        
           MOVE 0 TO WS-LINEA-ACTUAL.                                   
                                                                        
           ADD 1 TO WS-PAGINA-ACTUAL.                                   
                                                                        
           MOVE WS-PAGINA-ACTUAL TO WS-PAGINA.                          
                                                                        
           WRITE LINEA-IMPRESION FROM WS-HEADER-INICIAL                 
              AFTER ADVANCING PAGE.                                     
                                                                        
           WRITE LINEA-IMPRESION FROM WS-CABECERA-COLUMNAS              
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           WRITE LINEA-IMPRESION FROM WS-SUBCABECERA                    
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           ADD 3 TO WS-LINEA-ACTUAL.                                    
                                                                        
       1600-F-IMPRIMIR-CABECERA.                                        
           EXIT.                                                        

      *----------------------------------------------------------------*
      *          2 2 0 0 - I M P R I M I R - R E G I S T R O           *
      *----------------------------------------------------------------*
                                                                        
       2200-IMPRIMIR-REGISTRO.                                          
                                                                        
           MOVE '2200-IMPRIMIR-REGISTRO'      TO WS-PARRAFO.            
                                                                        
           IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                          
              PERFORM 1600-IMPRIMIR-CABECERA                            
                 THRU 1600-F-IMPRIMIR-CABECERA                          
           END-IF.                                                      
                                                                        
           MOVE CLI-TIP-DOC       TO DET-TIPO-DOC.                      
           MOVE CLI-NRO-DOC       TO DET-NRO-DOC.                       
           MOVE CLI-NRO-SUC       TO DET-NRO-SUC.                       
           MOVE CLI-NRO           TO DET-NRO-CLI.                       
           MOVE CLI-NOMAPE        TO DET-NOMAPE-CLI.                    
                                                                        
           PERFORM 2400-FORMATEAR-CAMPO                                 
              THRU 2400-F-FORMATEAR-CAMPOS.                             
                                                                        
           WRITE LINEA-IMPRESION FROM WS-DETALLE                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           PERFORM 2900-EVALUAR-GRABACION                               
              THRU 2900-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 1 TO WS-LINEA-ACTUAL.                                    
           ADD 1 TO CNT-LISTADO-GRABADOS.                               
                                                                        
       2200-F-IMPRIMIR-REGISTRO.                                        
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            2 4 0 0 - F O R M A T E A R - C A M P O S           *
      *----------------------------------------------------------------*
                                                                        
       2400-FORMATEAR-CAMPO.                                            
                                                                        
           MOVE '2400-FORMATEAR-CAMPO'        TO WS-PARRAFO.            
                                                                        
           EVALUATE TRUE                                                
              WHEN CLI-TIP-CUE IS EQUAL TO '01'                         
                 MOVE 'CORRIENTE   ' TO DET-TIPO-CTA                    
                                                                        
              WHEN CLI-TIP-CUE IS EQUAL TO '02'                         
                 MOVE 'AHORRO      ' TO DET-TIPO-CTA                    
                                                                        
              WHEN CLI-TIP-CUE IS EQUAL TO '03'                         
                 MOVE 'PLAZO FIJO  ' TO DET-TIPO-CTA                    

              WHEN OTHER                                                
                 MOVE 'DESCONOCIDO ' TO DET-TIPO-CTA                    
           END-EVALUATE.                                                
                                                                        
           EVALUATE TRUE                                                
              WHEN CLI-SEXO IS EQUAL TO 'M'                             
                 MOVE 'MASCULINO' TO DET-SEXO-CLI                       
                                                                        
              WHEN CLI-SEXO IS EQUAL TO 'F'                             
                 MOVE 'FEMENINO ' TO DET-SEXO-CLI                       
                                                                        
              WHEN OTHER                                                
                 MOVE 'OTRO     ' TO DET-SEXO-CLI                       
           END-EVALUATE.                                                
                                                                        
           MOVE CLI-AAAAMMDD(7:2) TO DET-FECHA-CLI(1:2).                
           MOVE '/'               TO DET-FECHA-CLI(3:1).                
           MOVE CLI-AAAAMMDD(5:2) TO DET-FECHA-CLI(4:2).                
           MOVE '/'               TO DET-FECHA-CLI(6:1).                
           MOVE CLI-AAAAMMDD(1:4) TO DET-FECHA-CLI(7:4).                
                                                                        
           MOVE CLI-SALDO          TO DET-SALDO-CLI.                    
                                                                        
       2400-F-FORMATEAR-CAMPOS.                                         
           EXIT.                                                        

      *----------------------------------------------------------------*
      *            2 6 0 0 - I M P R I M I R - C I E R R E             *
      *----------------------------------------------------------------*
                                                                        
       2600-IMPRIMIR-CIERRE.                                            
                                                                        
           MOVE '2600-IMPRIMIR-CIERRE'        TO WS-PARRAFO.            
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE 'FINAL LISTADO CLIENTES' TO LINEA-IMPRESION.            
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE ALL '-' TO LINEA-IMPRESION.                             
                                                                        
           WRITE LINEA-IMPRESION                                        
              AFTER ADVANCING 1 LINE.                                   
       2600-F-IMPRIMIR-CIERRE.                                          
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            2 9 0 0 - E V A L U A R - G R A B A C I O N         *
      *----------------------------------------------------------------*
                                                                        
       2900-EVALUAR-GRABACION.                                          
                                                                        
           MOVE '2900-EVALUAR-GRABACION'      TO WS-PARRAFO.            
                                                                        
           IF NOT FS-LISTADO-OK                                         
              MOVE CT-WRITE             TO AUX-ERR-ACCION               
              MOVE CT-LISTADO           TO AUX-ERR-NOMBRE               
              MOVE FS-LISTADO           TO AUX-ERR-STATUS               
              MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE              
              MOVE 10                   TO W-N-ERROR                    
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
                                                                        
           END-IF.                                                      
                                                                        
       2900-F-EVALUAR-GRABACION.                                        
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*

       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE CLIENTES                                               
                 LISTADO.                                               
                                                                        
           IF NOT FS-CLIENTES-OK                                        
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-CLIENTES                TO AUX-ERR-NOMBRE         
              MOVE FS-CLIENTES                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-LISTADO-OK                                         
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-LISTADO                 TO AUX-ERR-NOMBRE         
              MOVE FS-LISTADO                 TO AUX-ERR-STATUS         
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
                                                                        
           DISPLAY '**************************************************' 
           DISPLAY '*                 PROGRAMA PGMVSCAB              *' 
           DISPLAY '**************************************************' 
           DISPLAY '**************************************************' 
           DISPLAY '* CANTIDAD TOTAL DE REGISTROS LEIDOS: '             
                                        CNT-CLIENTES-LEIDOS '        *'.
           DISPLAY '**************************************************' 
           DISPLAY '* CANTIDAD TOTAL DE REGISTROS GRABADOS: '           
                                         CNT-LISTADO-GRABADOS '      *'.
           DISPLAY '**************************************************'.
                                                                        
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
           