      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMIMCAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-MAYO-19.                                 
                                                                        
      *----------------------------------------------------------------*
      * ACTIVIDAD CLASE ASINCRONICA 10 | IMPRESION FBA + CORTE CONTR.  *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA REALIZA UN CORTE DE CONTROL (POR TIPO DE CUENT- *
      *  A | CLIS-TIPO) EN EL ARCHIVO CLIENTES.                        *
      *  LUEGO GENERA UN ARCHIVO DE IMPRESION FBA DE 132 BYTES.        *
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
                                                                        
           SELECT ENTRADA ASSIGN TO ENTRADA                             
                                    FILE STATUS IS FS-ENTRADA.          
                                                                        
           SELECT LISTADO ASSIGN TO LISTADO                             
                                    FILE STATUS IS FS-LISTADO.          
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
       FD   ENTRADA                                                     
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
       01   REG-ENTRADA                                     PIC X(50).  
                                                                        
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
           02  FILLER          PIC X(19) VALUE "REPORTE DE CLIENTES".   
           02  FILLER          PIC X(11) VALUE SPACES.                  
           02  FILLER          PIC X(07) VALUE "FECHA: ".               
           02  WS-FECHA        PIC X(10).                               
           02  FILLER          PIC X(13) VALUE SPACES.                  
           02  FILLER          PIC X(08) VALUE "PAGINA: ".              
           02  WS-PAGINA       PIC Z9.                                  
           02  FILLER          PIC X(52) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  C A B E C E R A               *
      *----------------------------------------------------------------*
                                                                        
       01  WS-CABECERA-COLUMNAS.                                        
           02  FILLER          PIC X(07) VALUE " TIPO |".               
           02  FILLER          PIC X(14) VALUE "  DOCUMENTO  |".        
           02  FILLER          PIC X(11) VALUE " SUCURSAL |".           
           02  FILLER          PIC X(17) VALUE " TIPO DE CUENTA |".     
           02  FILLER          PIC X(16) VALUE " NRO DE CUENTA |".      
           02  FILLER          PIC X(21) VALUE "       IMPORTE       ". 
           02  FILLER          PIC X(02) VALUE " |".                    
           02  FILLER          PIC X(13) VALUE "    FECHA   |".         
           02  FILLER          PIC X(18) VALUE "    LOCALIDAD    |".    
           02  FILLER          PIC X(13) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *           F O R M A T O  D E  S U B C A B E C E R A            *
      *----------------------------------------------------------------*
                                                                        
       01  WS-SUBCABECERA.                                              
           02  FILLER          PIC X(07) VALUE "      |".               
           02  FILLER          PIC X(14) VALUE "             |".        
           02  FILLER          PIC X(11) VALUE "          |".           
           02  FILLER          PIC X(17) VALUE "                |".     
           02  FILLER          PIC X(16) VALUE "               |".      
           02  FILLER          PIC X(21) VALUE "                     ". 
           02  FILLER          PIC X(02) VALUE " |".                    
           02  FILLER          PIC X(13) VALUE "            |".         
           02  FILLER          PIC X(18) VALUE "                 |".    
           02  FILLER          PIC X(13) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *              F O R M A T O  D E  D E T A L L E                 *
      *----------------------------------------------------------------*
                                                                        
       01  WS-DETALLE.                                                  
           02  FILLER          PIC X(02) VALUE SPACES.                  
           02  DET-TIP-DOC     PIC X(02).                               
           02  FILLER          PIC X(04) VALUE "  | ".                  
           02  DET-NRO-DOC     PIC 9(11).                               
           02  FILLER          PIC X(06) VALUE " |    ".                
           02  DET-SUC         PIC 9(02).                               
           02  FILLER          PIC X(07) VALUE "    |  ".               
           02  DET-TIPO-CTA    PIC X(12).                               
           02  FILLER          PIC X(09) VALUE "  |      ".             
           02  DET-NRO-CTA     PIC 9(03).                               
           02  FILLER          PIC X(08) VALUE "      | ".              
           02  DET-IMPORTE     PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.                
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-FECHA       PIC X(10).                               
           02  FILLER          PIC X(03) VALUE " | ".                   
           02  DET-LOCALIDAD   PIC X(15).                               
           02  FILLER          PIC X(02) VALUE " |".                    
           02  FILLER          PIC X(14) VALUE SPACES.                  
                                                                        
      *----------------------------------------------------------------*
      *             F O R M A T O  D E  S U B T O T A L                *
      *----------------------------------------------------------------*
                                                                        
       01  WS-SUBTOTAL.                                                 
           02  FILLER          PIC X(66)   VALUE SPACES.                
           02  FILLER          PIC X(14)   VALUE "SUBTOTAL TIPO ".      
           02  SUBT-TIPO-CTA   PIC X(12).                               
           02  FILLER          PIC X(05)   VALUE SPACES.                
           02  SUBT-IMPORTE    PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.                
           02  FILLER          PIC X(15)   VALUE SPACES.                
                                                                        
      *----------------------------------------------------------------*
      *                 F O R M A T O  D E  T O T A L                  *
      *----------------------------------------------------------------*
                                                                        
       01  WS-TOTAL-GENERAL.                                            
           02  FILLER          PIC X(74)   VALUE SPACES.                
           02  FILLER          PIC X(18)   VALUE "* TOTAL GENERAL * ".  
           02  FILLER          PIC X(05)   VALUE SPACES.                
           02  TOT-IMPORTE     PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.                
           02  FILLER          PIC X(15)   VALUE SPACES.                
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMIMCAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-ENTRADA                    PIC X(08)  VALUE 'ENTRADA '.
           02 CT-LISTADO                    PIC X(08)  VALUE 'LISTADO '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01  WS-VARIABLES.                                                
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-SALDO-EDIT                 PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.   
                                                                        
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
           02 CNT-ENTRADA-LEIDOS            PIC 9(03)  VALUE ZEROS.     
           02 CNT-LISTADO-GRABADOS          PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  A C U M U L A D O R E S            *
      *----------------------------------------------------------------*
                                                                        
       01 ACM-ACUMULADORES.                                             
           02 ACM-SALDO-TIPO                PIC S9(15)V99  COMP-3.      
           02 ACM-SALDO-TOTAL               PIC S9(15)V99  COMP-3.      
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-ACT.                                                 
           02 WS-CTIPO-ACT                  PIC 9(02)  VALUE ZEROS.     
                                                                        
       01 WS-CLAVE-ANT.                                                 
           02 WS-CTIPO-ANT                  PIC 9(02)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-ENTRADA                    PIC X(02).                  
              88 FS-ENTRADA-OK                         VALUE '00'.      
              88 FS-ENTRADA-EOF                        VALUE '10'.      
                                                                        
           02 FS-LISTADO                    PIC X(02).                  
              88 FS-LISTADO-OK                         VALUE '00'.      
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CPCLIENS.                                               
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
                                                                        
           IF FS-ENTRADA-OK                                             
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
                UNTIL FS-ENTRADA-EOF                                    
                                                                        
              PERFORM 2800-TRATAR-ULTIMO                                
                 THRU 2800-F-TRATAR-ULTIMO                              
           END-IF.                                                      
                                                                        
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           INITIALIZE WS-VARIABLES                                      
                      ACM-ACUMULADORES.                                 
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO.            
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS                                  
              THRU 1200-F-ABRIR-ARCHIVOS.                               
                                                                        
           PERFORM 1400-LEER-ENTRADA                                    
              THRU 1400-F-LEER-ENTRADA.                                 
                                                                        
           PERFORM 1500-OBTENER-FECHA                                   
              THRU 1500-F-OBTENER-FECHA.                                
                                                                        
           PERFORM 1600-IMPRIMIR-CABECERA                               
              THRU 1600-F-IMPRIMIR-CABECERA.                            

           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT.          
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           EVALUATE TRUE                                                
              WHEN WS-CLAVE-ACT = WS-CLAVE-ANT                          
                   PERFORM 2200-ACUMULAR-SALDO                          
                      THRU 2200-F-ACUMULAR-SALDO                        
                                                                        
                   PERFORM 2400-IMPRIMIR-REGISTRO                       
                      THRU 2400-F-IMPRIMIR-REGISTRO                     
                                                                        
               WHEN WS-CLAVE-ACT NOT EQUAL WS-CLAVE-ANT                 
                   PERFORM 2600-IMPRIMIR-SUBTOTAL                       
                      THRU 2600-F-IMPRIMIR-SUBTOTAL                     
                                                                        
                   PERFORM 2200-ACUMULAR-SALDO                          
                      THRU 2200-F-ACUMULAR-SALDO                        
                                                                        
                   PERFORM 2400-IMPRIMIR-REGISTRO                       
                      THRU 2400-F-IMPRIMIR-REGISTRO                     
           END-EVALUATE.                                                
                                                                        
           PERFORM 1400-LEER-ENTRADA                                    
              THRU 1400-F-LEER-ENTRADA.                                 
                                                                        
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
                                                                        
           OPEN INPUT ENTRADA                                           
                OUTPUT LISTADO.                                         
                                                                        
           IF NOT FS-ENTRADA-OK                                         
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE         
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS         
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
                                                                        
       1400-LEER-ENTRADA.                                               
                                                                        
           MOVE '1400-LEER-ENTRADA'           TO WS-PARRAFO.            
                                                                        
           READ ENTRADA INTO REG-CLIENTES.                              
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-ENTRADA-OK                                       
                    MOVE CLIS-TIPO            TO WS-CTIPO-ACT           
                    ADD 1 TO CNT-ENTRADA-LEIDOS                         
                                                                        
               WHEN FS-ENTRADA-EOF                                      
                    SET FS-ENTRADA-EOF        TO TRUE                   
                    MOVE HIGH-VALUES          TO WS-CLAVE-ACT           
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-ENTRADA           TO AUX-ERR-NOMBRE         
                    MOVE FS-ENTRADA           TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1400-F-LEER-ENTRADA.                                             
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
      *              2 2 0 0 - A C U M U L A R - S A L D O             *
      *----------------------------------------------------------------*
                                                                       
       2200-ACUMULAR-SALDO.                                             
                                                                       
           MOVE '2200-ACUMULAR-SALDO'         TO WS-PARRAFO.            
                                                                       
           COMPUTE ACM-SALDO-TIPO = ACM-SALDO-TIPO + CLIS-IMPORTE.      
                                                                        
           COMPUTE ACM-SALDO-TOTAL = ACM-SALDO-TOTAL + CLIS-IMPORTE.    
                                                                        
       2200-F-ACUMULAR-SALDO.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *          2 4 0 0 - I M P R I M I R - R E G I S T R O           *
      *----------------------------------------------------------------*
                                                                        
       2400-IMPRIMIR-REGISTRO.                                          
                                                                        
           MOVE '2400-IMPRIMIR-REGISTRO'      TO WS-PARRAFO.            
                                                                        
           IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                          
              PERFORM 1600-IMPRIMIR-CABECERA                            
                 THRU 1600-F-IMPRIMIR-CABECERA                          
           END-IF.                                                      
                                                                        
           MOVE CLIS-TIP-DOC      TO DET-TIP-DOC.                       
           MOVE CLIS-NRO-DOC      TO DET-NRO-DOC.                       
           MOVE CLIS-SUC          TO DET-SUC.                           
           MOVE CLIS-NRO          TO DET-NRO-CTA.                       
           MOVE CLIS-LOCALIDAD    TO DET-LOCALIDAD.                     
                                                                        
           PERFORM 2450-FORMATEAR-CAMPOS                                
              THRU 2450-F-FORMATEAR-CAMPOS.                             
                                                                        
           WRITE LINEA-IMPRESION FROM WS-DETALLE                        
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           PERFORM 2900-EVALUAR-GRABACION                               
              THRU 2900-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 1 TO WS-LINEA-ACTUAL.                                    
           ADD 1 TO CNT-LISTADO-GRABADOS.                               

       2400-F-IMPRIMIR-REGISTRO.                                        
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *            2 4 5 0 - F O R M A T E A R - C A M P O S           *
      *----------------------------------------------------------------*
                                                                        
       2450-FORMATEAR-CAMPOS.                                           
                                                                        
           MOVE '2450-FORMATEAR-CAMPOS'       TO WS-PARRAFO.            
                                                                        
           EVALUATE TRUE                                                
              WHEN CLIS-TIPO IS EQUAL TO '01'                           
                 MOVE 'CORRIENTE   ' TO DET-TIPO-CTA                    
                                                                        
              WHEN CLIS-TIPO IS EQUAL TO '02'                           
                 MOVE 'AHORRO      ' TO DET-TIPO-CTA                    
                                                                        
              WHEN CLIS-TIPO IS EQUAL TO '03'                           
                 MOVE 'PLAZO FIJO  ' TO DET-TIPO-CTA                    
                                                                        
              WHEN OTHER                                                
                 MOVE 'DESCONOCIDO ' TO DET-TIPO-CTA                    
                                                                        
           END-EVALUATE.                                                
                                                                        
           MOVE CLIS-AAAAMMDD(7:2) TO DET-FECHA(1:2).                   
           MOVE '/'                TO DET-FECHA(3:1).                   
           MOVE CLIS-AAAAMMDD(5:2) TO DET-FECHA(4:2).                   
           MOVE '/'                TO DET-FECHA(6:1).                   
           MOVE CLIS-AAAAMMDD(1:4) TO DET-FECHA(7:4).                   
                                                                        
           MOVE CLIS-IMPORTE       TO DET-IMPORTE.                      
                                                                        
       2450-F-FORMATEAR-CAMPOS.                                         
           EXIT.                                                        

      *----------------------------------------------------------------*
      *           2 6 0 0 - I M P R I M I R - S U B T O T A L          *
      *----------------------------------------------------------------*
                                                                        
       2600-IMPRIMIR-SUBTOTAL.                                          
                                                                        
           MOVE '2600-IMPRIMIR-SUBTOTAL'      TO WS-PARRAFO.            
                                                                        
           MOVE SPACES TO LINEA-IMPRESION.                              
           WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.                
                                                                        
           MOVE ALL "-" TO LINEA-IMPRESION.                             
           WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.                
                                                                        
           MOVE DET-TIPO-CTA         TO SUBT-TIPO-CTA.                  
           MOVE ACM-SALDO-TIPO       TO SUBT-IMPORTE.                   
           WRITE LINEA-IMPRESION FROM WS-SUBTOTAL                       
              AFTER ADVANCING 1 LINE.                                   
                                                                        
           MOVE ALL "-" TO LINEA-IMPRESION.                             
           WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.                
                                                                        
           MOVE SPACES TO LINEA-IMPRESION.                              
           WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.                
                                                                        
           PERFORM 2900-EVALUAR-GRABACION                               
              THRU 2900-F-EVALUAR-GRABACION.                            
                                                                        
           ADD 5 TO WS-LINEA-ACTUAL.                                    
                                                                        
           MOVE ZEROS TO ACM-SALDO-TIPO.                                
                                                                        
           MOVE WS-CLAVE-ACT TO WS-CLAVE-ANT.                           
                                                                        
       2600-F-IMPRIMIR-SUBTOTAL.                                        
           EXIT.                                                        

      *----------------------------------------------------------------*
      *             2 7 0 0 - I M P R I M I R - T O T A L              *
      *----------------------------------------------------------------*
                                                                        
       2700-IMPRIMIR-TOTAL.                                             
                                                                        
           MOVE '2700-IMPRIMIR-TOTAL'         TO WS-PARRAFO.            
                                                                        
            MOVE ALL "-" TO LINEA-IMPRESION.                            
               WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.            
                                                                        
            MOVE ACM-SALDO-TOTAL    TO TOT-IMPORTE.                     
               WRITE LINEA-IMPRESION FROM WS-TOTAL-GENERAL              
                  AFTER ADVANCING 1 LINE.                               
                                                                        
            MOVE ALL "-" TO LINEA-IMPRESION.                            
               WRITE LINEA-IMPRESION AFTER ADVANCING 1 LINE.            
                                                                        
            PERFORM 2900-EVALUAR-GRABACION                              
               THRU 2900-F-EVALUAR-GRABACION.                           
                                                                        
            ADD 3 TO WS-LINEA-ACTUAL.                                   
                                                                        
       2700-F-IMPRIMIR-TOTAL.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 8 0 0 - T R A T A R - U L T I M O              *
      *----------------------------------------------------------------*
                                                                        
       2800-TRATAR-ULTIMO.                                              
                                                                        
           MOVE '2800-TRATAR-ULTIMO'          TO WS-PARRAFO.            
                                                                        
           PERFORM 2600-IMPRIMIR-SUBTOTAL                               
              THRU 2600-F-IMPRIMIR-SUBTOTAL.                            

           PERFORM 2700-IMPRIMIR-TOTAL                                  
              THRU 2700-F-IMPRIMIR-TOTAL.                               
                                                                        
           MOVE ZEROS TO ACM-SALDO-TIPO.                                
           MOVE ZEROS TO ACM-SALDO-TOTAL.                               
                                                                        
       2800-F-TRATAR-ULTIMO.                                            
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
                                                                        
           CLOSE ENTRADA                                                
                 LISTADO.                                               
                                                                        
           IF NOT FS-ENTRADA-OK                                         
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE         
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS         
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
           DISPLAY '*                 PROGRAMA PGMIMCAB              *' 
           DISPLAY '**************************************************' 
           DISPLAY '**************************************************' 
           DISPLAY '* CANTIDAD TOTAL DE REGISTROS LEIDOS: '             
                                         CNT-ENTRADA-LEIDOS '        *'.
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