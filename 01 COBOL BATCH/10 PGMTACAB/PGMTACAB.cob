      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMTACAB.                                         
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-MAYO-09                                  
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD CLASE ASINCRONICA 9 | PRIMER PROGRAMA CON VECTORES *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA DECLARA UN VECTOR DE 10 POSICIONES Y GUARDA EN  *
      *  CADA UNA DE ELLAS UN PRODUCTO (LEIDO DEL ARCHIVO 'PRODUCT1'.  *
      *  POSTERIORMENTE, ACTUALIZA EL PRECIO DE CADA UNO DE ESOS PRO-  *
      *  -DUCTOS MEDIANTE LA LECTURA DEL ARCHIVO 'PRECIO'.             *
      *                                                                *
      *  EN CASO DE NO COINCIDIR EL COD-PROD DEL REG DEL ARCHIVO PRE-  *
      *  -CIO, EL PROGRAMA DISPLAYARA 'PRODUCTO NO ENCOTNTRADO + COD-  *
      *  -PRECIO'.                                                     *
      *                                                                *
      *  EN CASO DE EOF DEL ARCHIVO PRECIO SIN COMPLETAR LA ACTUALIZ-  *
      *  -ACION DE LOS ITEMS DEL VECTOR DISPLAYARA EL MISMO MENSAJE.   *
      *                                                                *
      *  AL FINAL DEL PGM, SE RECORRE EL VECTOR GENERADO MOSTRANDO:    *
      *     - COD DE PRODUCTO                                          *
      *     - DENOMINACION                                             *
      *     - PRECIO                                                   *
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
                                                                        
           SELECT PRODUCTO ASSIGN TO PRODUCTO                           
                                    FILE STATUS IS FS-PRODUCTO.         
                                                                        
           SELECT PRECIO ASSIGN TO PRECIO                               
                                    FILE STATUS IS FS-PRECIO.           
                                                                        
       I-O-CONTROL.                                                     
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
       FD   PRODUCTO                                                    
            RECORDING MODE IS F.                                        
       01   REG-PRODUCTO                                    PIC X(32).  
                                                                        
       FD   PRECIO                                                      
            RECORDING MODE IS F.                                        
       01   REG-PRECIO                                      PIC X(07).  
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMTACAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-SIZE                       PIC X(08)  VALUE 'SIZE    '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-PRODUCTO                   PIC X(08)  VALUE 'PRODUCTO'.
           02 CT-PRECIO                     PIC X(08)  VALUE 'PRECIO  '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
           02 WS-PARRAFO                    PIC X(50).                  
           02 WS-CNT-MASC                   PIC ZZ9.                    
           02 WS-PRECIO-MASC                PIC $ZZ9,99.                
           02 WS-I                          PIC 9(02)  VALUE 1.         
           02 WS-TAMANIO-VECTOR             PIC 9(02)  VALUE 10.        
                                                                        
       01 WS-T-PRODUCTOS.                                               
          03 T-ITEMS                        OCCURS 10 TIMES.            
            05 T-COD-PROD                   PIC 9(02).                  
            05 T-DENOMINACION               PIC X(22).                  
            05 T-PRECIO                     PIC 9(03)V99.               
                                                                        
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
      *               A R E A  D E  C O N T A D O R E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CNT-CONTADORES.                                               
           02 CNT-PROD-LEIDOS               PIC 9(03)  VALUE ZEROS.     
           02 CNT-PREC-LEIDOS               PIC 9(03)  VALUE ZEROS.     
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS.                                               
           02 FS-PRODUCTO                   PIC X(02).                  
              88 FS-PRODUCTO-OK                        VALUE '00'.      
              88 FS-PRODUCTO-EOF                       VALUE '10'.      
                                                                        
           02 FS-PRECIO                     PIC X(02).                  
              88 FS-PRECIO-OK                          VALUE '00'.      
              88 FS-PRECIO-EOF                         VALUE '10'.      
                                                                        
      *----------------------------------------------------------------*
      *       A R E A  D E  F O R M A T O  D E  R E G I S T R O S      *
      *----------------------------------------------------------------*
                                                                        
      *  ARCHIVO PRODUCTO.                                              
                                                                        
       01  WS-REG-PRODUCTO.                                             
           03  PRO-COD-PRODUCTO     PIC 9(2)    VALUE ZEROS.            
           03  PRO-DENOMINACION     PIC X(22)   VALUE SPACES.           
           03  FILLER               PIC X(08)   VALUE SPACES.           
                                                                        
      *  ARCHIVO PRECIO.                                                
                                                                        
       01  WS-REG-PRECIO.                                               
           03  PRE-COD-PRODUCTO     PIC 9(2)    VALUE ZEROS.            
           03  PRE-PRECIO           PIC 9(3)V99 VALUE ZEROS.            
                                                                        
      ******************************************************************
       PROCEDURE DIVISION.                                              
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           IF FS-PRODUCTO-OK                                            
              PERFORM 2000-PROCESO                                      
                 THRU 2000-F-PROCESO                                    
           END-IF.                                                      
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO.            
                                                                        
           INITIALIZE WS-VARIABLES                                      
                      CNT-CONTADORES                                    
                                                                        
      *    LIMPIAR TABLA                                                
           PERFORM WS-TAMANIO-VECTOR TIMES                              
               INITIALIZE T-ITEMS(WS-I)                                 
               ADD 1 TO WS-I                                            
           END-PERFORM.                                                 
                                                                        
           MOVE 1 TO WS-I.                                              
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS                                  
              THRU 1200-F-ABRIR-ARCHIVOS.                               
                                                                        
           PERFORM 1400-LEER-PRODUCTO                                   
              THRU 1400-F-LEER-PRODUCTO.                                
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO        
                                                                        
           PERFORM 2200-CARGAR-VECTOR                                   
              THRU 2200-F-CARGAR-VECTOR.                                
                                                                        
           PERFORM 1600-LEER-PRECIO                                     
              THRU 1600-F-LEER-PRECIO.                                  
                                                                        
           PERFORM 2400-ACTUALIZAR-PRECIOS                              
              THRU 2400-F-ACTUALIZAR-PRECIOS                            
             UNTIL FS-PRECIO-EOF.                                       
                                                                        
           MOVE 1 TO WS-I.                                              
                                                                        
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
                                                                        
           OPEN INPUT   PRODUCTO                                        
                        PRECIO.                                         
                                                                        
           IF NOT FS-PRODUCTO-OK                                        
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-PRODUCTO                TO AUX-ERR-NOMBRE         
              MOVE FS-PRODUCTO                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-PRECIO-OK                                          
              MOVE CT-OPEN                    TO AUX-ERR-ACCION         
              MOVE CT-PRECIO                  TO AUX-ERR-NOMBRE         
              MOVE FS-PRECIO                  TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       1200-F-ABRIR-ARCHIVOS.                                           
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 4 0 0 - L E E R - P R O D U C T O              *
      *----------------------------------------------------------------*
                                                                        
       1400-LEER-PRODUCTO.                                              
                                                                        
           MOVE '1400-LEER-PRODUCTO'          TO WS-PARRAFO.            
                                                                        
           READ PRODUCTO INTO WS-REG-PRODUCTO.                          
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-PRODUCTO-OK                                      
                    ADD 1 TO CNT-PROD-LEIDOS                            
                                                                        
               WHEN FS-PRODUCTO-EOF                                     
                    SET FS-PRODUCTO-EOF       TO TRUE                   
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-PRODUCTO          TO AUX-ERR-NOMBRE         
                    MOVE FS-PRODUCTO          TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1400-F-LEER-PRODUCTO.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               1 6 0 0 - L E E R - P R E C I O                  *
      *----------------------------------------------------------------*
                                                                        
       1600-LEER-PRECIO.                                                
                                                                        
           MOVE '1600-LEER-PRECIO'            TO WS-PARRAFO.            
                                                                        
           READ PRECIO INTO WS-REG-PRECIO.                              
                                                                        
           EVALUATE TRUE                                                
               WHEN FS-PRECIO-OK                                        
                    ADD 1                     TO CNT-PREC-LEIDOS        
                                                                        
               WHEN FS-PRECIO-EOF                                       
                    SET FS-PRECIO-EOF         TO TRUE                   
                                                                        
               WHEN OTHER                                               
                    MOVE CT-READ              TO AUX-ERR-ACCION         
                    MOVE CT-PRECIO            TO AUX-ERR-NOMBRE         
                    MOVE FS-PRECIO            TO AUX-ERR-STATUS         
                    MOVE WS-PARRAFO           TO AUX-ERR-MENSAJE        
                    MOVE 10                   TO W-N-ERROR              
                                                                        
                    PERFORM 9000-SALIDA-ERRORES                         
                       THRU 9000-F-SALIDA-ERRORES                       
                                                                        
           END-EVALUATE.                                                
                                                                        
       1600-F-LEER-PRECIO.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 2 0 0 - C A R G A R - V E C T O R                *
      *----------------------------------------------------------------*
                                                                        
       2200-CARGAR-VECTOR.                                              
                                                                        
           MOVE '2200-CARGAR-VECTOR'          TO WS-PARRAFO.            
                                                                        
      *    PERFORM WS-TAMANIO-VECTOR TIMES                              
           PERFORM 10 TIMES                                             
                                                                        
              MOVE PRO-COD-PRODUCTO     TO T-COD-PROD(WS-I)             
              MOVE PRO-DENOMINACION     TO T-DENOMINACION(WS-I)         
                                                                        
              ADD 1 TO WS-I                                             
                                                                        
              PERFORM 1400-LEER-PRODUCTO                                
                 THRU 1400-F-LEER-PRODUCTO                              
                                                                        
           END-PERFORM.                                                 
                                                                        
           MOVE 1 TO WS-I.                                              
                                                                        
           IF NOT FS-PRODUCTO-EOF                                       
              MOVE CT-SIZE                    TO AUX-ERR-ACCION         
              MOVE CT-PRODUCTO                TO AUX-ERR-NOMBRE         
              MOVE FS-PRODUCTO                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
       2200-F-CARGAR-VECTOR.                                            
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *          2 4 0 0 - A C T U A L I Z A R - P R E C I O S         *
      *----------------------------------------------------------------*
                                                                        
       2400-ACTUALIZAR-PRECIOS.                                         
                                                                        
           MOVE '2400-ACTUALIZAR-PRECIOS'     TO WS-PARRAFO.            
                                                                        
           EVALUATE TRUE                                                
              WHEN PRE-COD-PRODUCTO = T-COD-PROD(WS-I)                  
                 MOVE PRE-PRECIO TO T-PRECIO(WS-I)                      
                                                                        
                 IF WS-I < 10                                           
                    ADD 1 TO WS-I                                       
                 END-IF                                                 
                                                                        
                 PERFORM 1600-LEER-PRECIO                               
                    THRU 1600-F-LEER-PRECIO                             
                                                                        
              WHEN PRE-COD-PRODUCTO < T-COD-PROD(WS-I)                  
                 DISPLAY "************************************"         
                 DISPLAY "* PRODCUTO NO ENCONTRADO | COD. "             
                                      PRE-COD-PRODUCTO " *"             
                 DISPLAY "************************************"         
                                                                        
                 PERFORM 1600-LEER-PRECIO                               
                    THRU 1600-F-LEER-PRECIO                             
                                                                        
              WHEN PRE-COD-PRODUCTO > T-COD-PROD(WS-I)                  
                 IF WS-I < 10                                           
                    ADD 1 TO WS-I                                       
                 ELSE                                                   
                    PERFORM 1600-LEER-PRECIO                            
                       THRU 1600-F-LEER-PRECIO                          
                 END-IF                                                 
           END-EVALUATE.                                                
                                                                        
       2400-F-ACTUALIZAR-PRECIOS.                                       
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS.                                            
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO.            
                                                                        
           CLOSE PRODUCTO                                               
                 PRECIO.                                                
                                                                        
           IF NOT FS-PRODUCTO-OK                                        
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-PRODUCTO                TO AUX-ERR-NOMBRE         
              MOVE FS-PRODUCTO                TO AUX-ERR-STATUS         
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE        
              MOVE 10                         TO W-N-ERROR              
                                                                        
              PERFORM 9000-SALIDA-ERRORES                               
                 THRU 9000-F-SALIDA-ERRORES                             
           END-IF.                                                      
                                                                        
           IF NOT FS-PRECIO-OK                                          
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION         
              MOVE CT-PRECIO                  TO AUX-ERR-NOMBRE         
              MOVE FS-PRECIO                  TO AUX-ERR-STATUS         
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
                                                                        
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                PROGRAMA PGMTACAB               *'.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-PROD-LEIDOS TO WS-CNT-MASC.                         
           DISPLAY '*  REGISTROS DE PRODUCTOS LEIDOS: '                 
                                     WS-CNT-MASC '            *'.       
           DISPLAY '*                                                *'.
                                                                        
           MOVE CNT-PREC-LEIDOS TO WS-CNT-MASC.                         
           DISPLAY '*  REGISTROS DE PRECIOS LEIDOS:   '                 
                                     WS-CNT-MASC '            *'.       
           DISPLAY '*                                                *'.
                                                                        
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '*              VECTOR DE PRODUCTOS               *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.
                                                                        
      *    PERFORM WS-TAMANIO-VECTOR TIMES                              
           PERFORM 10 TIMES                                             
                                                                        
           DISPLAY '*                                                *' 
           DISPLAY '* PRODUCTO NRO: ' WS-I                              
                                     '                               *' 
           DISPLAY '* CODIGO:       ' T-COD-PROD(WS-I)                  
                                     '                               *' 
           DISPLAY '* DENOMINACION: ' T-DENOMINACION(WS-I)              
                                                         '           *' 
           MOVE T-PRECIO(WS-I) TO WS-PRECIO-MASC                        
           DISPLAY '* PRECIO:       ' WS-PRECIO-MASC                    
                                          '                          *' 
           DISPLAY '*                                                *' 
           DISPLAY '**************************************************' 
                                                                        
           ADD 1 TO WS-I                                                
                                                                        
           END-PERFORM.                                                 
                                                                        
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