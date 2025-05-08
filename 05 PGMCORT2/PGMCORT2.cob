      ******************************************************************
       IDENTIFICATION DIVISION. 
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMCORT2. 
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB 
      *    DATE-WRITTEN.  ABRIL-2025 
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD ASINCRONICA SEMANA 7 | DOS CORTES DE CONTROL       *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA REALIZA DOS CORTES DE CONTROL (POR TIPO Y NRO   *
      *  DE DOCUMENTO) SOBRE EL ARCHIVO SUCURSAL. CONTARA LA CANTIDAD  *
      *  DE DNI LEIDOS POR TIPO Y POR SEXO PARA MOSTRARLOS DE MANERA   *
      *  ESTABLECIDA.                                                  *
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
                                                                        
       I-O-CONTROL. 
                                                                        
      ******************************************************************
       DATA DIVISION. 
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION. 
      *----------------------------------------------------------------*
                                                                        
       FD   ENTRADA 
            RECORDING MODE IS F. 
       01   REG-ENTRADA                                     PIC X(93). 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION. 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES. 
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMCORT2'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-ENTRADA                    PIC X(08)  VALUE 'ENTRADA '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES. 
           02 WS-PARRAFO                    PIC X(50). 
           02 WS-TITULO                     PIC X(02). 
                                                                        
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
           02 CNT-REGISTROS-PROCESADOS      PIC 9(03)  VALUE ZEROS. 
           02 CNT-TIPO-LEIDOS               PIC 9(03)  VALUE ZEROS. 
           02 CNT-SEXO-LEIDOS               PIC 9(03)  VALUE ZEROS. 
                                                                                                                  
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-ACT. 
           02 WS-TIPO-ACT                   PIC X(02)  VALUE SPACES. 
           02 WS-SEXO-ACT                   PIC X(01)  VALUE SPACES. 
                                                                        
       01 WS-CLAVE-ANT. 
           02 WS-TIPO-ANT                   PIC X(02)  VALUE SPACES. 
           02 WS-SEXO-ANT                   PIC X(01)  VALUE SPACES. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS. 
           02 FS-ENTRADA                    PIC X(02). 
              88 FS-ENTRADA-OK                         VALUE '00'. 
              88 FS-ENTRADA-EOF                        VALUE '10'. 
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CLICOB. 
                                                                        
      ******************************************************************
       PROCEDURE DIVISION. 
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO 
              THRU 1000-F-INICIO. 
                                                                        
                                                                        
           IF FS-ENTRADA-OK 
              PERFORM 2000-PROCESO 
                 THRU 2000-F-PROCESO 
                UNTIL FS-ENTRADA-EOF 
                                                                        
              PERFORM 2600-TRATAR-ULTIMO 
                 THRU 2600-F-TRATAR-ULTIMO 
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
                                                                        
           PERFORM 1400-LEER-ENTRADA 
              THRU 1400-F-LEER-ENTRADA. 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT. 
                                                                        
       1000-F-INICIO. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO. 
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO 
                                                                        
           IF WS-TIPO-ACT IS EQUAL 'DU' 
                       OR WS-TIPO-ACT IS EQUAL 'PA' 
                       OR WS-TIPO-ACT IS EQUAL 'PE' 
                       OR WS-TIPO-ACT IS EQUAL 'CI' 
                                                                        
             ADD 1 TO CNT-REGISTROS-PROCESADOS 
                                                                        
             EVALUATE TRUE 
                 WHEN WS-CLAVE-ACT = WS-CLAVE-ANT 
                     PERFORM 2200-CONTAR-LEIDOS 
                        THRU 2200-F-CONTAR-LEIDOS 
                                                                        
                 WHEN WS-SEXO-ACT NOT EQUAL WS-SEXO-ANT 
                                    AND WS-TIPO-ACT IS EQUAL WS-TIPO-ANT
                                                                        
                         IF WS-TIPO-ACT NOT EQUAL WS-TITULO 
                           MOVE WS-TIPO-ACT             TO WS-TITULO 
                           DISPLAY "* TIPO DE DOCUMENTO: " WS-TITULO 
                         END-IF 
                                                                        
                         PERFORM 2300-CORTE-SEXO 
                            THRU 2300-F-CORTE-SEXO 
                                                                        
                         PERFORM 2200-CONTAR-LEIDOS 
                            THRU 2200-F-CONTAR-LEIDOS 
                                                                        
                 WHEN WS-TIPO-ACT NOT EQUAL WS-TIPO-ANT 
                         PERFORM 2300-CORTE-SEXO 
                            THRU 2300-F-CORTE-SEXO 
                                                                        
                         PERFORM 2400-MOSTRAR-TIPO 
                            THRU 2400-F-MOSTRAR-TIPO 
                                                                        
                         PERFORM 2200-CONTAR-LEIDOS 
                            THRU 2200-F-CONTAR-LEIDOS 
                                                                        
             END-EVALUATE 
           END-IF. 
                                                                        
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
                                                                        
           IF NOT FS-ENTRADA-OK 
              MOVE CT-OPEN                    TO AUX-ERR-ACCION 
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE 
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS 
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
                                                                        
           READ ENTRADA INTO WS-REG-CLICOB. 
                                                                        
           EVALUATE TRUE 
               WHEN FS-ENTRADA-OK 
                    ADD 1                     TO CNT-REGISTROS-LEIDOS 
                    MOVE WS-SUC-TIP-DOC       TO WS-TIPO-ACT 
                    MOVE WS-SUC-SEXO          TO WS-SEXO-ACT 
                                                                        
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
      *              2 2 0 0 - C O N T A R - L E I D O S               *
      *----------------------------------------------------------------*
                                                                        
       2200-CONTAR-LEIDOS. 
                                                                        
           MOVE '2200-CONTAR-LEIDOS'          TO WS-PARRAFO. 
                                                                        
           ADD 1 TO CNT-TIPO-LEIDOS. 
                                                                        
           ADD 1 TO CNT-SEXO-LEIDOS. 
                                                                        
       2200-F-CONTAR-LEIDOS. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *                 2 3 0 0 - C O R T E - S E X O                  *
      *----------------------------------------------------------------*
                                                                        
       2300-CORTE-SEXO. 
                                                                        
           MOVE '2300-CORTE-SEXO'           TO WS-PARRAFO. 
                                                                        
           DISPLAY '  - SEXO ' WS-SEXO-ANT ': ' CNT-SEXO-LEIDOS. 
                                                                        
           MOVE 0                           TO CNT-SEXO-LEIDOS. 
                                                                        
           MOVE WS-CLAVE-ACT                TO WS-CLAVE-ANT. 
                                                                        
       2300-F-CORTE-SEXO. 
           EXIT. 
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 4 0 0 - M O S T R A R - T I P O                  *
      *----------------------------------------------------------------*
                                                                        
       2400-MOSTRAR-TIPO. 
                                                                        
           MOVE '2400-MOSTRAR-TIPO'           TO WS-PARRAFO. 
                                                                        
           DISPLAY '   | TOTAL CON TIPO DOCUMENTO ' WS-TITULO ': ' 
                                                   CNT-TIPO-LEIDOS. 
                                                                        
           MOVE 0                             TO CNT-TIPO-LEIDOS. 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT. 
                                                                        
       2400-F-MOSTRAR-TIPO. 
           EXIT. 
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 6 0 0 - T R A T A R - U L T I M O              *
      *----------------------------------------------------------------*
                                                                        
       2600-TRATAR-ULTIMO. 
                                                                        
           MOVE '2600-TRATAR-ULTIMO'          TO WS-PARRAFO. 
                                                                        
           PERFORM 2300-CORTE-SEXO 
              THRU 2300-F-CORTE-SEXO. 
                                                                        
           PERFORM 2400-MOSTRAR-TIPO 
              THRU 2400-F-MOSTRAR-TIPO. 
                                                                        
       2600-F-TRATAR-ULTIMO. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS. 
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO. 
                                                                        
           CLOSE ENTRADA. 
                                                                        
           IF NOT FS-ENTRADA-OK 
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION 
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE 
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS 
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
                                                                        
           DISPLAY ' ' 
           DISPLAY '**************************************************' 
           DISPLAY '*                                                *' 
           DISPLAY '* CANTIDAD DE REGISTROS LEIDOS: ' 
                            CNT-REGISTROS-LEIDOS '              *'. 
           DISPLAY '*                                                *' 
           DISPLAY '* CANTIDAD DE REGISTROS PROCESADOS: ' 
                            CNT-REGISTROS-PROCESADOS '          *'. 
           DISPLAY '*                                                *' 
           DISPLAY '**************************************************' 
           DISPLAY '*                PROGRAMA PGMCORT2               *' 
           DISPLAY '**************************************************' 
           DISPLAY ' '. 
                                                                        
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