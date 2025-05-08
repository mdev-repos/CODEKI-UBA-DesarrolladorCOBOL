      ******************************************************************
       IDENTIFICATION DIVISION. 
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGM3CCAB. 
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB 
      *    DATE-WRITTEN.  ABRIL-2025. 
                                                                        
      *----------------------------------------------------------------*
      *              PROGRAMA BASE CON MANEJO DE ARCHIVOS              *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA SUMA SALDO DE LOS CLIENTES CON TIPO DOC = 'DU'  *
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
       01   REG-ENTRADA                                     PIC X(50). 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION. 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES. 
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGM3CCAB'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-ENTRADA                    PIC X(08)  VALUE 'ENTRADA '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES. 
           02 WS-PARRAFO                    PIC X(50). 
           02 WS-SALDO-EDIT                 PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99. 
           02 WS-LEIDOS-EDIT                PIC ZZZZZZZZ9. 
           02 WS-TIPO-DOC-EDIT              PIC ZZZZZZZZ9. 
                                                                        
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
           02 CNT-LEIDOS-ENTRADA            PIC 9(09)  VALUE ZEROS. 
           02 CNT-TIPO-DOC-DU               PIC 9(09)  VALUE ZEROS. 
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  A C U M U L A D O R E S            *
      *----------------------------------------------------------------*
                                                                        
       01 ACM-ACUMULADORES. 
           02 ACM-SALDO                     PIC S9(15)V99  COMP-3. 
                                                                        
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
                                                                        
           COPY CPCLI. 
                                                                        
      ******************************************************************
       PROCEDURE DIVISION. 
      ******************************************************************
                                                                        
           PERFORM 1000-INICIO 
              THRU 1000-F-INICIO. 
                                                                        
                                                                        
           IF FS-ENTRADA-OK 
              PERFORM 2000-PROCESO 
                 THRU 2000-F-PROCESO 
                UNTIL FS-ENTRADA-EOF 
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
                      ACM-ACUMULADORES. 
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO. 
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS 
              THRU 1200-F-ABRIR-ARCHIVOS. 
                                                                        
           PERFORM 1400-LEER-ENTRADA 
              THRU 1400-F-LEER-ENTRADA. 
                                                                        
       1000-F-INICIO. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *                    2 0 0 0 - P R O C E S O                     *
      *----------------------------------------------------------------*
 
       2000-PROCESO. 
                                                                        
           MOVE '2000-PROCESO'                     TO WS-PARRAFO 
                                                                        
           IF CLI-TIP-DOC = 'DU' 
              ADD 1                                TO CNT-TIPO-DOC-DU 
              ADD CLI-SALDO                        TO ACM-SALDO 
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
                                                                        
           READ ENTRADA INTO REG-CLIENTE. 
                                                                        
           EVALUATE TRUE 
               WHEN FS-ENTRADA-OK 
                    ADD 1                     TO CNT-LEIDOS-ENTRADA 
                                                                        
               WHEN FS-ENTRADA-EOF 
                    SET FS-ENTRADA-EOF        TO TRUE 
                                                                        
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
                                                                        
           MOVE ACM-SALDO                     TO WS-SALDO-EDIT. 
           MOVE CNT-LEIDOS-ENTRADA            TO WS-LEIDOS-EDIT. 
           MOVE CNT-TIPO-DOC-DU               TO WS-TIPO-DOC-EDIT. 
                                                                        
           DISPLAY '**************************************************' 
           DISPLAY '*                 PROGRAMA PGM3CCAB              *' 
           DISPLAY '**************************************************' 
           DISPLAY '                                       ' 
           DISPLAY '* CANTIDAD TOTAL DE REGISTROS LEIDOS: ' 
                                                  WS-LEIDOS-EDIT. 
           DISPLAY '* CANTIDAD DE DOCUMENTOS TIPO DU: ' 
                                                  WS-TIPO-DOC-EDIT. 
           DISPLAY '* SALDO ACUMULADO EN ESAS CUENTAS: ' 
                                                  WS-SALDO-EDIT. 
           DISPLAY '                                       ' 
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