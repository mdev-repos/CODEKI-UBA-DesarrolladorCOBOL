      ******************************************************************
       IDENTIFICATION DIVISION. 
      ******************************************************************
                                                                        
       PROGRAM-ID.    CORTEC12. 
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB 
      *    DATE-WRITTEN.  ABRIL-2025 
                                                                        
      *----------------------------------------------------------------*
      *   CLASE 12 | PROGRAMA CON DOS CORTES DE CONTROL | SUCURSAL     *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA REALIZA DOS CORTES DE CONTROL (NRO Y TIPO)      *
      *    MUESTRA EL SUBTOTAL POR TIPO DE SUCURSAL, EL SUBTOTAL POR   *
      *    SUCURSAL Y EL TOTAL GENERAL DE TODAS LAS SUCURSALES         *
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
       01   REG-ENTRADA                                     PIC X(20). 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION. 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES. 
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'CORTEC12'.
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
           02 WS-MENOR-EDIT                 PIC -$ZZ.ZZZ.ZZ9,99. 
                                                                        
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
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  A C U M U L A D O R E S            *
      *----------------------------------------------------------------*
                                                                        
       01 ACM-ACUMULADORES. 
           02 ACM-SALDO-SUC                 PIC S9(15)V99  COMP-3. 
           02 ACM-SALDO-TIPO                PIC S9(15)V99  COMP-3. 
           02 ACM-SALDO-TOTAL               PIC S9(15)V99  COMP-3. 
                                                                        
      *----------------------------------------------------------------*
      *                   C L A V E  D E  A P A R E O                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-CLAVE-ACT. 
           02 WS-SUC-ACT                    PIC 9(02)  VALUE ZEROS. 
           02 WS-TIP-ACT                    PIC X(02)  VALUE ZEROS. 
                                                                        
       01 WS-CLAVE-ANT. 
           02 WS-SUC-ANT                    PIC 9(02)  VALUE ZEROS. 
           02 WS-TIP-ANT                    PIC X(02)  VALUE ZEROS. 
                                                                        
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
                                                                        
           COPY CORTE. 
                                                                        
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
                      ACM-ACUMULADORES. 
                                                                        
           MOVE '1000-INICIO'                 TO WS-PARRAFO. 
                                                                        
           PERFORM 1200-ABRIR-ARCHIVOS 
              THRU 1200-F-ABRIR-ARCHIVOS. 
                                                                        
           PERFORM 1400-LEER-ENTRADA 
              THRU 1400-F-LEER-ENTRADA. 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT. 
                                                                        
           DISPLAY 'SUCURSAL ' WS-SUC-ACT. 
                                                                        
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
                                                                        
               WHEN WS-TIP-ACT NOT EQUAL WS-TIP-ANT 
                                  AND WS-SUC-ACT IS EQUAL WS-SUC-ANT 
                       PERFORM 2300-CORTE-TIPO 
                          THRU 2300-F-CORTE-TIPO 
                                                                        
                       PERFORM 2200-ACUMULAR-SALDO 
                          THRU 2200-F-ACUMULAR-SALDO 
                                                                        
               WHEN WS-SUC-ACT NOT EQUAL WS-SUC-ANT 
                       PERFORM 2300-CORTE-TIPO 
                          THRU 2300-F-CORTE-TIPO 
                                                                        
                       PERFORM 2400-MOSTRAR-SUCURSAL 
                          THRU 2400-F-MOSTRAR-SUCURSAL 
                                                                        
                       PERFORM 2200-ACUMULAR-SALDO 
                          THRU 2200-F-ACUMULAR-SALDO 
                                                                        
                       DISPLAY 'SUCURSAL ' WS-SUC-ACT 
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
                                                                        
           READ ENTRADA INTO WS-REG-SUCURSAL. 
                                                                        
           EVALUATE TRUE 
               WHEN FS-ENTRADA-OK 
                    ADD 1                     TO CNT-LEIDOS-ENTRADA 
                    MOVE WS-SUC-NRO           TO WS-SUC-ACT 
                    MOVE WS-SUC-TIPN          TO WS-TIP-ACT 
                                                                        
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
      *              2 2 0 0 - A C U M U L A R - S A L D O             *
      *----------------------------------------------------------------*
                                                                        
       2200-ACUMULAR-SALDO. 
                                                                        
           MOVE '2200-ACUMULAR-SALDO'         TO WS-PARRAFO. 
                                                                        
           COMPUTE ACM-SALDO-TIPO = ACM-SALDO-TIPO + WS-SUC-IMPORTE. 
                                                                        
           COMPUTE ACM-SALDO-SUC = ACM-SALDO-SUC + WS-SUC-IMPORTE. 
                                                                        
           COMPUTE ACM-SALDO-TOTAL = ACM-SALDO-TOTAL + WS-SUC-IMPORTE. 
                                                                        
       2200-F-ACUMULAR-SALDO. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *                 2 3 0 0 - C O R T E - T I P O                  *
      *----------------------------------------------------------------*
                                                                        
       2300-CORTE-TIPO. 
                                                                        
           MOVE '2300-CORTE-TIPO'             TO WS-PARRAFO. 
                                                                        
           MOVE ACM-SALDO-TIPO                TO WS-MENOR-EDIT. 
                                                                        
           DISPLAY '  TIPO DE CUENTA: ' WS-TIP-ANT WS-MENOR-EDIT. 
                                                                        
           MOVE 0                             TO ACM-SALDO-TIPO. 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT. 
                                                                        
       2300-F-CORTE-TIPO. 
           EXIT. 
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *             2 4 0 0 - M O S T R A R - S U C U R S A L          *
      *----------------------------------------------------------------*
                                                                        
       2400-MOSTRAR-SUCURSAL. 
                                                                        
           MOVE '2400-MOSTRAR-SUCURSAL'       TO WS-PARRAFO. 
                                                                        
           MOVE ACM-SALDO-SUC                 TO WS-SALDO-EDIT. 
                                                                        
           DISPLAY '        TOTAL SUCURSAL:  '   WS-SALDO-EDIT. 
                                                                        
           MOVE 0                             TO ACM-SALDO-SUC. 
                                                                        
           MOVE WS-CLAVE-ACT                  TO WS-CLAVE-ANT. 
                                                                        
       2400-F-MOSTRAR-SUCURSAL. 
           EXIT. 
                                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 6 0 0 - T R A T A R - U L T I M O              *
      *----------------------------------------------------------------*
                                                                        
       2600-TRATAR-ULTIMO. 
                                                                        
           MOVE '2600-TRATAR-ULTIMO'          TO WS-PARRAFO. 
                                                                        
           PERFORM 2300-CORTE-TIPO 
              THRU 2300-F-CORTE-TIPO. 
                                                                        
           PERFORM 2400-MOSTRAR-SUCURSAL 
              THRU 2400-F-MOSTRAR-SUCURSAL. 
                                                                        
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
                                                                        
           MOVE ACM-SALDO-TOTAL               TO WS-SALDO-EDIT. 
                                                                        
           DISPLAY '                                       ' 
           DISPLAY '**************************************************' 
           DISPLAY '*                 PROGRAMA CORTEC12              *' 
           DISPLAY '**************************************************' 
           DISPLAY '                                       ' 
           DISPLAY '* CANTIDAD TOTAL DE REGISTROS LEIDOS: ' 
                                                  CNT-LEIDOS-ENTRADA. 
           DISPLAY '* SALDO ACUMULADO: ' WS-SALDO-EDIT. 
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