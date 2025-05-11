      ******************************************************************
       IDENTIFICATION DIVISION. 
      ******************************************************************
                                                                        
       PROGRAM-ID.    PGMSIN14. 
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB 
      *    DATE-WRITTEN.  MAYO-2025 
                                                                        
      *----------------------------------------------------------------*
      *   ACTIVIDAD SINCRONICA CLASE 14 | VALIDACION Y ESCRITURA       *
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ESTE PROGRAMA REALIZA LA LECTURA DE UN ARCHIVO QSAM (NOVCLI-  *
      *  -EN) VALIDANDO LOS REGISTROS POR TIPO DE DOCUMENTO. DE SER    *
      *  VALIDOS, GRABA LOS MISMOS EN UN ARCHIVO DE SALIDA, DE SER I-  *
      *  -NVALIDOS REALIZA EL DISPLAY DE LOS MISMO. AL FINALIZAR, MU-  *
      *  -ESTRA UN TOTAL DE REGISTROS LEIDOS, VALIDOS E INVALIDOS.     *
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
                                                                        
           SELECT SALIDA  ASSIGN TO SALIDA 
                                    FILE STATUS IS FS-SALIDA. 
                                                                        
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
                                                                        
       FD   SALIDA 
            RECORDING MODE IS F. 
       01   REG-SALIDA. 
            02 NOV-SECUEN                                   PIC 9(05). 
            02 NOV-RESTO                                    PIC X(50). 
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION. 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  C O N S T A N T E S                *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES. 
           02 CT-PROGRAMA                   PIC X(08)  VALUE 'PGMSIN14'.
           02 CT-OPEN                       PIC X(08)  VALUE 'OPEN    '.
           02 CT-READ                       PIC X(08)  VALUE 'READ    '.
           02 CT-WRITE                      PIC X(08)  VALUE 'WRITE   '.
           02 CT-CLOSE                      PIC X(08)  VALUE 'CLOSE   '.
           02 CT-ENTRADA                    PIC X(08)  VALUE 'ENTRADA '.
           02 CT-SALIDA                     PIC X(08)  VALUE 'SALIDA  '.
                                                                        
      *----------------------------------------------------------------*
      *               A R E A  D E  V A R I A B L E S                  *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES. 
           02 WS-PARRAFO                    PIC X(50). 
           02 WS-MASCARA                    PIC ZZZ9. 
           02 WS-GRABADOS                   PIC 9(03). 
                                                                        
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
           02 CNT-REG-LEIDOS                PIC 9(03)  VALUE ZEROS. 
           02 CNT-REG-GRABADOS              PIC 9(03)  VALUE ZEROS. 
           02 CNT-REG-ERRONEOS              PIC 9(03)  VALUE ZEROS. 
                                                           
      *----------------------------------------------------------------*
      *               A R E A  D E  F I L E - S T A T U S              *
      *----------------------------------------------------------------*
                                                                        
       01 FS-FILE-STATUS. 
           02 FS-ENTRADA                    PIC X(02). 
              88 FS-ENTRADA-OK                         VALUE '00'. 
              88 FS-ENTRADA-EOF                        VALUE '10'. 
                                                                        
           02 FS-SALIDA                     PIC X(02). 
              88 FS-SALIDA-OK                          VALUE '00'. 
                                                                        
      *----------------------------------------------------------------*
      *                     A R E A  D E  C O P Y S                    *
      *----------------------------------------------------------------*
                                                                        
           COPY CPNOVCLI. 
                                                                        
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
                                                                        
           PERFORM 2200-VALIDAR-REGISTRO 
              THRU 2200-F-VALIDAR-REGISTRO. 
                                                                        
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
                                                                        
           OPEN INPUT   ENTRADA 
                OUTPUT  SALIDA. 
                                                                        
           IF NOT FS-ENTRADA-OK 
              MOVE CT-OPEN                    TO AUX-ERR-ACCION 
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE 
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS 
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE 
              MOVE 10                         TO W-N-ERROR 
                                                                        
              PERFORM 9000-SALIDA-ERRORES 
                 THRU 9000-F-SALIDA-ERRORES 
           END-IF. 
                                                                        
           IF NOT FS-SALIDA-OK 
              MOVE CT-OPEN                    TO AUX-ERR-ACCION 
              MOVE CT-SALIDA                  TO AUX-ERR-NOMBRE 
              MOVE FS-SALIDA                  TO AUX-ERR-STATUS 
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
                                                                        
           READ ENTRADA INTO WS-REG-NOVCLIE. 
                                                                        
           EVALUATE TRUE 
               WHEN FS-ENTRADA-OK 
                    ADD 1                     TO CNT-REG-LEIDOS 
                                                                        
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
      *           2 2 0 0 - V A L I D A R - R E G I S T R O            *
      *----------------------------------------------------------------*
                                                                        
       2200-VALIDAR-REGISTRO. 
                                                                        
           MOVE '2200-VALIDAR-REGISTRO'       TO WS-PARRAFO. 
                                                                        
             IF NOV-TIP-DOC IS EQUAL TO 'DU' 
                                   OR NOV-TIP-DOC IS EQUAL TO 'PA' 
                                   OR NOV-TIP-DOC IS EQUAL TO 'PE' 
                                   OR NOV-TIP-DOC IS EQUAL TO 'CI' 
                                                                        
                PERFORM 2400-GRABAR-SALIDA 
                   THRU 2400-F-GRABAR-SALIDA 
                                                                        
             ELSE 
                ADD 1 TO CNT-REG-ERRONEOS 
             END-IF. 
                                                                        
       2200-F-VALIDAR-REGISTRO. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *               2 4 0 0 - G R A B A R - S A L I D A              *
      *----------------------------------------------------------------*
                                                                        
       2400-GRABAR-SALIDA. 
                                                                        
           MOVE '2400-GRABAR-SALIDA'          TO WS-PARRAFO. 
                                                                        
           ADD 1 TO WS-GRABADOS 
                                                                        
           MOVE WS-GRABADOS           TO NOV-SECUEN 
           MOVE WS-REG-NOVCLIE        TO NOV-RESTO 
                                                                        
           WRITE REG-SALIDA. 
                                                                        
           EVALUATE TRUE 
             WHEN FS-SALIDA-OK 
               ADD 1 TO CNT-REG-GRABADOS 
                                                                        
              WHEN OTHER 
                MOVE CT-WRITE TO AUX-ERR-ACCION 
                MOVE CT-SALIDA TO AUX-ERR-NOMBRE 
                MOVE FS-SALIDA TO AUX-ERR-STATUS 
                MOVE WS-PARRAFO TO AUX-ERR-MENSAJE 
                MOVE 10 TO W-N-ERROR 
                                                                        
                PERFORM 9000-SALIDA-ERRORES 
                THRU 9000-F-SALIDA-ERRORES 
           END-EVALUATE. 
                                                                        
       2400-F-GRABAR-SALIDA. 
           EXIT. 
                                                                        
      *----------------------------------------------------------------*
      *              3 2 0 0 - C E R R A R - A R C H I V O S           *
      *----------------------------------------------------------------*
                                                                        
       3200-CERRAR-ARCHIVOS. 
                                                                        
           MOVE '3200-CERRAR-ARCHIVOS'        TO WS-PARRAFO. 
                                                                        
           CLOSE ENTRADA 
                 SALIDA. 
                                                                        
           IF NOT FS-ENTRADA-OK 
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION 
              MOVE CT-ENTRADA                 TO AUX-ERR-NOMBRE 
              MOVE FS-ENTRADA                 TO AUX-ERR-STATUS 
              MOVE WS-PARRAFO                 TO AUX-ERR-MENSAJE 
              MOVE 10                         TO W-N-ERROR 
                                                                        
              PERFORM 9000-SALIDA-ERRORES 
                 THRU 9000-F-SALIDA-ERRORES 
           END-IF. 
                                                                        
           IF NOT FS-SALIDA-OK 
              MOVE CT-CLOSE                   TO AUX-ERR-ACCION 
              MOVE CT-SALIDA                  TO AUX-ERR-NOMBRE 
              MOVE FS-SALIDA                  TO AUX-ERR-STATUS 
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
                                                                        
           MOVE CNT-REG-LEIDOS                TO WS-MASCARA. 
                                                                        
           DISPLAY ' ' 
           DISPLAY '**************************************************' 
           DISPLAY '*                PROGRAMA PGMSIN14               *' 
           DISPLAY '**************************************************' 
           DISPLAY ' '. 
           DISPLAY '**************************************************' 
           DISPLAY '*                                                *' 
           DISPLAY '* CANTIDAD DE REGISTROS LEIDOS: ' WS-MASCARA 
                                           '             *'. 
           DISPLAY '*                                                *' 
                                                                        
           MOVE CNT-REG-GRABADOS              TO WS-MASCARA. 
                                                                        
           DISPLAY '* CANTIDAD DE REGISTROS GRABADOS: ' WS-MASCARA 
                                           '           *'. 
           DISPLAY '*                                                *' 
                                                                        
           MOVE CNT-REG-ERRONEOS              TO WS-MASCARA. 
                                                                        
           DISPLAY '* CANTIDAD DE REGISTROS ERRONEOS: ' WS-MASCARA 
                                           '           *'. 
           DISPLAY '*                                                *' 
           DISPLAY '**************************************************' 
           DISPLAY '                                                 '. 
                                                                        
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