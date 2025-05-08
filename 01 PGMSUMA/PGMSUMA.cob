       IDENTIFICATION DIVISION. 
      *                                                        * 
       PROGRAM-ID. PGMSUMA. 
                                                                        
      ********************************************************** 
      *                                                        * 
      *  CLASE ASINCRONICA 5 | MATIAS MAZZITELLI               * 
      *                                                        * 
      *    SE SOLICITA LA CONFECCION DE UN PROGRAMA COBOL      * 
      *    QUE REALICE LA SUMA DE LOS PRIMEROS 10 NUMEROS      * 
      *                                                        * 
      *  FECHA: 09/04/2025                      KC03CAB | MDEV * 
      *                                                        * 
      ********************************************************** 
                                                                        
       DATA DIVISION. 
      ************************************** 
       WORKING-STORAGE SECTION. 
      ************************************** 
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'. 
       77  CONTADOR      PIC 99    VALUE ZEROES. 
       77  SUMADOR       PIC 99    VALUE ZEROES. 
                                                                        
      ***************************************************************. 
                                                                        
       PROCEDURE DIVISION. 
                                                                        
      ************************************** 
      *                                    * 
      *  CUERPO PRINCIPAL DEL PROGRAMA     * 
      *                                    * 
      ************************************** 
                                                                        
       MAIN-PROGRAM. 
           PERFORM UNTIL CONTADOR = 10 
             ADD 1 TO CONTADOR 
             COMPUTE SUMADOR = SUMADOR + CONTADOR 
           END-PERFORM. 
                                                                        
           DISPLAY "EJERCICIO CLASE ASINCRONICA 5". 
           DISPLAY "LA SUMA DE LOS PRIMEROS 10 NUMEROS". 
           DISPLAY "RESULTADO: " SUMADOR. 
           GOBACK. 