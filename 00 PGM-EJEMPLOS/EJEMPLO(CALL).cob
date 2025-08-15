       IDENTIFICATION DIVISION.                                         
      *                                                        *        
        PROGRAM-ID PGMCALL.                                             
      **********************************************************        
      *                                                        *        
      *  EJEMPLO DE LLAMADO A RUTINA DINAMICO Y ESTATICO       *        
      *                                                        *        
      **********************************************************        
      *      MANTENIMIENTO DE PROGRAMA                         *        
      **********************************************************        
      *  FECHA   *    DETALLE        * COD *                            
      **************************************                            
      *          *                   *     *                            
      *          *                   *     *                            
      *          *                   *     *                            
      *          *                   *     *                            
      *          *                   *     *                            
      **************************************                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT NOVEDAD ASSIGN DDNOVED                              
                    FILE STATUS IS WS-NOV-CODE.                         
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD NOVEDAD                                                       
            BLOCK CONTAINS 0 RECORDS                                    
            RECORDING MODE IS F.                                        
                                                                        
       01 REG-NOVED      PIC X(93).                                     
                                                                        
                                                                        
      **************************************                            
       WORKING-STORAGE SECTION.                                         
      **************************************                            
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
                                                                        
       77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'.  
       77  WS-NOV-CODE      PIC XX    VALUE SPACES.                     
       77  WS-PGMRUT        PIC X(8)  VALUE 'PGMRUT'.                   
      **************************************                            
                                                                        
       01  WS-STATUS-NOV    PIC X.                                      
           88  WS-FIN-NOV             VALUE 'Y'.                        
           88  WS-NO-FIN-NOV          VALUE 'N'.                        
       77  FILLER         PIC X(15)   VALUE "FECHA PROCESO: ".          
       01  WS-FECHA.                                                    
           03  WS-ANIO    PIC 9(02)   VALUE ZEROS.                      
           03  WS-MES     PIC 9(02)   VALUE ZEROS.                      
           03  WS-DIA     PIC 9(02)   VALUE ZEROS.                      
                                                                        
      **************************************                            
      *         LAYOUT NOVEDAD             *                            
      **************************************                            
       01  WS-REG-NOVEDAD.                                              
                                                                        
           03  WS-NOV-TIPO         PIC XX       VALUE SPACES.           
                                                                        
               88 WS-DU          VALUE 'DU'.                            
               88 WS-CI          VALUE 'CI'.                            
               88 WS-PA          VALUE 'PA'.                            
               88 WS-PE          VALUE 'PE'.                            
                                                                        
           03  WS-NOV-NRO          PIC 9(11)   VALUE ZEROS.             
           03  WS-NOV-NOMBRE       PIC X(30)   VALUE SPACES.            
           03  WS-ESTADO-CIVIL     PIC X(10)   VALUE SPACES.            
           03  WS-SEXO             PIC X       VALUE SPACES.            
           03  FILLER              PIC X(39)   VALUE SPACES.            
                                                                        
      * CONTADOR DE LEIDOS                                              
       77  WS-CANT-NOV             PIC 9(3)      VALUE ZEROS.           
       77  WS-NOV-EDIT             PIC Z(3)      VALUE ZEROS.           
       77  WS-NOV-TIPO-ANT         PIC X(2)      VALUE SPACES.          
       77  WS-CONTADOR-TIPO        PIC 9(2)      VALUE ZEROS.           
                                                                        
      * CONTADOR DE REGISTROS QUE CUMPLEN LA CONDICION                  
       77  WS-CANT-CONDICION       PIC 9(3)      VALUE ZEROS.           
       77  WS-COND-EDIT            PIC Z(3)      VALUE ZEROS.           
                                                                        
      **************************************                            
       LINKAGE SECTION.                                                 
       01  LK-COMUNICACION.                                             
           03  LK-SIGLO            PIC 9(02).                           
           03  LK-ANIO             PIC 9(02).                           
           03  LK-MES              PIC 9(02).                           
           03  LK-DIA              PIC 9(02).                           
           03  FILLER              PIC X(22).                           
      **************************************                            
                                                                        
      ***************************************************************.  
       PROCEDURE DIVISION USING LK-COMUNICACION.                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       MAIN-PROGRAM.                                                    
                                                                        
           PERFORM 1000-INICIO  THRU   F-1000-INICIO.                   
                                                                        
           PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                   
                   UNTIL WS-FIN-NOV.                                    
                                                                        
           PERFORM 9999-FINAL    THRU  F-9999-FINAL.                    
                                                                        
       F-MAIN-PROGRAM. GOBACK.                                          
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            
      *                                    *                            
      **************************************                            
       1000-INICIO.                                                     
           ACCEPT WS-FECHA FROM DATE.                                   
           SET WS-NO-FIN-NOV TO TRUE.                                   
                                                                        
           OPEN INPUT  NOVEDAD.                                         
                                                                        
                                                                        
           IF WS-NOV-CODE IS NOT EQUAL '00'                             
              DISPLAY '* ERROR EN OPEN NOVEDAD = ' WS-NOV-CODE          
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-NOV     TO TRUE                               
           END-IF.                                                      
                                                                        
           PERFORM 2500-LEER     THRU F-2500-LEER.                      
           MOVE WS-NOV-TIPO       TO WS-NOV-TIPO-ANT.                   
           PERFORM 1200-CALL-FECHA   THRU F-1200-CALL-FECHA.            
                                                                        
       F-1000-INICIO.   EXIT.                                           
                                                                        
      * INVOCA RUTINA DE FECHAS PARA OBTENER ÚLTIMO DIA DEL MES         
       1200-CALL-FECHA.                                                 
                                                                        
              MOVE SPACES           TO    LK-COMUNICACION               
              MOVE 20         TO  LK-SIGLO                              
              MOVE WS-ANIO    TO  LK-ANIO                               
              MOVE WS-MES     TO  LK-MES                                
                                                                        
      * INVOCA CALL ESTATICO                                            
      *       CALL  'PGMRUT'   USING LK-COMUNICACION                    
                                                                        
      * INVOCA CALL DINAMICO                                            
              CALL  WS-PGMRUT  USING LK-COMUNICACION                    
                                                                        
              IF RETURN-CODE    EQUAL    05                             
                SET  WS-FIN-NOV   TO TRUE                               
                DISPLAY  'ERROR RUTINA FECHA: 05'                       
              END-IF.                                                   
                                                                        
       F-1200-CALL-FECHA. EXIT.                                         
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DE PROCESOS      *                            
      *                                    *                            
      **************************************                            
       2000-PROCESO.                                                    
      *                                                                 
           IF WS-NOV-TIPO  EQUAL WS-NOV-TIPO-ANT                        
              ADD 1 TO WS-CONTADOR-TIPO                                 
           ELSE                                                         
                                                                        
                                                                        
              PERFORM  3000-MOSTRAR-TOTAL THRU                          
                     F-3000-MOSTRAR-TOTAL                               
              INITIALIZE WS-CONTADOR-TIPO                               
              MOVE WS-NOV-TIPO    TO    WS-NOV-TIPO-ANT                 
           END-IF.                                                      
                                                                        
           PERFORM 2500-LEER     THRU F-2500-LEER.                      
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
                                                                        
      **************************************                            
      *  LEER REGISTROS                    *                            
      **************************************                            
       2500-LEER.                                                       
           READ NOVEDAD   INTO WS-REG-NOVEDAD                           

           EVALUATE WS-NOV-CODE                                         
             WHEN '00'                                                  
                            ADD 1 TO WS-CANT-NOV                        
              WHEN '10'                                                 
              SET WS-FIN-NOV  TO TRUE                                   
                                                                        
           WHEN OTHER                                                   
              DISPLAY '* ERROR EN LECTURA NOVEDAD = ' WS-NOV-CODE       
              MOVE 9999 TO RETURN-CODE                                  
              SET WS-FIN-NOV  TO TRUE                                   
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-2500-LEER. EXIT.                                               
                                                                        
       3000-MOSTRAR-TOTAL.                                              
           DISPLAY ' TOTAL TIPO DOCUMENTO: '  WS-NOV-TIPO               
                WS-CONTADOR-TIPO.                                       
                                                                        
       F-3000-MOSTRAR-TOTAL. EXIT.                                      
                                                                        
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL CIERRE DE FILES      *                            
      *                                    *                            
      **************************************                            
       9999-FINAL.                                                      
                                                                        
           CLOSE NOVEDAD                                                
              IF WS-NOV-CODE IS NOT EQUAL '00'                          
                DISPLAY '* ERROR EN CLOSE NOVEDAD = '                   
                                            WS-NOV-CODE                 
                MOVE 9999 TO RETURN-CODE                                
                SET WS-FIN-NOV     TO TRUE                              
             END-IF.                                                    
                                                                        
                                                                        
      **************************************                            
      *   MOSTRAR TOTALES DE CONTROL                                    
      **************************************                            
                                                                        
                DISPLAY '                           ' .                 
                MOVE WS-CANT-NOV  TO    WS-NOV-EDIT.                    
                DISPLAY 'CANTIDAD REGISTROS LEIDOS  '   WS-NOV-EDIT.    
                                                                        
                MOVE WS-CANT-CONDICION TO WS-COND-EDIT.                 
                DISPLAY 'CANTIDAD QUE CUMPLEN COND  '                   
                                    WS-COND-EDIT.                       
                                                                        
       F-9999-FINAL.                                                    
           EXIT.                                                                                                                                