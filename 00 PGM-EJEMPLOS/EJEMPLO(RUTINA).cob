       IDENTIFICATION DIVISION.                                         
        PROGRAM-ID PGMRUT.                                              
      **************************************                            
      *                                    *                            
      *  RUTINA QUE DEVUELVE úLTIMO DIA DEL*                            
      *  MES                               *                            
      *  PROGRAMA LLAMADOR ENVIA:          *                            
      *  AñO EN 4 CARACTERES NUMERICOS     *                            
      *  MES EN 2 CARACTERES NUMERICOS     *                            
      *                                    *                            
      *  RUTINA DEVUELVE:                  *                            
      *  AñO EN 4 CARACTERES NUMERICOS     *                            
      *  MES EN 2 CARACTERES NUMERICOS     *                            
      *  DIA EN 2 CARACTERES NUMERICOS     *                            
      *  RETURN-CODE EXITOSO = ZEROS       *                            
      *  RETURN-CODE PARA FECHA INVAL.= 05 *                            
      **************************************                            
      *      MANTENIMIENTO DE PROGRAMA     *                            
      **************************************                            
      *  FECHA   *    DETALLE        * COD *                            
      **************************************                            
      *          *                   *     *                            
      *          *                   *     *                            
      **************************************                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
      **************************************************************    
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
                                                                        
       77  WS-RUTINA     PIC X(45)             VALUE                    
                  "      *      PROGRAMA COBOL 2030            *".      
                                                                        
       01  WS-AREA.                                                     
           03  WS-AREA-ANIO   PIC 9999     VALUE ZEROS.                 
           03  WS-AREA-MES    PIC 99       VALUE ZEROS.                 
           03  WS-AREA-DIA    PIC 99       VALUE ZEROS.                 
           03  FILLER         PIC X(22)    VALUE SPACES.                
                                                                        
       01  WS-FECHA-RECIBIDA.                                           
           03  WS-RECI-ANIO   PIC 9999     VALUE ZEROS.                 
           03  WS-RECI-MES    PIC 99       VALUE ZEROS.                 
                                                                        
       77  WS-RESULTADO    PIC 9(4)    VALUE ZEROS.                     
       77  WS-REMAINDER    PIC 99      VALUE ZEROS.                     
                                                                        
       01  WS-MESES.                                                    
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 28.              
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 30.              
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 30.              
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 30.              
             05  FILLER    PIC 9(02)             VALUE 31.              
             05  FILLER    PIC 9(02)             VALUE 30.              
             05  FILLER    PIC 9(02)             VALUE 31.              
         01 FILLER          REDEFINES WS-MESES.                         
            03 WS-ITEM-TABLA              OCCURS 12 TIMES.              
               05  WS-ITEM-DIA   PIC 9(02).                             
                                                                        
       77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.  
                                                                        
      **************************************************************    
       LINKAGE SECTION.                                                 
                                                                        
       01  LK-AREA.                                                     
           03 FILLER.                                                   
              05 LK-SIGLO    PIC 99.                                    
              05 LK-ANIO     PIC 99.                                    
           03 LK-MES      PIC 99.                                       
           03 LK-DIA      PIC 99.                                       
           03 FILLER      PIC X(22).                                    
                                                                        
      ***************************************************************.  
       PROCEDURE DIVISION USING LK-AREA.                                
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
      *                                    *                            
      **************************************                            
       MAIN-PROGRAM.                                                    
                                                                        
           PERFORM 1000-INICIO  THRU   F-1000-INICIO.                   
                                                                        
           IF RETURN-CODE = ZEROS                                       
              PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                
           END-IF.                                                      
                                                                        
           PERFORM 9999-FINAL    THRU  F-9999-FINAL.                    
                                                                        
       F-MAIN-PROGRAM. GOBACK.                                          
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO INICIO INDICES             *                            
      *                                    *                            
      **************************************                            
       1000-INICIO.                                                     
           MOVE ZEROS      TO RETURN-CODE.                              
           MOVE LK-AREA       TO WS-AREA.                               
           MOVE WS-AREA-MES   TO WS-RECI-MES.                           
           MOVE WS-AREA-ANIO  TO WS-RECI-ANIO.                          
           PERFORM 1100-VALIDAR-AREA  THRU F-1100-VALIDAR-AREA.         
                                                                        
       F-1000-INICIO.                                                   
           EXIT.                                                        
                                                                        
      **************************************                            
       1100-VALIDAR-AREA.                                               
      **************************************                            
           IF WS-AREA-MES  = ZEROS OR                                   
              WS-AREA-MES > 12     OR                                   
              WS-AREA-ANIO = ZEROS                                      
                  MOVE 05  TO RETURN-CODE                               
           END-IF.                                                      
                                                                        
       F-1100-VALIDAR-AREA.                                             
           EXIT.                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO PRINCIPAL DE PROCESO       *                            
      *                                    *                            
      **************************************                            
       2000-PROCESO.                                                    
                                                                        
           IF WS-AREA-MES = 02                                          
      * CALCULO AÑO BISIESTO               *                            
              DIVIDE WS-AREA-ANIO BY 4 GIVING WS-RESULTADO              
                  REMAINDER    WS-REMAINDER                             
              IF WS-REMAINDER = ZEROS                                   
                 MOVE 29    TO WS-ITEM-DIA (2)                          
              END-IF                                                    
           END-IF.                                                      
                                                                        
                                                                        
           MOVE WS-ITEM-DIA (WS-AREA-MES)   TO WS-AREA-DIA.             
                                                                        
       F-2000-PROCESO. EXIT.                                            
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO FINAL MUESTRA RESULTADO    *                            
      *                                    *                            
      **************************************                            
       9999-FINAL.                                                      
                                                                        
           MOVE WS-AREA          TO LK-AREA.                            
           DISPLAY "***PGMRUT - CóDIGO DE RETORNO ES   ****** "         
                                     RETURN-CODE                        
           DISPLAY "   FECHA RECIBIDA: "  WS-FECHA-RECIBIDA.            
           DISPLAY "   FECHA ENVIADA : "  WS-AREA.                      
                                                                        
       F-9999-FINAL.  EXIT.                                             
                                                                        
      *                                                                                                                 