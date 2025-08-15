       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PGMALD1F.                                            
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  WS-VARIABLES.                                                
           03 WS-MAP-00            PIC X(07)       VALUE 'MAP3D1F'.     
           03 WS-MAPSET-00         PIC X(07)       VALUE 'MAP3D1F'.     
           03 WS-LONG              PIC S9(04) COMP.                     
           03 WS-COMLONG           PIC S9(04) COMP.                     
           03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.            
           03 WS-FECHA             PIC X(10)       VALUE SPACES.        
           03 WS-SEP-DATE          PIC X           VALUE '/'.           
           03 WS-HORA              PIC X(08)       VALUE SPACES.        
           03 WS-SEP-HOUR          PIC X           VALUE ':'.           
           03 WS-RESP              PIC S9(04) COMP.                     
           03 SW-CONFIRMAR         PIC X VALUE 'Y'.                     
           03 WS-NORMAL    PIC X VALUE '*'.                             
           03 WS-ENTER     PIC X VALUE ' '.                             
                                                                        
       01  CT-CONSTANTES.                                               
           03 CT-MSGO.                                                  
              05 CT-MNS-01         PIC X(72) VALUE                      
                 'INGRESE LOS DATOS Y PRESIONE ENTER'.                  
              05 CT-MNS-02         PIC X(72) VALUE                      
                 'TIPO Y NRO DE DOC EXISTENTES - REINGRESAR '.          
              05 CT-MNS-03         PIC X(72) VALUE                      
                 'TIPO DE DOCUMENTO INVALIDO - REINGRESAR '.            
              05 CT-MNS-04         PIC X(72) VALUE                      
                 'NRO DE DOCUMENTO INVALIDO - REINGRESAR'.              
              05 CT-MNS-05         PIC X(72) VALUE                      
                 'NOMBRE Y APELLIDO INVALIDO - REINGRESAR'.             
              05 CT-MNS-06         PIC X(72) VALUE                      
                 'FECHA DE NACIMIENTO INVALIDA - REINGRESAR'.           
              05 CT-MNS-07         PIC X(72) VALUE                      
                 'SEXO INVALIDO - REINGRESAR'.                          
              05 CT-MNS-08         PIC X(72) VALUE                      
                 'CLIENTE DADO DE ALTA CON EXITO'.                      
              05 CT-MNS-09         PIC X(72) VALUE                      
                 'PROBLEMA CON EL ARCHIVO PERSONA'.                     
              05 CT-MNS-10         PIC X(72) VALUE                      
                 'TECLA INVALIDA'.                                      
              05 CT-MNS-EXIT       PIC X(72) VALUE                      
                 'FIN TRANSACCION T308'.                                
           03 CT-DATASET           PIC X(08)  VALUE 'PERSONA'.          
           03 CT-DATASET-LEN       PIC S9(04) COMP VALUE 160.           
           03 CT-DATASET-KEYLEN    PIC S9(04) COMP VALUE 013.           
                                                                        
       COPY MAP3D1F.                                                    
       COPY DFHBMSCA.                                                   
       COPY DFHAID.                                                     
       COPY CPPERSON.                                                   
                                                                        
       01  WS-COMMAREA.                                                 
           03 WS-USER-DATA.                                             
              05 WS-USER-TIPDOC    PIC X(02).                           
              05 WS-USER-NRODOC    PIC 9(11).                           
           03 WS-TIP-DOC           PIC X(02).                           
              88 WS-TIP-DOC-BOOLEAN                   VALUE 'DU'        
                                                            'PA'        
                                                            'PE'.       
           03 FILLER               PIC X(5).                            
                                                                        
      *********VARIABLES DE VALIDACION                                  
                                                                        
       01  WS-FECHA-VAL.                                                
           03 WS-ANIO              PIC 9(04)          VALUE ZEROS.      
           03 WS-MES               PIC 9(02)          VALUE ZEROS.      
           03 WS-DIA               PIC 9(02)          VALUE ZEROS.      
                                                                        
       77  WS-FECHA-VALIDA         PIC X.                               
           88 FECHAOK                                 VALUE 'Y'.        
           88 FECHANOOK                               VALUE 'N'.        
                                                                        
       77  WS-CLIENTE-VALIDO       PIC X.                               
           88 CLIENTEOK                               VALUE 'Y'.        
           88 CLIENTENOOK                             VALUE 'N'.        
                                                                        
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01 DFHCOMMAREA PIC X(20).                                        
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
                                                                        
       MAIN-PROGRAM-INICIO.                                             
                                                                        
           PERFORM 1000-I-INICIO  THRU 1000-F-INICIO.                   
                                                                        
           PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO.                  
                                                                        
           PERFORM 9999-I-FINAL THRU 9999-F-FINAL.                      
                                                                        
       MAIN-PROGRAM-FINAL.                                              
                                                                        
                                                                        
       1000-I-INICIO.                                                   
                                                                        
           MOVE LOW-VALUES TO MAP3D1FO.                                 
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             

           IF EIBCALEN = 0                                              
                                                                        
               MOVE LENGTH OF MAP3D1FO TO WS-LONG                       
               MOVE CT-MNS-01 TO MSGO                                   
               PERFORM 7000-I-TIME THRU 7000-F-TIME                     
                                                                        
               EXEC CICS                                                
                  SEND MAP (WS-MAP-00)                                  
                       MAPSET (WS-MAPSET-00)                            
                       FROM (MAP3D1FO)                                  
                       LENGTH (WS-LONG)                                 
                       ERASE                                            
                       FREEKB                                           
               END-EXEC                                                 
                                                                        
               PERFORM 9999-I-FINAL THRU 9999-F-FINAL                   
                                                                        
           END-IF.                                                      
                                                                        
                                                                        
       1000-F-INICIO. EXIT.                                             
                                                                        
       2000-I-PROCESO.                                                  
                                                                        
           MOVE LENGTH OF MAP3D1FO TO WS-LONG                           

           EXEC CICS                                                    
                RECEIVE MAP    (WS-MAP-00)                              
                        MAPSET (WS-MAPSET-00)                           
                        INTO   (MAP3D1FI)                               
                        RESP   (WS-RESP)                                
           END-EXEC                                                     
                                                                        
           MOVE TIPDOCI      TO WS-USER-TIPDOC.                         
           MOVE NUMDOCI      TO WS-USER-NRODOC.                         
                                                                        
           PERFORM 2200-I-TECLAS THRU 2200-F-TECLAS.                    
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
       2200-I-TECLAS.                                                   
                                                                        
           EVALUATE EIBAID                                              
                                                                        
             WHEN DFHENTER                                              
               PERFORM 3000-I-ENTER THRU 3000-F-ENTER                   
             WHEN DFHPF3                                                
               PERFORM 3100-I-PF3   THRU 3100-F-PF3                     
             WHEN DFHPF12                                               
               PERFORM 9000-I-PF12  THRU 9000-F-PF12                    
             WHEN OTHER                                                 
               MOVE CT-MNS-10        TO  MSGO                           
               PERFORM 7000-I-TIME  THRU 7000-F-TIME                    
               EXEC CICS                                                
                    SEND MAP    (WS-MAP-00)                             
                      MAPSET (WS-MAPSET-00)                             
                      FROM   (MAP3D1FO)                                 
                      LENGTH (WS-LONG)                                  
                      ERASE                                             
               END-EXEC                                                 
           END-EVALUATE.                                                
                                                                        
       2200-F-TECLAS. EXIT.                                             
                                                                        
       3000-I-ENTER.                                                    
                                                                        
           PERFORM    3500-I-VALIDAR THRU 3500-F-VALIDAR                
                                                                        
           IF CLIENTEOK                                                 
              PERFORM 4000-I-WRITE   THRU 4000-F-WRITE                  
           ELSE                                                         
              PERFORM 5000-I-SEND THRU 5000-F-SEND                      
                                                                        
           END-IF.                                                      
                                                                        
       3000-F-ENTER. EXIT.                                              
                                                                        
       3100-I-PF3.                                                      
                                                                        
           MOVE LOW-VALUES TO MAP3D1FO.                                 
           PERFORM 7000-I-TIME THRU 7000-F-TIME.                        
           MOVE CT-MNS-01      TO MSGO                                  
                                                                        
           EXEC CICS                                                    
               SEND MAP    (WS-MAP-00)                                  
               MAPSET (WS-MAPSET-00)                                    
               FROM   (MAP3D1FO)                                        
               LENGTH (WS-LONG)                                         
               ERASE                                                    
           END-EXEC.                                                    
                                                                        
       3100-F-PF3. EXIT.                                                
                                                                        
       3500-I-VALIDAR.                                                  
                                                                        
           SET CLIENTEOK  TO TRUE.                                      
                                                                        
           MOVE TIPDOCI TO WS-TIP-DOC.                                  
                                                                        
           IF NOT WS-TIP-DOC-BOOLEAN                                    
              SET CLIENTENOOK TO TRUE                                   
              MOVE CT-MNS-03  TO MSGO                                   
           ELSE                                                         
              IF NUMDOCI IS NOT NUMERIC                                 
                 SET CLIENTENOOK TO TRUE                                
                 MOVE CT-MNS-04  TO MSGO                                
              ELSE                                                      
                 IF NUMDOCI IS EQUAL ZEROS                              
                    SET CLIENTENOOK TO TRUE                             
                    MOVE CT-MNS-04  TO MSGO                             
              END-IF                                                    
           END-IF.                                                      
                                                                        
           IF CLIENTEOK                                                 
              MOVE TIPDOCI TO WS-USER-TIPDOC                            
              MOVE NUMDOCI TO WS-USER-NRODOC                            
                                                                        
              EXEC CICS                                                 
                 READ DATASET ('PERSONA')                               
                      RIDFLD  (WS-USER-DATA)                            
                      INTO (REG-PERSONA)                                
                      LENGTH (CT-DATASET-LEN)                           
                      EQUAL                                             
                      RESP (WS-RESP)                                    
              END-EXEC                                                  
                                                                        
              EVALUATE WS-RESP                                          
                  WHEN DFHRESP(NORMAL)                                  
                     MOVE CT-MNS-02  TO MSGO                            

                  WHEN DFHRESP(NOTFND)                                  
                     PERFORM 3600-I-CONTINUA-VAL                        
                             THRU 3600-F-CONTINUA-VAL                   
                  WHEN OTHER                                            
                     MOVE CT-MNS-09            TO  MSGO                 
                                                                        
              END-EVALUATE                                              
           END-IF.                                                      
       3500-F-VALIDAR. EXIT.                                            
                                                                        
       3600-I-CONTINUA-VAL.                                             
                                                                        
           IF NOMAPEI IS EQUAL TO (SPACES OR LOW-VALUES)                
              MOVE -1 TO NOMAPEL                                        
              SET CLIENTENOOK TO TRUE                                   
              MOVE CT-MNS-05  TO MSGO                                   
           ELSE                                                         
              PERFORM 3700-I-FECHA THRU 3700-F-FECHA                    
              IF FECHANOOK                                              
                 SET CLIENTENOOK TO TRUE                                
                 MOVE CT-MNS-06  TO MSGO                                
              ELSE                                                      
                 IF SEXOI IS NOT EQUAL TO ('F' AND 'M' AND 'O')         
                    MOVE -1 TO SEXOL                                    
                    SET CLIENTENOOK TO TRUE                             
                    MOVE CT-MNS-07  TO MSGO                             
                 END-IF                                                 
              END-IF                                                    
           END-IF.                                                      
                                                                        
       3600-F-CONTINUA-VAL. EXIT.                                       
                                                                        
       3700-I-FECHA.                                                    
                                                                        
           SET FECHAOK TO TRUE.                                         
                                                                        
           MOVE ANIOI TO WS-ANIO                                        
           MOVE MESI  TO WS-MES                                         
           MOVE DIAI  TO WS-DIA                                         
                                                                        
           IF WS-ANIO IS NOT NUMERIC OR                                 
              WS-MES  IS NOT NUMERIC OR                                 
              WS-DIA  IS NOT NUMERIC                                    
                 SET FECHANOOK TO TRUE                                  
           END-IF.                                                      
                                                                        
           IF FECHAOK                                                   
                 IF WS-ANIO < 1950 OR WS-ANIO > 2020                    
                    SET FECHANOOK TO TRUE                               
                 END-IF                                                 
                                                                        
                 IF WS-MES < 00 OR WS-MES > 13                          
                    SET FECHANOOK TO TRUE                               
                 END-IF                                                 
                                                                        
                 IF WS-MES = 02                                         
                  IF WS-DIA > 28                                        
                     SET FECHANOOK TO TRUE                              
                  END-IF                                                
                 END-IF                                                 
                                                                        
                 IF WS-MES IS EQUAL TO (4 OR 6 OR 9 OR 11) AND          
                  WS-DIA > 30                                           
                     SET FECHANOOK TO TRUE                              
                 END-IF                                                 
                 IF WS-MES IS EQUAL TO                                  
                   (1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12) AND              
                   WS-DIA > 31                                          
                     SET FECHANOOK TO TRUE                              
                 END-IF                                                 
                                                                        
           END-IF.                                                      
                                                                        
       3700-F-FECHA. EXIT.                                              
                                                                        
       4000-I-WRITE.                                                    
                                                                        
           MOVE SPACES TO REG-PERSONA.                                  
           MOVE TIPDOCI      TO PER-TIP-DOC.                            
           MOVE NUMDOCI      TO PER-NRO-DOC.                            
           MOVE ZEROS        TO PER-CLI-NRO.                            
           MOVE NOMAPEI      TO PER-NOMAPE.                             
           MOVE WS-FECHA-VAL TO PER-CLI-AAAAMMDD.                       
           MOVE SPACES       TO PER-DIRECCION.                          
           MOVE SPACES       TO PER-LOCALIDAD.                          
           MOVE SPACES       TO PER-EMAIL.                              
           MOVE SPACES       TO PER-TELEFONO.                           
           MOVE SEXOI        TO PER-SEXO.                               
                                                                        
                                                                        
                                                                        
           EXEC CICS WRITE                                              
              FILE      (CT-DATASET)                                    
              FROM      (REG-PERSONA)                                   
              RIDFLD    (WS-USER-DATA)                                  
              LENGTH    (CT-DATASET-LEN)                                
              KEYLENGTH (CT-DATASET-KEYLEN)                             
              RESP      (WS-RESP)                                       
           END-EXEC.                                                    
                                                                        
           EVALUATE WS-RESP                                             
                WHEN DFHRESP(DUPREC)                                    
                   MOVE CT-MNS-02  TO MSGO                              
                WHEN DFHRESP(NORMAL)                                    
                   MOVE CT-MNS-08  TO MSGO                              
                WHEN OTHER                                              
                   MOVE CT-MNS-09  TO MSGO                              
           END-EVALUATE.                                                
                                                                        
           PERFORM 5000-I-SEND THRU 5000-F-SEND.                        
                                                                        
       4000-F-WRITE. EXIT.                                              
                                                                        
       5000-I-SEND.                                                     
                                                                        
           PERFORM    7000-I-TIME    THRU 7000-F-TIME.                  
      *    MOVE       WS-USER-TIPDOC  TO  TIPDOCO.                      
      *    MOVE       WS-USER-NRODOC  TO  NUMDOCO.                      
                                                                        
           EXEC CICS                                                    
               SEND MAP    (WS-MAP-00)                                  
               MAPSET (WS-MAPSET-00)                                    
               FROM   (MAP3D1FO)                                        
               LENGTH (WS-LONG)                                         
               ERASE                                                    
           END-EXEC.                                                    
                                                                        
                                                                        
       5000-F-SEND. EXIT.                                               

       7000-I-TIME.                                                     
                                                                        
           EXEC CICS ASKTIME                                            
             ABSTIME (WS-ABSTIME)                                       
           END-EXEC.                                                    
                                                                        
           EXEC CICS FORMATTIME                                         
             ABSTIME (WS-ABSTIME)                                       
             DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)                   
             TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)                        
           END-EXEC.                                                    
                                                                        
           MOVE WS-FECHA TO FECHAO.                                     
                                                                        
       7000-F-TIME. EXIT.                                               
                                                                        
       9000-I-PF12.                                                     
                                                                        
           EXEC CICS SEND CONTROL                                       
              ERASE                                                     
           END-EXEC.                                                    
                                                                        
           EXEC CICS XCTL                                               
              PROGRAM ('PGMMED1F')                                      
           END-EXEC.                                                    

           EXEC CICS                                                    
              RETURN                                                    
           END-EXEC.                                                    
                                                                        
       9000-F-PF12. EXIT.                                               
                                                                        
       9999-I-FINAL.                                                    
                                                                        
           EXEC CICS                                                    
             RETURN                                                     
             TRANSID  ('DD1F')                                          
             COMMAREA (WS-COMMAREA)                                     
           END-EXEC.                                                    
                                                                        
       9999-F-FINAL. EXIT.                                              
                                                                                                                                                                                                                                        