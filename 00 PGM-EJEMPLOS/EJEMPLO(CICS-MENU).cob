       IDENTIFICATION DIVISION.                                        
       PROGRAM-ID. PGMMEN99.                                           
       DATA DIVISION.                                                  
       FILE SECTION.                                                   
       WORKING-STORAGE SECTION.                                        
                                                                       
       01 CT-CONSTANTES.                                               
          03 CT-MSGO.                                                  
             05 CT-MNS-01         PIC X(72) VALUE                      
                                  'TIPO DE DOCUMENTO INVALIDO'.        
             05 CT-MNS-02         PIC X(72) VALUE                      
                                  'NUMERO DE DOCUMENTO INVALIDO'.      
             05 CT-MNS-03         PIC X(72) VALUE 'TECLA INVALIDA'.    
             05 CT-MNS-04         PIC X(72) VALUE 'BAJA REALIZADA'.    
             05 CT-MNS-05         PIC X(72) VALUE 'ALTA EFECTUADA'.    
             05 CT-MNS-06         PIC X(72) VALUE                      
                                  'MODIFICACION REALIZADA'.            
             05 CT-MNS-07         PIC X(72) VALUE 'CONSULTA REALIZADA'.
             05 CT-MNS-EXIT       PIC X(72) VALUE                      
                                  'FIN TRANSACCION T699'.              
                                                                       
       01 WS-VARIABLES.                                                
          03 WS-MAP               PIC X(07)       VALUE 'MAP0299'.     
          03 WS-MAPSET            PIC X(07)       VALUE 'MAP0299'.     
          03 WS-LONG              PIC S9(04) COMP.                     
          03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.             
          03 WS-FECHA             PIC X(10)       VALUE SPACES.         
          03 WS-SEP-DATE          PIC X           VALUE '/'.            
          03 WS-HORA              PIC X(08)       VALUE SPACES.         
          03 WS-SEP-HOUR          PIC X           VALUE ':'.            
          03 WS-RESP              PIC S9(04) COMP.                      
          03 WS-COMMAREA          PIC X(20) VALUE SPACES.               
                                                                        
       COPY MAP0299.                                                    
       COPY DFHBMSCA.                                                   
       COPY DFHAID.                                                     
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(20).                                        
                                                                        
       PROCEDURE DIVISION.                                              
       MAIN-PROGRAM.                                                    
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             
           PERFORM 1000-I-INICIO THRU 1000-F-INICIO.                    
                                                                        
           PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO.                  
                                                                        
           PERFORM 9999-I-FINAL THRU 9999-F-FINAL.                      
                                                                        
       1000-I-INICIO.                                                   
                                                                        
           MOVE LOW-VALUES TO MAP0299I.                                 
                                                                        
      *    IF EIBCALEN = 0                                              
              PERFORM 1500-I-RECEIVE-MAP THRU 1500-F-RECEIVE-MAP        
      *    END-IF                                                       
                                                                        
           .                                                            
       1000-F-INICIO. EXIT.                                             
                                                                        
       1500-I-RECEIVE-MAP.                                              
                                                                        
           EXEC CICS                                                    
                RECEIVE MAP (WS-MAP)                                    
                        MAPSET (WS-MAPSET)                              
                        INTO (MAP0299I)                                 
                        RESP(WS-RESP)                                   
           END-EXEC                                                     
            .                                                           
                                                                        
       1500-F-RECEIVE-MAP. EXIT.                                        
                                                                        
       2000-I-PROCESO.                                                  
                                                                        
           EVALUATE WS-RESP                                             
             WHEN DFHRESP (NORMAL)                                      
               MOVE LENGTH OF MAP0299O TO WS-LONG                       
               PERFORM 2500-I-PULSAR-TECLA THRU 2500-F-PULSAR-TECLA     
                                                                        
             WHEN DFHRESP (MAPFAIL)                                     
               MOVE LENGTH OF MAP0299O TO WS-LONG                       
               MOVE LOW-VALUES TO MAP0299O                              
               MOVE 'INGRESE LA OPCION DESEADA'  TO MSGO                
                                                                        
             WHEN OTHER                                                 
               INITIALIZE MAP0299O                                      
               MOVE 'INGRESO DE DATOS' TO MSGO                          
                                                                        
           END-EVALUATE.                                                
                                                                        
           PERFORM 9500-I-SENDMAP THRU 9500-F-SENDMAP.                  
                                                                        
       2000-F-PROCESO. EXIT.                                            
                                                                        
       2500-I-PULSAR-TECLA.                                             
                                                                        
             EVALUATE EIBAID                                            
                                                                        
             WHEN DFHPF1                                                
                 PERFORM 3000-I-PF1 THRU 3000-F-PF1                     
                                                                        
             WHEN DFHPF2                                                
                 PERFORM 3500-I-PF2 THRU 3500-F-PF2                     

             WHEN DFHPF3                                                
                 PERFORM 4000-I-PF3 THRU 4000-F-PF3                     
                                                                        
             WHEN DFHPF4                                                
                 PERFORM 4500-I-PF4 THRU 4500-F-PF4                     
                                                                        
             WHEN DFHPF5                                                
                 PERFORM 4600-I-PF5 THRU 4600-F-PF5                     
                                                                        
             WHEN DFHPF12                                               
                 PERFORM 5500-I-PF12 THRU 5500-F-PF12                   
                                                                        
             WHEN OTHER                                                 
                 MOVE CT-MNS-03 TO MSGO                                 
                                                                        
             END-EVALUATE.                                              
                                                                        
       2500-F-PULSAR-TECLA. EXIT.                                       
                                                                        
       3000-I-PF1.                                                      
                                                                        
           MOVE CT-MNS-05 TO MSGO.                                      
                                                                        
       3000-F-PF1. EXIT.                                                
                                                                        
       3500-I-PF2.                                                      
                                                                        
           MOVE CT-MNS-04 TO MSGO.                                      
                                                                        
       3500-F-PF2. EXIT.                                                
                                                                        
       4000-I-PF3.                                                      
                                                                        
           MOVE CT-MNS-06 TO MSGO.                                      
                                                                        
       4000-F-PF3. EXIT.                                                
                                                                        
       4500-I-PF4.                                                      
                                                                        
           MOVE 'CONSULTA REALIZADA' TO MSGO.                           
                                                                        
      *    EXEC CICS XCTL                                               
      *                                                                 
      *        PROGRAM('PGMCON99')                                      
      *                                                                 
      *    END-EXEC.                                                    
                                                                        
       4500-F-PF4. EXIT.                                                
                                                                        
       4600-I-PF5.                                                      
                                                                        
           MOVE LOW-VALUES TO MAP0299O.                                 
                                                                        
       4600-F-PF5. EXIT.                                                
                                                                        
       5500-I-PF12.                                                     
                                                                        
           EXEC CICS                                                    
                SEND CONTROL ERASE                                      
           END-EXEC                                                     
                                                                        
           EXEC CICS                                                    
                SEND TEXT                                               
                     FROM (CT-MNS-EXIT)                                 
           END-EXEC                                                     
                                                                        
           EXEC CICS                                                    
                RETURN                                                  
           END-EXEC.                                                    
                                                                        
       5500-F-PF12. EXIT.                                               
                                                                        
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
                                                                        
       9500-I-SENDMAP.                                                  
                                                                        
           PERFORM 7000-I-TIME THRU 7000-F-TIME                         
           EXEC CICS                                                    
                SEND MAP    (WS-MAP)                                    
                     MAPSET (WS-MAPSET)                                 
                     FROM   (MAP0299O)                                  
                     LENGTH (WS-LONG)                                   
                     ERASE                                              
                     FREEKB                                             
           END-EXEC.                                                    
           EXEC CICS                                                    
                RETURN                                                  
                TRANSID  ('T699')                                       
           END-EXEC.                                                    
       9500-F-SENDMAP. EXIT.                                            
                                                                        
       9999-I-FINAL.                                                    
                                                                        
           EXEC CICS                                                    
             RETURN                                                     
           END-EXEC.                                                    
                                                                        
       9999-F-FINAL. EXIT.                                                                                                                                                          