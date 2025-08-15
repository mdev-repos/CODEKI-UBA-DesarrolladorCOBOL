      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID. PGMMECAB.                                            
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-AGOSTO-09                                
                                                                        
      *----------------------------------------------------------------*
      *     ACTIVIDAD CLASE SINCRONICA 36 | MENU CICS                  *
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
       DATA DIVISION.                                                   
      ******************************************************************
                                                                        
      *----------------------------------------------------------------*
       FILE SECTION.                                                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.                                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *              A R E A  D E  C O N S T A N T E S                 *
      *----------------------------------------------------------------*
                                                                        
       01 CT-CONSTANTES.                                                
          03 CT-MSGO.                                                   
             05 CT-MSG-TIPDOC     PIC X(72) VALUE                       
                                  'TIPO DE DOCUMENTO INVALIDO'.         
             05 CT-MSG-NRODOC     PIC X(72) VALUE                       
                                  'NUMERO DE DOCUMENTO INVALIDO'.       
             05 CT-MSG-TECLA      PIC X(72) VALUE 'TECLA INVALIDA'.     
             05 CT-MSG-OPCION     PIC X(72) VALUE                       
                                  'INGRESE LA OPCION DESEADA'.          
             05 CT-MSG-DATOS      PIC X(72) VALUE                       
                                  'INGRESE LOS DATOS REQUERIDOS'.       
             05 CT-MSG-PF1        PIC X(72) VALUE 'ALTA EFECTUADA'.     
             05 CT-MSG-PF2        PIC X(72) VALUE 'BAJA REALIZADA'.     
             05 CT-MSG-PF3        PIC X(72) VALUE                       
                                  'MODIFICACION REALIZADA'.             
             05 CT-MSG-PF4        PIC X(72) VALUE 'CONSULTA REALIZADA'. 
             05 CT-MSG-PF12       PIC X(72) VALUE                       
                                  'FIN TRANSACCION BCAB'.               
                                                                        
      *----------------------------------------------------------------*
      *              A R E A  D E  V A R I A B L E S                   *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
          03 WS-MAP               PIC X(07)       VALUE 'MAP2CAB'.      
          03 WS-MAPSET            PIC X(07)       VALUE 'MAP2CAB'.      
          03 WS-LONG              PIC S9(04) COMP.                      
          03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.             
          03 WS-FECHA             PIC X(10)       VALUE SPACES.         
          03 WS-SEP-DATE          PIC X           VALUE '/'.            
          03 WS-HORA              PIC X(08)       VALUE SPACES.         
          03 WS-SEP-HOUR          PIC X           VALUE ':'.            
          03 WS-RESP              PIC S9(04) COMP.                      
          03 WS-ERR               PIC X(20) VALUE SPACES.               
          03 WS-RESP-EDITED       PIC ZZZ9 VALUE ZEROS.                 

      *----------------------------------------------------------------*
      *            D E F I N I C I O N  D E  C O M M A R E A           *
      *----------------------------------------------------------------*
                                                                        
       01 WS-COMMAREA.                                                  
          03 WS-USER-DATA.                                              
             05 WS-USER-TIPDOC    PIC X(02).                            
             05 WS-USER-NRODOC    PIC 9(11).                            
                                                                        
          03 WS-TIP-DOC           PIC X(02).                            
             88 WS-TIP-DOC-BOOLEAN                   VALUE 'DU'         
                                                           'PA'         
                                                           'PE'.        
          03 FILLER               PIC X(5).                             
                                                                                                                                                
      *----------------------------------------------------------------*
      *                   A R E A  D E  C O P Y S                      *
      *----------------------------------------------------------------*
                                                                        
       COPY MAP2CAB.                                                    
       COPY DFHBMSCA.                                                   
       COPY DFHAID.                                                     
                                                                        
      *----------------------------------------------------------------*
      *    D E F I N I C I O N  D E  L I N K A G E  S E C T I O N      *
      *----------------------------------------------------------------*
                                                                        
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(20).                                        
                                                                        
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.                                              
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIO                                          
              THRU 1000-F-INICIO.                                       
                                                                        
           PERFORM 2000-PROCESO                                         
              THRU 2000-F-PROCESO.                                      
                                                                        
      *----------------------------------------------------------------*
      *                     1 0 0 0 - I N I C I O                      *
      *----------------------------------------------------------------*
                                                                        
       1000-INICIO.                                                     
                                                                        
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             
                                                                        
           INITIALIZE WS-RESP.                                          
                                                                        
           IF EIBCALEN = 0                                              
              MOVE LOW-VALUES TO MAP2CABO                               
              MOVE CT-MSG-OPCION TO MSGO                                
                                                                        
              PERFORM 1600-GET-TIME                                     
                 THRU 1600-F-GET-TIME                                   
                                                                        
              PERFORM 1200-SEND-MAP                                     
                 THRU 1200-F-SEND-MAP                                   
           ELSE                                                         
              PERFORM 1400-RECEIVE-MAP                                  
                 THRU 1400-F-RECEIVE-MAP                                
           END-IF.                                                      
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                     2 0 0 0 - P R O C E S O                    *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           EVALUATE WS-RESP                                             
             WHEN DFHRESP(NORMAL)                                       
               PERFORM 2200-PULSAR-TECLA                                
                  THRU 2200-F-PULSAR-TECLA                              

             WHEN DFHRESP(MAPFAIL)                                      
               MOVE LENGTH OF MAP2CABO TO WS-LONG                       
               MOVE LOW-VALUES TO MAP2CABO                              
               MOVE CT-MSG-OPCION  TO MSGO                              
                                                                        
             WHEN OTHER                                                 
               INITIALIZE MAP2CABO                                      
               MOVE CT-MSG-DATOS TO MSGO                                
           END-EVALUATE.                                                
                                                                        
           PERFORM 1600-GET-TIME                                        
              THRU 1600-F-GET-TIME.                                     
                                                                        
           PERFORM 1200-SEND-MAP                                        
              THRU 1200-F-SEND-MAP.                                     
                                                                        
       2000-F-PROCESO.                                                  
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *             M O D U L O S | S U B - P R O C E S O S            *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                    1 2 0 0 - S E N D - M A P                   *
      *----------------------------------------------------------------*

       1200-SEND-MAP.                                                   
                                                                        
           EXEC CICS                                                    
                SEND MAP    (WS-MAP)                                    
                     MAPSET (WS-MAPSET)                                 
                     FROM   (MAP2CABO)                                  
                     LENGTH (WS-LONG)                                   
                     ERASE                                              
                     FREEKB                                             
                     RESP   (WS-RESP)                                   
           END-EXEC.                                                    
                                                                        
           EVALUATE WS-RESP                                             
              WHEN DFHRESP(NORMAL)                                      
                 EXEC CICS                                              
                    RETURN                                              
                    TRANSID  ('BCAB')                                   
                    COMMAREA (WS-COMMAREA)                              
                 END-EXEC                                               
                                                                        
              WHEN OTHER                                                
                 MOVE SPACES TO WS-ERR                                  
                 MOVE WS-RESP TO WS-RESP-EDITED                         
                 STRING 'ERROR ENVIO MAPA COD: ' DELIMITED BY SIZE      
                                  WS-RESP-EDITED DELIMITED BY SIZE      
                 INTO WS-ERR                                            
                 EXEC CICS                                              
                   SEND TEXT FROM (WS-ERR)                              
                   FREEKB                                               
                 END-EXEC                                               
                 PERFORM 3000-FIN                                       
                    THRU 3000-F-FIN                                     
           END-EVALUATE.                                                
                                                                        
       1200-F-SEND-MAP.                                                 
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                1 4 0 0 - R E C E I V E - M A P                 *
      *----------------------------------------------------------------*
                                                                        
       1400-RECEIVE-MAP.                                                
                                                                        
           EXEC CICS                                                    
                RECEIVE MAP (WS-MAP)                                    
                        MAPSET (WS-MAPSET)                              
                        INTO (MAP2CABI)                                 
                        RESP(WS-RESP)                                   
           END-EXEC.                                                    
                                                                        
           EVALUATE WS-RESP                                             
             WHEN DFHRESP(NORMAL)                                       
                CONTINUE                                                
                                                                        
             WHEN DFHRESP(MAPFAIL)                                      
                MOVE LOW-VALUES TO MAP2CABI                             
                MOVE CT-MSG-DATOS TO MSGO                               
                                                                        
             WHEN OTHER                                                 
                MOVE SPACES TO WS-ERR                                   
                MOVE WS-RESP TO WS-RESP-EDITED                          
                STRING 'ERROR RECIBIENDO MAPA COD: ' DELIMITED BY SIZE  
                                      WS-RESP-EDITED DELIMITED BY SIZE  
                INTO WS-ERR                                             
                EXEC CICS                                               
                  SEND TEXT FROM(WS-ERR)                                
                  FREEKB                                                
                END-EXEC                                                
                PERFORM 3000-FIN                                        
                   THRU 3000-F-FIN                                      
           END-EVALUATE.                                                
                                                                        
       1400-F-RECEIVE-MAP.                                              
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                  1 6 0 0 - G E T - T I M E                     *
      *----------------------------------------------------------------*
                                                                        
       1600-GET-TIME.                                                   
                                                                        
           EXEC CICS ASKTIME                                            
             ABSTIME (WS-ABSTIME)                                       
           END-EXEC.                                                    
                                                                        
           EXEC CICS FORMATTIME                                         
             ABSTIME (WS-ABSTIME)                                       
             DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)                   
             TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)                        
           END-EXEC.                                                    
                                                                        
           MOVE WS-FECHA TO FECHAO.                                     
                                                                        
       1600-F-GET-TIME.                                                 
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *               2 2 0 0 - P U L S A R - T E C L A                *
      *----------------------------------------------------------------*
                                                                        
       2200-PULSAR-TECLA.                                               
                                                                        
           EVALUATE EIBAID                                              
             WHEN DFHENTER                                              
               MOVE LENGTH OF MAP2CABO TO WS-LONG                       
               MOVE LOW-VALUES TO MAP2CABO                              
               MOVE CT-MSG-OPCION TO MSGO                               
                                                                        
             WHEN DFHPF1                                                
               PERFORM 2210-PF1                                         
                  THRU 2210-F-PF1                                       
                                                                        
             WHEN DFHPF2                                                
               PERFORM 2220-PF2                                         
                  THRU 2220-F-PF2                                       
                                                                        
             WHEN DFHPF3                                                
               PERFORM 2230-PF3                                         
                  THRU 2230-F-PF3                                       
                                                                        
             WHEN DFHPF4                                                
               PERFORM 2240-PF4                                         
                  THRU 2240-F-PF4                                       
                                                                        
             WHEN DFHPF5                                                
               PERFORM 2250-PF5                                         
                  THRU 2250-F-PF5                                       
                                                                        
             WHEN DFHPF12                                               
               PERFORM 2320-PF12                                        
                  THRU 2320-F-PF12                                      
                                                                        
             WHEN OTHER                                                 
               MOVE CT-MSG-TECLA TO MSGO                                
           END-EVALUATE.                                                
                                                                        
       2200-F-PULSAR-TECLA.                                             
             EXIT.                                                      
                                                                        
      *----------------------------------------------------------------*
      *                         2 2 1 0 - P F 1                        *
      *----------------------------------------------------------------*
                                                                        
       2210-PF1.                                                        
                                                                        
      *    HACER XCTL A LA TRANSACCIóN DCAB. PROGRAMA PGMALCAB          
                                                                        
           MOVE CT-MSG-PF1 TO MSGO.                                     
                                                                        
       2210-F-PF1.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                         2 2 2 0 - P F 2                        *
      *----------------------------------------------------------------*

       2220-PF2.                                                        
                                                                        
      *    HACER XCTL A LA TRANSACCIóN ECAB. PROGRAMA PGMBACAB.         
                                                                        
           MOVE CT-MSG-PF2 TO MSGO.                                     
                                                                        
       2220-F-PF2.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                         2 2 3 0 - P F 3                        *
      *----------------------------------------------------------------*
                                                                        
       2230-PF3.                                                        
                                                                        
      *    HACER XCTL A LA TRANSACCIóN FCAB. PROGRAMA PGMMOCAB.         
                                                                        
           MOVE CT-MSG-PF3 TO MSGO.                                     
                                                                        
       2230-F-PF3.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                         2 2 4 0 - P F 4                        *
      *----------------------------------------------------------------*
       
       2240-PF4.                                                        
                                                                        
      *    PF4 --> TECLA DE CONSULTA DE CLIENTE                         
                                                                        
      *    HACER XCTL A LA TRANSACCIóN ACAB. PROGRAMA PGMPRCAB.         
                                                                        
           MOVE TIPDOCI TO WS-USER-TIPDOC.                              
                                                                        
           MOVE NUMDOCI TO WS-USER-NRODOC.                              
                                                                        
           MOVE TIPDOCI TO WS-TIP-DOC.                                  
                                                                        
           EXEC CICS XCTL                                               
               PROGRAM('PGMPRCAB')                                      
               COMMAREA(WS-COMMAREA)                                    
           END-EXEC.                                                    
                                                                                                                                               
       2240-F-PF4.                                                      
           EXIT.                                                        
                                                                                                                          
      *----------------------------------------------------------------*
      *                         2 2 5 0 - P F 5                        *
      *----------------------------------------------------------------*
                                                                        
       2250-PF5.                                                        
                                                                        
           MOVE LOW-VALUES TO MAP2CABO.                                 
                                                                        
       2250-F-PF5.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                         2 3 2 0 - P F 1 2                      *
      *----------------------------------------------------------------*
                                                                        
       2320-PF12.                                                       
                                                                        
           EXEC CICS                                                    
                SEND CONTROL ERASE                                      
           END-EXEC.                                                    
                                                                        
           EXEC CICS                                                    
                SEND TEXT                                               
                     FROM (CT-MSG-PF12)                                 
           END-EXEC.                                                    
                                                                        
           PERFORM 3000-FIN                                             
              THRU 3000-F-FIN.                                          
                                                                        
       2320-F-PF12.                                                     
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                       3 0 0 0 - F I N                          *
      *----------------------------------------------------------------*
                                                                        
       3000-FIN.                                                        
                                                                        
           EXEC CICS                                                    
             RETURN                                                     
           END-EXEC.                                                    
                                                                        
       3000-F-FIN.                                                      
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *------------------ E N D  O F  P R O G R A M -------------------*
      *----------------------------------------------------------------*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   