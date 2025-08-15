      ******************************************************************
       IDENTIFICATION DIVISION.                                         
      ******************************************************************
                                                                        
       PROGRAM-ID. PGMPRCAB.                                            
                                                                        
      *    AUTHOR.        MATIAS N. MAZZITELLI | KC03CAB                
      *    DATE-WRITTEN.  2025-AGOSTO-13                                
                                                                        
      *----------------------------------------------------------------*
      *     ACTIVIDAD CLASE SINCRONICA 36 | CONSULTA A VSAM EN CICS    *
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
             05 CT-MSG-DATOS      PIC X(72) VALUE                       
                                        'INGRESE LOS DATOS REQUERIDOS'. 
             05 CT-MSG-TIPDOC     PIC X(72) VALUE                       
                                          'TIPO DE DOCUMENTO INVALIDO'. 
             05 CT-MSG-NRODOC     PIC X(72) VALUE                       
                                        'NUMERO DE DOCUMENTO INVALIDO'. 
             05 CT-MSG-INVALID    PIC X(72) VALUE                       
                              'DATOS INGRESADOS INVALIDOS | REINGRESE'. 
             05 CT-MSG-TECLA      PIC X(72) VALUE     'TECLA INVALIDA'. 
             05 CT-MSG-NOTFOUND   PIC X(72) VALUE                       
                    'TIPO Y NÚMERO DOCUMENTO INEXISTENTES | REINGRESE'. 
             05 CT-MSG-FOUND      PIC X(72) VALUE 'CLIENTE ENCONTRADO'. 
             05 CT-MSG-ERRDATA    PIC X(72) VALUE                       
                                        'PROBLEMA CON ARCHIVO PERSONA'. 
             05 CT-MSG-OPCION     PIC X(72) VALUE                       
                                           'INGRESE LA OPCION DESEADA'. 
                                                                        
             05 CT-MSG-PF1        PIC X(72) VALUE     'ALTA EFECTUADA'. 
             05 CT-MSG-PF2        PIC X(72) VALUE     'BAJA REALIZADA'. 
             05 CT-MSG-PF3        PIC X(72) VALUE                       
                                              'MODIFICACION REALIZADA'. 
             05 CT-MSG-PF4        PIC X(72) VALUE 'CONSULTA REALIZADA'. 
             05 CT-MSG-PF12       PIC X(72) VALUE                       
                                                'FIN TRANSACCION ACAB'. 
                                                                        
          03 CT-DATASET           PIC X(08)  VALUE 'PERSONA'.           
          03 CT-DATASET-LEN       PIC S9(04) COMP VALUE 160.            
                                                                        
      *----------------------------------------------------------------*
      *              A R E A  D E  V A R I A B L E S                   *
      *----------------------------------------------------------------*
                                                                        
       01 WS-VARIABLES.                                                 
          03 WS-MAP               PIC X(07)       VALUE 'MAP1CAB'.      
          03 WS-MAPSET            PIC X(07)       VALUE 'MAP1CAB'.      
          03 WS-LONG              PIC S9(04) COMP.                      
          03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.             
          03 WS-FECHA             PIC X(10)       VALUE SPACES.         
          03 WS-SEP-DATE          PIC X           VALUE '/'.            
          03 WS-HORA              PIC X(08)       VALUE SPACES.         
          03 WS-SEP-HOUR          PIC X           VALUE ':'.            
          03 WS-RESP              PIC S9(04) COMP.                      
          03 WS-ERR               PIC X(20) VALUE SPACES.               
          03 WS-RESP-EDITED       PIC ZZZ9 VALUE ZEROS.                 
          03 WS-CONFIRMAR         PIC X VALUE 'Y'.                      
          03 WS-NORMAL            PIC X VALUE '*'.                      
          03 WS-ENTER             PIC X VALUE ' '.                      
                                                                        
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
                                                                        
       COPY MAP1CAB.                                                    
       COPY DFHBMSCA.                                                   
       COPY DFHAID.                                                     
       COPY CPPERSON.                                                   

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
                                                                        
      * READ DEL ARCHIVO VSAM PERSONA (CLIENTES)                        
                                                                        
           EXEC CICS                                                    
                READ DATASET (CT-DATASET)                               
                RIDFLD  (WS-USER-DATA)                                  
                INTO (REG-PERSONA)                                      
                LENGTH (CT-DATASET-LEN)                                 
                EQUAL                                                   
                RESP (WS-RESP)                                          
           END-EXEC.                                                    
                                                                        
       1000-F-INICIO.                                                   
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                     2 0 0 0 - P R O C E S O                    *
      *----------------------------------------------------------------*
                                                                        
       2000-PROCESO.                                                    
                                                                        
           EVALUATE WS-RESP                                             
             WHEN DFHRESP(NORMAL)                                       
               PERFORM 2200-MOVER-DATA                                  
                  THRU 2200-F-MOVER-DATA                                

             WHEN DFHRESP(NOTFND)                                       
               INITIALIZE MAP1CABO                                      
               MOVE WS-USER-TIPDOC     TO TIPDOCO                       
               MOVE WS-USER-NRODOC     TO NUMDOCO                       
               MOVE LENGTH OF MAP1CABO TO WS-LONG                       
               MOVE CT-MSG-NOTFOUND    TO  MSGO                         
                                                                        
             WHEN OTHER                                                 
               INITIALIZE MAP1CABO                                      
               MOVE WS-USER-TIPDOC     TO TIPDOCO                       
               MOVE WS-USER-NRODOC     TO NUMDOCO                       
               MOVE LENGTH OF MAP1CABO TO WS-LONG                       
               MOVE CT-MSG-ERRDATA     TO  MSGO                         
           END-EVALUATE.                                                
                                                                        
           PERFORM 1400-GET-TIME                                        
              THRU 1400-F-GET-TIME.                                     
                                                                        
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
                     FROM   (MAP1CABO)                                  
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
      *                  1 4 0 0 - G E T - T I M E                     *
      *----------------------------------------------------------------*
                                                                        
       1400-GET-TIME.                                                   
                                                                        
           EXEC CICS ASKTIME                                            
             ABSTIME (WS-ABSTIME)                                       
           END-EXEC.                                                    

           EXEC CICS FORMATTIME                                         
             ABSTIME (WS-ABSTIME)                                       
             DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)                   
             TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)                        
           END-EXEC.                                                    
                                                                        
           MOVE WS-FECHA TO FECHAO.                                     
                                                                        
       1400-F-GET-TIME.                                                 
           EXIT.                                                        
                                                                        
      *----------------------------------------------------------------*
      *                 2 2 0 0 - M O V E R - D A T A                  *
      *----------------------------------------------------------------*
                                                                        
       2200-MOVER-DATA.                                                 
                                                                        
           INITIALIZE MAP1CABO MAP1CABI.                                
                                                                        
           MOVE PER-TIP-DOC        TO  TIPDOCO.                         
           MOVE PER-NRO-DOC        TO  NUMDOCO.                         
           MOVE PER-CLI-NRO        TO  NROCLIO.                         
           MOVE PER-NOMAPE         TO  NOMAPEO.                         
           MOVE PER-DIRECCION      TO  CLIDIRO.                         
           MOVE PER-EMAIL          TO  CLIMAIO.                         
           MOVE PER-TELEFONO       TO  CLITELO.                         
           MOVE LENGTH OF MAP1CABO TO WS-LONG.                          
           MOVE CT-MSG-FOUND       TO  MSGO.                            
                                                                        
       2200-F-MOVER-DATA.                                               
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