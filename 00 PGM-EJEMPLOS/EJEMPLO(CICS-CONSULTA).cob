       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PGMPRD1F.                                            
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       WORKING-STORAGE SECTION.                                         
       77 WS-INICIO       PIC X(20) VALUE 'INICIO DE LA WORKING'.       
       01 CT-CONSTANTES.                                                
          03 CT-MSGO.                                                   
             05 CT-MNS-01         PIC X(72) VALUE                       
                            'INGRESE LOS DATOS Y PRESIONE ENTER'.       
             05 CT-MNS-02         PIC X(72) VALUE                       
                            'DATOS INGRESADOS INCORRECTOS - REINGRESE'. 
             05 CT-MNS-03         PIC X(72) VALUE                       
                    'TIPO Y NÚMERO DOCUMENTO INEXISTENTES - REINGRESE'. 
             05 CT-MNS-04         PIC X(72) VALUE                       
                            'TIPO DE DOCUMENTO INVALIDO'.               
             05 CT-MNS-05         PIC X(72) VALUE                       
                            'NUMERO DE DOCUMENTO INVALIDO'.             
             05 CT-MNS-06         PIC X(72) VALUE 'CLIENTE ENCONTRADO'. 
             05 CT-MNS-08         PIC X(72) VALUE                       
                            'PROBLEMA CON ARCHIVO PERSONA'.             
             05 CT-MNS-09         PIC X(72) VALUE 'TECLA INVALIDA'.     
             05 CT-MNS-10         PIC X(72) VALUE 'ERROR SEND    '.     
             05 CT-MNS-EXIT       PIC X(72) VALUE                       
                            'FIN TRANSACCION BF1F'.                     
          03 CT-DATASET           PIC X(08)  VALUE 'PERSONA'.           
          03 CT-DATASET-LEN       PIC S9(04) COMP VALUE 160.            
                                                                        
       01 WS-VARIABLES.                                                 
      *   03 WS-MAP-00            PIC X(07)       VALUE 'MAP0099'.      
      *   03 WS-MAPSET-00         PIC X(07)       VALUE 'MAP0099'.      
          03 WS-MAP-01            PIC X(07)       VALUE 'MAP1D1F'.      
          03 WS-MAPSET-01         PIC X(07)       VALUE 'MAP1D1F'.      
          03 WS-LONG              PIC S9(04) COMP.                      
          03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.             
          03 WS-FECHA             PIC X(10)       VALUE SPACES.         
          03 WS-SEP-DATE          PIC X           VALUE '/'.            
          03 WS-HORA              PIC X(08)       VALUE SPACES.         
          03 WS-SEP-HOUR          PIC X           VALUE ':'.            
          03 WS-RESP              PIC S9(04) COMP.                      
          03 SW-CONFIRMAR         PIC X VALUE 'Y'.                      
          03 WS-NORMAL    PIC X VALUE '*'.                              
          03 WS-ENTER     PIC X VALUE ' '.                              
                                                                        
       01 WS-COMMAREA.                                                  
          03 WS-USER-DATA.                                              
             05 WS-USER-TIPDOC    PIC X(02).                            
             05 WS-USER-NRODOC    PIC 9(11).                            
          03 WS-TIP-DOC           PIC X(02).                            
             88 WS-TIP-DOC-BOOLEAN                   VALUE 'DU'         
                                                           'PA'         
                                                           'PE'.        
          03 FILLER               PIC X(5).                             
                                                                        
       COPY MAP1D1F.                                                    
       COPY DFHBMSCA.                                                   
       COPY DFHAID.                                                     
       COPY CPPERSON.                                                   
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
         01 DFHCOMMAREA PIC X(20).                                      
                                                                        
       PROCEDURE DIVISION.                                              
       MAIN-PROGRAM.                                                    
                                                                        
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             
           EXEC CICS                                                    
                READ DATASET (CT-DATASET)                               
                RIDFLD  (WS-USER-DATA)                                  
                INTO (REG-PERSONA)                                      
                LENGTH (CT-DATASET-LEN)                                 
                EQUAL                                                   
                RESP (WS-RESP)                                          
           END-EXEC.                                                    
           EVALUATE WS-RESP                                             
            WHEN DFHRESP(NORMAL)                                        
                 INITIALIZE MAP1D1FO MAP1D1FI                           
                 MOVE PER-TIP-DOC TO  TIPDO1O                           
                 MOVE PER-NRO-DOC TO  NUMDO1O                           
                 MOVE PER-CLI-NRO TO  NROCLIO                           
                 MOVE PER-NOMAPE  TO  NOMAPEO                           
                 MOVE PER-DIRECCION TO  DIRECO                          
                 MOVE PER-EMAIL TO  EMAILO                              
                 MOVE PER-TELEFONO TO  TELO                             
                 MOVE LENGTH OF MAP1D1FO TO WS-LONG                     
                 MOVE 'CLIENTE ENCONTRADO' TO  MSG1O                    
            WHEN DFHRESP(NOTFND)                                        
                 INITIALIZE MAP1D1FO                                    
                 MOVE WS-USER-TIPDOC TO TIPDO1O                         
                 MOVE WS-USER-NRODOC TO NUMDO1O                         
                 MOVE LENGTH OF MAP1D1FO TO WS-LONG                     
                 MOVE CT-MNS-03            TO  MSG1O                    
            WHEN OTHER                                                  
                 INITIALIZE MAP1D1FO                                    
                 MOVE WS-USER-TIPDOC TO TIPDO1O                         
                 MOVE WS-USER-NRODOC TO NUMDO1O                         
                 MOVE LENGTH OF MAP1D1FO TO WS-LONG                     
                 MOVE CT-MNS-08 TO  MSG1O                               
            END-EVALUATE.                                               
           EXEC CICS ASKTIME                                            
             ABSTIME (WS-ABSTIME)                                       
           END-EXEC.                                                    
           EXEC CICS FORMATTIME                                         
             ABSTIME (WS-ABSTIME)                                       
             DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)                   
             TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)                        
           END-EXEC.                                                    
           MOVE WS-FECHA TO FECHA1O.                                    
           MOVE 'AD1F-MAP1D1F' TO TXXXO.                                
           EXEC CICS                                                    
              SEND MAP (WS-MAP-01)                                      
              MAPSET (WS-MAPSET-01)                                     
              FROM   (MAP1D1FO)                                         
              LENGTH (WS-LONG)                                          
              RESP (WS-RESP)                                            
              ERASE                                                     
           END-EXEC.                                                    
           EVALUATE WS-RESP                                             
            WHEN DFHRESP(NORMAL)                                        
               CONTINUE                                                 
            WHEN OTHER                                                  
                 INITIALIZE MAP1D1FO                                    
                 MOVE CT-MNS-10 TO  MSG1O                               
                 EXEC CICS SEND TEXT FROM(MSG1O) END-EXEC               
            END-EVALUATE.                                               
           EXEC CICS                                                    
             RETURN                                                     
             TRANSID  ('BD1F')                                          
           END-EXEC.                                                    