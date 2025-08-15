         TITLE 'FILEA - MAP FOR OPERATOR INSTRUCTIONS - COBOL'          
MAP1CAB  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
               LANG=COBOL,TIOAPFX=YES,COLOR=BLUE                        
                                                                        
MAP1CAB  DFHMDI SIZE=(24,80)                                            
         DFHMDF POS=(3,03),LENGTH=30,                                  *
               INITIAL='PGMPRCAB-CONSULTA DE CLIENTES',                *
               HILIGHT=UNDERLINE                                        
         DFHMDF POS=(3,33),LENGTH=1,ATTRB=PROT                          
                                                                        
         DFHMDF POS=(03,64),LENGTH=12,INITIAL='ACAB-MAP1CAB'            
                                                                        
FECHA    DFHMDF POS=(04,64),LENGTH=10,INITIAL='99-99-9999'              
                                                                        
                                                                        
         DFHMDF POS=(07,03),LENGTH=18,INITIAL='TIPO DE DOCUMENTO:'      
TIPDOC   DFHMDF POS=(07,22),LENGTH=02,PICIN='X(02)',PICOUT='X(02)',    *
               COLOR=YELLOW,ATTRB=(IC,UNPROT,FSET)                      
         DFHMDF POS=(07,25),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(07,32),LENGTH=20,INITIAL='NUMERO DE DOCUMENTO:'    
NUMDOC   DFHMDF POS=(07,53),LENGTH=11,PICIN='9(11)',PICOUT='Z(11)',    *
               COLOR=YELLOW,ATTRB=(UNPROT,NUM,FSET)                     
         DFHMDF POS=(07,64),LENGTH=01,ATTRB=(ASKIP,PROT)                

         DFHMDF POS=(12,03),LENGTH=10,INITIAL='NRO. CLI.:'              
NROCLI   DFHMDF POS=(12,20),LENGTH=03,PICIN='9(03)',                   *
               COLOR=RED,ATTRB=(PROT,NUM,FSET)                          
         DFHMDF POS=(12,25),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(13,03),LENGTH=07,INITIAL='NOMBRE:'                 
NOMAPE   DFHMDF POS=(13,20),LENGTH=30,PICIN='X(30)',                   *
               COLOR=RED,ATTRB=(IC,PROT,FSET)                           
         DFHMDF POS=(13,50),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(14,03),LENGTH=10,INITIAL='DIRECCION:'              
CLIDIR   DFHMDF POS=(14,20),LENGTH=30,PICIN='X(30)',                   *
               COLOR=RED,ATTRB=(IC,PROT,FSET)                           
         DFHMDF POS=(14,50),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(15,03),LENGTH=06,INITIAL='EMAIL:'                  
CLIMAI   DFHMDF POS=(15,20),LENGTH=30,PICIN='X(30)',                   *
               COLOR=RED,ATTRB=(IC,PROT,FSET)                           
         DFHMDF POS=(15,50),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(16,03),LENGTH=09,INITIAL='TELEFONO:'               
CLITEL   DFHMDF POS=(16,20),LENGTH=15,PICIN='X(15)',                   *
               COLOR=RED,ATTRB=(IC,PROT,FSET)                           
         DFHMDF POS=(16,35),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(22,03),LENGTH=14,INITIAL='PRESIONE TECLA'          
         DFHMDF POS=(22,21),LENGTH=12,INITIAL='ENTER:BUSCAR'            
         DFHMDF POS=(22,36),LENGTH=10,INITIAL='F3:LIMPIAR'              
         DFHMDF POS=(22,50),LENGTH=12,INITIAL='F4:SIGUIENTE'            
         DFHMDF POS=(22,67),LENGTH=09,INITIAL='F12:SALIR'               
                                                                        
MSG      DFHMDF POS=(24,03),LENGTH=72,ATTRB=PROT,COLOR=RED,            *
               PICOUT='X(72)',HILIGHT=UNDERLINE                         
         DFHMDF POS=(24,77),LENGTH=01,ATTRB=(ASKIP,PROT)                
         DFHMSD TYPE=FINAL                                              
         END                                                                                                                                                                                                                          