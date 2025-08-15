         TITLE 'FILEA - MAP FOR OPERATOR INSTRUCTIONS - COBOL'          
MAP1D1F  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
               LANG=COBOL,TIOAPFX=YES,COLOR=BLUE                        
                                                                        
MAP1D1F  DFHMDI SIZE=(24,80)                                            
         DFHMDF POS=(1,07),LENGTH=26,INITIAL='CONSULTA DATOS DEL CLIENT*
               E',HILIGHT=UNDERLINE                                     
         DFHMDF POS=(1,34),LENGTH=1,ATTRB=PROT                          
                                                                        
TXXX     DFHMDF POS=(01,60),LENGTH=12,INITIAL='AD1F-MAP1D1F'            
                                                                        
FECHA1   DFHMDF POS=(02,60),LENGTH=10,ATTRB=PROT,INITIAL='99-99-9999'   
                                                                        
         DFHMDF POS=(05,09),LENGTH=20,INITIAL='TIPO DE DOCUMENTO  :'    
TIPDO1   DFHMDF POS=(05,30),LENGTH=02,PICIN='X(02)',PICOUT='X(02)',    *
               COLOR=YELLOW,ATTRB=(IC,UNPROT,FSET)                      
         DFHMDF POS=(05,99),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(07,09),LENGTH=20,INITIAL='NUMERO DE DOCUMENTO:'    
NUMDO1   DFHMDF POS=(07,30),LENGTH=11,PICIN='9(11)',PICOUT='Z(11)',    *
               COLOR=YELLOW,ATTRB=(UNPROT,NUM,FSET)                     
         DFHMDF POS=(07,43),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(09,11),LENGTH=18,INITIAL='NUMERO DE CLIENTE:'      
NROCLI   DFHMDF POS=(09,30),LENGTH=03,PICOUT='Z99',                    *
               HILIGHT=UNDERLINE,COLOR=TURQUOISE                        
         DFHMDF POS=(9,34),LENGTH=01,ATTRB=(ASKIP,PROT)                 
                                                                        
         DFHMDF POS=(11,11),LENGTH=18,INITIAL='NOMBRE Y APELLIDO:'      
NOMAPE   DFHMDF POS=(11,30),LENGTH=30,PICOUT='X(30)',                  *
               HILIGHT=UNDERLINE,COLOR=TURQUOISE                        
         DFHMDF POS=(11,61),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(13,11),LENGTH=18,INITIAL='DIRECCION        :'      
DIREC    DFHMDF POS=(13,30),LENGTH=30,PICOUT='X(30)',                  *
               HILIGHT=UNDERLINE,COLOR=TURQUOISE                        
         DFHMDF POS=(13,61),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(15,11),LENGTH=18,INITIAL='EMAIL            :'      
EMAIL    DFHMDF POS=(15,30),LENGTH=30,PICOUT='X(30)',                  *
               HILIGHT=UNDERLINE,COLOR=TURQUOISE                        
         DFHMDF POS=(15,61),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(17,11),LENGTH=18,INITIAL='TELEFONO         :'      
TEL      DFHMDF POS=(17,30),LENGTH=15,PICOUT='X(15)',                  *
               HILIGHT=UNDERLINE,COLOR=TURQUOISE                        
         DFHMDF POS=(17,46),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
MSG1     DFHMDF POS=(21,04),LENGTH=72,ATTRB=PROT,COLOR=RED,            *
               PICOUT='X(72)',HILIGHT=UNDERLINE                         
         DFHMDF POS=(21,77),LENGTH=01,ATTRB=(ASKIP,PROT)                
         DFHMDF POS=(23,13),LENGTH=12,INITIAL='ENTER:Buscar'            
         DFHMSD TYPE=FINAL                                              
         END                                                            