         TITLE 'FILEA - MAP FOR OPERATOR INSTRUCTIONS - COBOL'          
MAP2CAB  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
               LANG=COBOL,TIOAPFX=YES,COLOR=BLUE                        
                                                                        
MAP2CAB  DFHMDI SIZE=(24,80)                                            
         DFHMDF POS=(3,10),LENGTH=13,INITIAL='MENÚ CLIENTES',          *
               HILIGHT=UNDERLINE                                        
         DFHMDF POS=(3,24),LENGTH=1,ATTRB=PROT                          
                                                                        
         DFHMDF POS=(03,50),LENGTH=12,INITIAL='BCAB-MAP2CAB'            
                                                                        
         DFHMDF POS=(04,10),LENGTH=18,INITIAL='SELECCIONAR OPCIÓN'      
                                                                        
FECHA    DFHMDF POS=(04,50),LENGTH=10,INITIAL='99-99-9999'              
                                                                        
                                                                        
         DFHMDF POS=(09,16),LENGTH=18,INITIAL='TIPO DE DOCUMENTO:'      
TIPDOC   DFHMDF POS=(09,35),LENGTH=02,PICIN='X(02)',                   *
               COLOR=YELLOW,ATTRB=(IC,UNPROT,FSET)                      
         DFHMDF POS=(09,39),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(11,16),LENGTH=20,INITIAL='NUMERO DE DOCUMENTO:'    
NUMDOC   DFHMDF POS=(11,37),LENGTH=11,PICIN='9(11)',PICOUT='Z(11)',    *
               COLOR=YELLOW,ATTRB=(UNPROT,NUM,FSET)                     
         DFHMDF POS=(11,49),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
MSG      DFHMDF POS=(21,04),LENGTH=72,ATTRB=PROT,COLOR=RED,            *
               PICOUT='X(72)',HILIGHT=UNDERLINE                         
         DFHMDF POS=(21,77),LENGTH=01,ATTRB=(ASKIP,PROT)                
         DFHMDF POS=(23,13),LENGTH=08,INITIAL='PF1:ALTA'                
         DFHMDF POS=(23,24),LENGTH=08,INITIAL='PF2:BAJA'                
         DFHMDF POS=(23,37),LENGTH=16,INITIAL='PF3:MODIFICACION'        
         DFHMDF POS=(23,55),LENGTH=12,INITIAL='PF4:CONSULTA'            
         DFHMDF POS=(24,18),LENGTH=11,INITIAL='PF5:LIMPIAR'             
         DFHMDF POS=(24,30),LENGTH=20,INITIAL='PF6:CONSULTA GENERAL'    
         DFHMDF POS=(24,57),LENGTH=10,INITIAL='PF12:SALIR'              
         DFHMSD TYPE=FINAL                                              
         END                                                                     