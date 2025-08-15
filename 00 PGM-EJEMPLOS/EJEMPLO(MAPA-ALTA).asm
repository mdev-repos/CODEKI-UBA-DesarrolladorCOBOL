         TITLE 'ALTA CLIENTES'                                          
MAP3D1F  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
               LANG=COBOL,TIOAPFX=YES,COLOR=BLUE,TERM=3270-2            
                                                                        
MAP3D1F  DFHMDI SIZE=(24,80)                                            
         DFHMDF POS=(1,07),LENGTH=13,INITIAL='ALTA CLIENTES'            
         DFHMDF POS=(1,21),LENGTH=1,ATTRB=PROT                          
                                                                        
*********************************************************************** 
         DFHMDF POS=(01,63),LENGTH=12,INITIAL='DD1F-MAP3D1F',          *
               COLOR=BLUE,ATTRB=PROT                                    
                                                                        
FECHA    DFHMDF POS=(02,63),LENGTH=10,PICOUT='X(10)',                  *
               COLOR=BLUE,ATTRB=(ASKIP,PROT,FSET),INITIAL='99-99-9999'  
         DFHMDF POS=(05,74),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
*********************************************************************** 
                                                                        
         DFHMDF POS=(08,20),LENGTH=38,                                 *
               INITIAL='CARGAR LOS DATOS Y PRESIONAR "ENTER": '         
                                                                        
         DFHMDF POS=(10,25),LENGTH=15,INITIAL='TIPO DOCUMENTO:'         
TIPDOC   DFHMDF POS=(10,41),LENGTH=02,PICIN='X(02)',PICOUT='X(02)',    *
               COLOR=YELLOW,ATTRB=(IC,UNPROT,FSET),HILIGHT=UNDERLINE    
         DFHMDF POS=(10,44),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(11,25),LENGTH=15,INITIAL='NRO. DOCUMENTO:'         
NUMDOC   DFHMDF POS=(11,41),LENGTH=11,PICIN='9(11)',PICOUT='9(11)',    *
               COLOR=YELLOW,ATTRB=(UNPROT,NUM,FSET),HILIGHT=UNDERLINE   
         DFHMDF POS=(11,53),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
         DFHMDF POS=(13,25),LENGTH=18,INITIAL='NOMBRE Y APELLIDO:',    *
               ATTRB=(ASKIP,NORM)                                       
NOMAPE   DFHMDF POS=(13,44),LENGTH=30,PICOUT='X(30)',PICIN='X(30)',    *
               COLOR=NEUTRAL,ATTRB=(UNPROT,FSET)                        
         DFHMDF POS=(13,75),LENGTH=01,ATTRB=(ASKIP,PROT)                
                                                                        
************************************************************************
                                                                        
         DFHMDF POS=(14,25),LENGTH=18,INITIAL='FECHA NACIMIENTO:'       
                                                                        
DIA      DFHMDF POS=(14,44),LENGTH=02,PICOUT='Z(02)',PICIN='9(02)',    *
               COLOR=NEUTRAL,ATTRB=(UNPROT,NUM,FSET)                    
         DFHMDF POS=(14,47),LENGTH=01,ATTRB=(ASKIP,PROT),INITIAL='/'    
MES      DFHMDF POS=(14,49),LENGTH=02,PICOUT='Z(02)',PICIN='9(02)',    *
               COLOR=NEUTRAL,ATTRB=(UNPROT,NUM,FSET)                    
         DFHMDF POS=(14,52),LENGTH=01,ATTRB=(ASKIP,PROT),INITIAL='/'    
ANIO     DFHMDF POS=(14,54),LENGTH=04,PICOUT='Z(04)',PICIN='9(04)',    *
               COLOR=NEUTRAL,ATTRB=(UNPROT,NUM,FSET)                    
         DFHMDF POS=(14,59),LENGTH=01,ATTRB=(ASKIP,PROT)                

         DFHMDF POS=(14,61),LENGTH=10,INITIAL='DD/MM/AAAA'              
************************************************************************
                                                                        
         DFHMDF POS=(15,25),LENGTH=5,INITIAL='SEXO:'                    
SEXO     DFHMDF POS=(15,31),LENGTH=1,PICOUT='X',PICIN='X',             *
               COLOR=NEUTRAL,ATTRB=(UNPROT,FSET)                        
         DFHMDF POS=(15,33),LENGTH=01,ATTRB=(ASKIP,PROT)                
         DFHMDF POS=(15,35),LENGTH=7,INITIAL='(F;M;O)'                  
                                                                        
MSG      DFHMDF POS=(21,04),LENGTH=72,ATTRB=PROT,COLOR=RED,            *
               PICOUT='X(72)',HILIGHT=UNDERLINE                         
         DFHMDF POS=(21,77),LENGTH=01,ATTRB=(ASKIP,PROT)                
         DFHMDF POS=(24,05),LENGTH=17,INITIAL='ENTER:Seleccionar'       
         DFHMDF POS=(24,35),LENGTH=11,INITIAL='PF3:Limpiar'             
         DFHMDF POS=(24,65),LENGTH=10,INITIAL='PF12:Salir'              
         DFHMSD TYPE=FINAL                                              
         END                                                                                                                                             