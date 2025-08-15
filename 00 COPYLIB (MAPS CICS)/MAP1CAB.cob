       01  MAP1CABI.                                                    
           02  FILLER PIC X(12).                                        
           02  FECHAL    COMP  PIC  S9(4).                              
           02  FECHAF    PICTURE X.                                     
           02  FILLER REDEFINES FECHAF.                                 
             03 FECHAA    PICTURE X.                                    
           02  FECHAI  PIC X(10).                                       
           02  TIPDOCL    COMP  PIC  S9(4).                             
           02  TIPDOCF    PICTURE X.                                    
           02  FILLER REDEFINES TIPDOCF.                                
             03 TIPDOCA    PICTURE X.                                   
           02  TIPDOCI  PIC X(02).                                      
           02  NUMDOCL    COMP  PIC  S9(4).                             
           02  NUMDOCF    PICTURE X.                                    
           02  FILLER REDEFINES NUMDOCF.                                
             03 NUMDOCA    PICTURE X.                                   
           02  NUMDOCI  PIC 9(11).                                      
           02  NROCLIL    COMP  PIC  S9(4).                             
           02  NROCLIF    PICTURE X.                                    
           02  FILLER REDEFINES NROCLIF.                                
             03 NROCLIA    PICTURE X.                                   
           02  NROCLII  PIC 9(03).                                      
           02  NOMAPEL    COMP  PIC  S9(4).                             
           02  NOMAPEF    PICTURE X.                                    
           02  FILLER REDEFINES NOMAPEF.                                
             03 NOMAPEA    PICTURE X.                                   
           02  NOMAPEI  PIC X(30).                                      
           02  CLIDIRL    COMP  PIC  S9(4).                             
           02  CLIDIRF    PICTURE X.                                    
           02  FILLER REDEFINES CLIDIRF.                                
             03 CLIDIRA    PICTURE X.                                   
           02  CLIDIRI  PIC X(30).                                      
           02  CLIMAIL    COMP  PIC  S9(4).                             
           02  CLIMAIF    PICTURE X.                                    
           02  FILLER REDEFINES CLIMAIF.                                
             03 CLIMAIA    PICTURE X.                                   
           02  CLIMAII  PIC X(30).                                      
           02  CLITELL    COMP  PIC  S9(4).                             
           02  CLITELF    PICTURE X.                                    
           02  FILLER REDEFINES CLITELF.                                
             03 CLITELA    PICTURE X.                                   
           02  CLITELI  PIC X(15).                                      
           02  MSGL    COMP  PIC  S9(4).                                
           02  MSGF    PICTURE X.                                       
           02  FILLER REDEFINES MSGF.                                   
             03 MSGA    PICTURE X.                                      
           02  MSGI  PIC X(72).                                         
       01  MAP1CABO REDEFINES MAP1CABI.                                 
           02  FILLER PIC X(12).                                        
           02  FILLER PICTURE X(3).                                     
           02  FECHAO  PIC X(10).                                       
           02  FILLER PICTURE X(3).                                     
           02  TIPDOCO PIC X(02).                                       
           02  FILLER PICTURE X(3).                                     
           02  NUMDOCO PIC Z(11).                                       
           02  FILLER PICTURE X(3).                                     
           02  NROCLIO  PIC X(3).                                       
           02  FILLER PICTURE X(3).                                     
           02  NOMAPEO  PIC X(30).                                      
           02  FILLER PICTURE X(3).                                     
           02  CLIDIRO  PIC X(30).                                      
           02  FILLER PICTURE X(3).                                     
           02  CLIMAIO  PIC X(30).                                      
           02  FILLER PICTURE X(3).                                     
           02  CLITELO  PIC X(15).                                      
           02  FILLER PICTURE X(3).                                     
           02  MSGO PIC X(72).                                          