       01  MAP2CABI.                                                    
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
           02  MSGL    COMP  PIC  S9(4).                                
           02  MSGF    PICTURE X.                                       
           02  FILLER REDEFINES MSGF.                                   
             03 MSGA    PICTURE X.                                      
           02  MSGI  PIC X(72).                                         
       01  MAP2CABO REDEFINES MAP2CABI.                                 
           02  FILLER PIC X(12).                                        
           02  FILLER PICTURE X(3).                                     
           02  FECHAO  PIC X(10).                                       
           02  FILLER PICTURE X(3).                                     
           02  TIPDOCO  PIC X(2).                                       
           02  FILLER PICTURE X(3).                                     
           02  NUMDOCO PIC Z(11).                                       
           02  FILLER PICTURE X(3).                                     
           02  MSGO PIC X(72).                                          