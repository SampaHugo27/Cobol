                                                                         
        IDENTIFICATION DIVISION.                                         
                                                                         
        PROGRAM-ID. VRB662S.                                             
                                                                         
        AUTHOR.     HUGO DA SILVA SAMPAIO.                                                   
                                                                         
       ******************************************************************
                                                                         
        ENVIRONMENT DIVISION.                                            
                                                                         
        CONFIGURATION SECTION.                                           
                                                                         
        SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.     
                                                                         
       ******************************************************************
                                                                         
        DATA DIVISION.                                                   
                                                                         
        FILE SECTION.                                                    
                                                                         
       ******************************************************************
                                                                         
        WORKING-STORAGE SECTION.                                         
        77 WS-CAMPOA            PIC 9(02)    VALUE 1.                    
        77 WS-CAMPOB            PIC 9(02)    VALUE 2.                    
        77 WS-CAMPOC            PIC 9(02)    VALUE 0.                    
        77 WS-SOMA              PIC 9(05)    VALUE 0.                    
        77 WS-QUANTIDADE        PIC 9(05)    VALUE 0.                    
        77 WS-MEDIA             PIC 9(05)V99 VALUE 0.                    
        77 WS-MEDIA-M           PIC 99,99.                               
       ***************************************************************** 
                                                                         
        PROCEDURE DIVISION.                                              
       * 
		                                                           
            DISPLAY 'SEQUENCIA DE FIBONACCI'.                
            DISPLAY WS-CAMPOA                                
            DISPLAY WS-CAMPOA                                
            DISPLAY WS-CAMPOB                                
            ADD 3                  TO WS-QUANTIDADE          
            ADD WS-CAMPOA          TO WS-SOMA                
            ADD WS-CAMPOA          TO WS-SOMA                
            ADD WS-CAMPOB          TO WS-SOMA                
            PERFORM UNTIL WS-CAMPOC GREATER 34               
                    COMPUTE WS-CAMPOC = WS-CAMPOA + WS-CAMPOB
                    IF WS-CAMPOA = 1                         
                       ADD 1       TO WS-CAMPOA              
                    END-IF                                   
                    MOVE WS-CAMPOA TO WS-CAMPOB              
                    MOVE WS-CAMPOC TO WS-CAMPOA              
                    DISPLAY WS-CAMPOC                        
                    ADD 1          TO WS-QUANTIDADE          
		                  ADD WS-CAMPOC  TO WS-SOMA                       
       *            IF WS-CAMPOC =                                  
            END-PERFORM.                                            
            DISPLAY 'ETC'.                                          
                                                                    
                                                                    
 ***********COMPUTE WS-SOMA = WS-CAMPOA + WS-CAMPOB + WS-CAMPOC - 1.
                                                                    
            COMPUTE WS-MEDIA = WS-SOMA / WS-QUANTIDADE.             
            MOVE WS-MEDIA TO WS-MEDIA-M.                            
                                                                    
            DISPLAY 'SOMATORIA :' WS-SOMA.                          
            DISPLAY 'QUANTIDADE:' WS-QUANTIDADE.                    
            DISPLAY 'MEDIA     :' WS-MEDIA-M.                       
                                                                    
            DISPLAY WS-CAMPOA.                                      
            DISPLAY WS-CAMPOB.                                      
            DISPLAY WS-CAMPOC. 
            STOP RUN.             
      
      
