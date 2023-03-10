		                                                                       
        IDENTIFICATION DIVISION.                                         
                                                                         
        PROGRAM-ID. VRB660S.                                             
                                                                         
        AUTHOR. HUGO DA SILVA SAMPAIO.                                                 
                                                                         
       ******************************************************************
                                                                         
        ENVIRONMENT DIVISION.                                            
                                                                         
        CONFIGURATION SECTION.                                           
                                                                         
        SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.     
                                                                         
       ******************************************************************
                                                                         
        DATA DIVISION.                                                   
		                                                                       
        FILE SECTION.                                                    
                                                                         
       ******************************************************************
                                                                         
        WORKING-STORAGE SECTION.                                         
        77 WS-CAMPOX            PIC X(04)    VALUE'LUIS'.                
        77 WS-CAMPOY            PIC X(04)    VALUE'LHPS'.                
        77 WS-CAMPO0            PIC 9(01)    VALUE 0.                    
        77 WS-CAMPO1            PIC 9(01)    VALUE 1.                    
        77 WS-CAMPO2            PIC 9(01)    VALUE 2.                    
        77 WS-CAMPO3            PIC 9(01)    VALUE 3.                    
        77 WS-CAMPO4            PIC 9(01)    VALUE 4.                    
        77 WS-CAMPO5            PIC 9(01)    VALUE 5.                    
        77 WS-CAMPO6            PIC 9(01)    VALUE 6.                    
        77 WS-CAMPO7            PIC 9(01)    VALUE 7.                    
        77 WS-CAMPO8            PIC 9(01)    VALUE 8.                    
        77 WS-CAMPO9            PIC 9(01)    VALUE 9.
	77 WS-CAMPOIMPARES      PIC 9(02)    VALUE 0.                   
        77 WS-SOMAPARES         PIC 9(02)    VALUE ZEROS.               
        77 WS-VAR               PIC 9(02)    VALUE ZEROS.               
                                                                        
       *****************************************************************
                                                                        
        PROCEDURE DIVISION.                                             
       *                                                                
                                                                        
            DISPLAY 'HELLO WORLD!'.                                     
            DISPLAY WS-CAMPOX.                                          
            DISPLAY WS-CAMPOY.                                          
            MOVE 'ABCD' TO WS-CAMPOX.                                   
            DISPLAY WS-CAMPOX.                                          
                                                                        
                                                                        
            IF WS-CAMPO0 EQUAL 0 OR 2 OR 4 OR 6 OR 8                    
               ADD WS-CAMPO0 TO WS-SOMAPARES
	       DISPLAY WS-CAMPO0                         
            ELSE                                         
               ADD 1 TO WS-CAMPOIMPARES                  
               DISPLAY 'CAMPO0 NAO E PAR:' WS-CAMPO0     
            END-IF.                                      
                                                         
                                                         
            IF WS-CAMPO1 EQUAL 0 OR 2 OR 4 OR 6 OR 8     
               ADD WS-CAMPO1 TO WS-SOMAPARES             
               DISPLAY WS-CAMPO1                         
            ELSE                                         
               ADD 1 TO WS-CAMPOIMPARES                  
               DISPLAY 'CAMPO1 NAO E PAR:' WS-CAMPO1     
            END-IF.                                      
                                                         
            IF WS-CAMPO2 EQUAL 0 OR 2 OR 4 OR 6 OR 8     
               ADD WS-CAMPO2 TO WS-SOMAPARES             
               DISPLAY WS-CAMPO2  
	    ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO2 NAO E PAR:' WS-CAMPO2 
            END-IF.                                  
                                                     
            IF WS-CAMPO3 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO3 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO3                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO3 NAO E PAR:' WS-CAMPO3 
            END-IF.                                  
                                                     
            IF WS-CAMPO4 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO4 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO4                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES
	       DISPLAY 'CAMPO4 NAO E PAR:' WS-CAMPO4 
            END-IF.                                  
                                                     
            IF WS-CAMPO5 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO5 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO5                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO5 NAO E PAR:' WS-CAMPO5 
            END-IF.                                  
                                                     
            IF WS-CAMPO6 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO6 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO6                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO6 NAO E PAR:' WS-CAMPO6 
            END-IF.
		                                                   
            IF WS-CAMPO7 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO7 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO7                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO7 NAO E PAR:' WS-CAMPO7 
            END-IF.                                  
                                                     
            IF WS-CAMPO8 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
               ADD WS-CAMPO8 TO WS-SOMAPARES         
               DISPLAY WS-CAMPO8                     
            ELSE                                     
               ADD 1 TO WS-CAMPOIMPARES              
               DISPLAY 'CAMPO8 NAO E PAR:' WS-CAMPO8 
            END-IF.                                  
                                                     
            IF WS-CAMPO9 EQUAL 0 OR 2 OR 4 OR 6 OR 8 
	       ADD WS-CAMPO9 TO WS-SOMAPARES                        
               DISPLAY WS-CAMPO9                                    
            ELSE                                                    
               ADD 1 TO WS-CAMPOIMPARES                             
               DISPLAY 'CAMPO9 NAO E PAR:' WS-CAMPO9                
            END-IF.                                                 
               DISPLAY 'QUANTIDADE CAMPOS IMPARES:' WS-CAMPOIMPARES.
               DISPLAY 'SOMATORIA CAMPOS PARES:' WS-SOMAPARES       
                                                                    
            PERFORM VARYING WS-VAR FROM 1 BY 1                      
                    UNTIL   WS-VAR GREATER 10                       
               DISPLAY WS-VAR                                  
            END-PERFORM. 
                                           
            STOP RUN.        
      
      
