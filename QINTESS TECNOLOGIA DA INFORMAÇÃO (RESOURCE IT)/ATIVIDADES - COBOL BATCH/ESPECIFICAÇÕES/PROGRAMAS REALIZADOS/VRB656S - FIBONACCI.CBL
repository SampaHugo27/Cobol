       *                                                                
        IDENTIFICATION DIVISION.                                        
        PROGRAM-ID. VRB656S.                                            
        AUTHOR.     HUGO DA SILVA SAMPAIO.                               
       *----------------------------------------------------------------
        ENVIRONMENT                         DIVISION.                   
       *----------------------------------------------------------------
        INPUT-OUTPUT SECTION.                                           
       *----------------------------------------------------------------
        FILE-CONTROL.                                                   
                                                                        
            SELECT ARQOUT   ASSIGN TO ARQOUT                            
            ORGANIZATION    IS SEQUENTIAL                               
            ACCESS MODE     IS SEQUENTIAL                               
            FILE STATUS     IS WS-FSTATUS.                              
       *----------------------------------------------------------------
        DATA                                DIVISION.                   
       *----------------------------------------------------------------
	FILE                                SECTION.                    
                                                                        
        FD  ARQOUT                                                      
            RECORD CONTAINS 04 CHARACTERS                               
            DATA RECORD     IS FIBONACCI                                
            RECORDING MODE  IS F.                                       
                                                                        
        01  FIBONACCI.                                                  
              05 RESULTADO                  PIC 9(04).                  
       *----------------------------------------------------------------
        WORKING-STORAGE                     SECTION.                    
                                                                        
        01 WS-FSTATUS                       PIC X(02) VALUE SPACES.     
                                                                        
        01 VEZES                            PIC 9(02) VALUE 15.         
                                                                        
        01 WS-A                             PIC 9(04) VALUE ZEROS.      
        01 WS-B                             PIC 9(04) VALUE ZEROS.
	01 WS-C                             PIC 9(04) VALUE ZEROS.      
                                                                        
        01 IND                              PIC 9(02) VALUE ZEROS.      
                                                                        
        01 DATA-ATUAL.                                                  
           05 ANO                           PIC 9(04) VALUE ZEROES.     
           05 MES                           PIC 9(02) VALUE ZEROES.     
           05 DIA                           PIC 9(02) VALUE ZEROES.     
       *----------------------------------------------------------------
        PROCEDURE DIVISION.                                             
       *----------------------------------------------------------------
       *                                                                
        0000-PRINCIPAL                      SECTION.                    
       *                                                                
       *----------------------------------------------------------------
       *                                                                
            PERFORM 1000-INICIAR            THRU 1000-99-FIM.           
            PERFORM 2000-PROCESSAR          THRU 2000-99-FIM.
	    PERFORM 3000-FINALIZAR          THRU 3000-99-FIM.           
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        0000-99-FIM.                        EXIT.                       
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        1000-INICIAR                        SECTION.                    
       *                                                                
       *----------------------------------------------------------------
       *                                                                
            OPEN OUTPUT ARQOUT.                                         
                                                                        
            MOVE 0          TO RESULTADO.                               
            WRITE FIBONACCI.                                            
                                                                        
            MOVE 1          TO WS-A.
		                                                                      
            ACCEPT DATA-ATUAL FROM DATE YYYYMMDD.                       
                                                                        
            DISPLAY 'DATA ATUAL ' DATA-ATUAL.                           
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        1000-99-FIM.                        EXIT.                       
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        2000-PROCESSAR                      SECTION.                    
       *                                                                
       *----------------------------------------------------------------
       *                                                                
            PERFORM VARYING IND FROM 1 BY 1                             
                    UNTIL   IND = VEZES                                 
                 COMPUTE WS-C = WS-A + WS-B 
		                                                                      
                 PERFORM 2100-GRAVA-DADO    THRU 2100-99-FIM            
                                                                        
                 MOVE WS-A  TO WS-B                                     
                 MOVE WS-C  TO WS-A                                     
            END-PERFORM.                                                
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        2000-99-FIM.                        EXIT.                       
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        2100-GRAVA-DADO                     SECTION.                    
       *                                                                
       *----------------------------------------------------------------
       *                                                                
            MOVE WS-C       TO RESULTADO.                               
	    WRITE FIBONACCI.                                            
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        2100-99-FIM.                        EXIT.                       
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        3000-FINALIZAR                      SECTION.                    
       *                                                                
       *----------------------------------------------------------------
       *                                                                
            CLOSE ARQOUT.                                               
            STOP RUN.                                                   
       *                                                                
       *----------------------------------------------------------------
       *                                                                
        3000-99-FIM.                        EXIT.
       *                                                                
       *----------------------------------------------------------------                                                                                                        

      
      
