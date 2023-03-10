      	*-----------------------------------------------------------------
        IDENTIFICATION               DIVISION.                           
        PROGRAM-ID. FIBTEST.                                             
        AUTHOR.     HUGO DA SILVA SAMPAIO.                                           
       *-----------------------------------------------------------------
        ENVIRONMENT                  DIVISION.                           
       *SOURCE                                                           
       *OBJECT                                                           
       *SPECIAL-NAMES                                                    
       *-----------------------------------------------------------------
        INPUT-OUTPUT                 SECTION.                            
        FILE-CONTROL.                                                    
       *                                                                 
              SELECT FIBINP ASSIGN TO FIBINP                             
              FILE STATUS IS WK-FS-INP.                                  
       *                                                                 
              SELECT FIBOUT ASSIGN TO FIBOUT                             
              FILE STATUS IS WK-FS-OUT.                                  
       *-----------------------------------------------------------------
        DATA                         DIVISION.                           
        FILE                         SECTION.                            
       *                                                                 
        FD FIBINP                    RECORDING MODE F.                   
        01 ENTRADA.                                                      
           05 NUMERO-ENT             PIC 9(04).                          
           05 FILLER                 PIC X(76).                          
       *                                                                 
        FD FIBOUT                    RECORDING MODE F.                   
        01 SAIDA.                                                        
           05 NUMERO-SAI             PIC 9(04).                          
           05 FILLER                 PIC X(76).                          
       *-----------------------------------------------------------------
        WORKING-STORAGE              SECTION.                            
       *                                                                 
        77 WK-FS-INP                 PIC X(02).                          
        77 WK-FS-OUT                 PIC X(02).
		     *                                                                 
        77 WK-NUM1                   PIC 9(05) VALUE 0.                  
        77 WK-NUM2                   PIC 9(05) VALUE 1.                  
        77 WK-NUM3                   PIC 9(05) VALUE 0.                  
       *-----------------------------------------------------------------
        PROCEDURE                    DIVISION.                           
       *-----------------------------------------------------------------
        0000-PRINCIPAL               SECTION.                            
             DISPLAY '*********************************************'.    
             DISPLAY '**************INICIANDO PGM....**************'.    
             DISPLAY '*********************************************'.    
       *                                                                 
             PERFORM 0100-INICIO.                                        
             PERFORM 0200-PROCESSA.                                      
             PERFORM 0300-FINALIZA.                                      
       *                                                                 
             STOP RUN.                                                   
       *                                                                 
		      0000-PRINCIPAL-FIM.          EXIT.                               
       *-----------------------------------------------------------------
        0100-INICIO                  SECTION.                            
       *-----------------------------------------------------------------
             OPEN INPUT   FIBINP.                                        
             IF WK-FS-INP NOT EQUAL '00'                                 
                DISPLAY 'FILE-STATUS: ' WK-FS-INP                        
                DISPLAY 'NAO HA ARQUIVO DE INPUT / ERRO NO INPUT'        
                STOP RUN                                                 
             END-IF.                                                     
       *                                                                 
             OPEN OUTPUT  FIBOUT.                                        
             IF WK-FS-OUT NOT EQUAL '00'                                 
                DISPLAY 'FILE-STATUS: ' WK-FS-OUT                        
                DISPLAY 'NAO FOI ABERTO ARQUIVO DE SAIDA / ERRO NA SAIDA'
                STOP RUN                                                 
             END-IF.                                                     
       *                                                                 
		           PERFORM 0150-LEITURA.                                       
       *-----------------------------------------------------------------
        0100-INICIO-FIM.             EXIT.                               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
        0150-LEITURA                 SECTION.                            
       *-----------------------------------------------------------------
             READ         FIBINP.                                        
       *                                                                 
             IF WK-FS-INP NOT EQUAL '00' OR NUMERO-ENT EQUAL SPACES      
                DISPLAY 'FILE-STATUS:' WK-FS-INP ' NUMERO-ENT:'          
                DISPLAY 'O REGISTRO NAO FOI LIDO OU NAO HA REGISTRO'     
                STOP RUN                                                 
             END-IF.                                                     
       *-----------------------------------------------------------------
        0150-LEITURA-EXIT.           EXIT.                               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
		      0200-PROCESSA                SECTION.                            
       *-----------------------------------------------------------------
            IF NUMERO-ENT EQUAL 0                                        
               MOVE    WK-NUM1 TO NUMERO-SAI                             
               PERFORM 0210-WRITE                                        
            ELSE                                                         
               MOVE    WK-NUM1 TO NUMERO-SAI                             
               PERFORM 0210-WRITE                                        
       *                                                                 
               MOVE    WK-NUM2 TO NUMERO-SAI                             
               PERFORM 0210-WRITE                                        
       *                                                                 
               PERFORM 0250-CALCULO UNTIL NUMERO-ENT < WK-NUM3           
            END-IF.                                                      
       *-----------------------------------------------------------------
        0200-PRINCIPAL-FIM.          EXIT.                               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
		      0210-WRITE                   SECTION.                            
       *-----------------------------------------------------------------
             INSPECT SAIDA REPLACING ALL LOW-VALUES BY SPACES.           
             WRITE SAIDA.                                                
       *                                                                 
             IF WK-FS-OUT NOT EQUAL '00'                                 
                DISPLAY 'FILE-STATUS: ' WK-FS-OUT                        
                DISPLAY 'ERRO NO WRITE, PROGRAMA ENCERRANDO.'            
                STOP RUN                                                 
             END-IF.                                                     
       *-----------------------------------------------------------------
        0210-WRITE-FIM.              EXIT.                               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
        0250-CALCULO                 SECTION.                            
       *-----------------------------------------------------------------
       *    ADD WK-NUM1 TO WK-NUM2 GIVING WK-NUM3.                       
            COMPUTE    WK-NUM3 =  WK-NUM1 + WK-NUM2.                     
		          MOVE       WK-NUM2 TO WK-NUM1.                               
            MOVE       WK-NUM3 TO WK-NUM2.                               
            MOVE       WK-NUM3 TO NUMERO-SAI.                            
            IF         WK-NUM3 <= NUMERO-ENT                             
               PERFORM 0210-WRITE                                        
            END-IF.                                                      
       *-----------------------------------------------------------------
        0250-CALCULO-FIM.            EXIT.                               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
        0300-FINALIZA                SECTION.                            
       *-----------------------------------------------------------------
            CLOSE FIBINP                                                 
            IF WK-FS-INP NOT EQUAL '00'                                  
               DISPLAY 'FILE-STATUS: ' WK-FS-INP                         
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO FIBINP'            
            END-IF.                                                      
       *                                                                 
		          CLOSE FIBOUT.                                                
            IF WK-FS-OUT NOT EQUAL '00'                                  
               DISPLAY 'FILE-STATUS: ' WK-FS-OUT                         
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO FIBOUT'            
            END-IF.                                                      
                                                                         
            DISPLAY '*********************************************'.     
            DISPLAY '************PGM FOI FINALIZADO.**************'.     
            DISPLAY '*********************************************'.     
       *                                                                 
       *-----------------------------------------------------------------
        0300-FINALIZA-FIM.           EXIT.                               
       *-----------------------------------------------------------------                          

      
