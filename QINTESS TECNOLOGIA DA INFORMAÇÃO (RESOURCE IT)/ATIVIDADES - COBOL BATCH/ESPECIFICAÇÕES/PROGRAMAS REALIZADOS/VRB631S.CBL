       *-----------------------------------------------------------------
        IDENTIFICATION                     DIVISION.                     
        PROGRAM-ID. VRB631S.                                             
        AUTHOR.     HUGO DA SILVA SAMPAIO.                                           
       *-----------------------------------------------------------------
        ENVIRONMENT                        DIVISION.                     
       *-----------------------------------------------------------------
       *SOURCE                                                           
       *OBJECT                                                           
       *SPECIAL-NAMES                                                    
       *-----------------------------------------------------------------
        INPUT-OUTPUT                       SECTION.                      
       *-----------------------------------------------------------------
        FILE-CONTROL.                                                    
       *                                                                 
              SELECT ARQENT ASSIGN TO ARQENT                             
              FILE STATUS IS WK-FS-ARQENT.                               
       *                                       
		            SELECT ARQSAI ASSIGN TO ARQSAI                             
              FILE STATUS IS WK-FS-ARQSAI.                               
       *-----------------------------------------------------------------
        DATA                               DIVISION.                     
       *-----------------------------------------------------------------
        FILE                               SECTION.                      
       *-----------------------------------------------------------------
        FD ARQENT                  RECORDING MODE F.                     
        01 ENTRADA.                                                      
           05 CPF-E                        PIC X(11).                    
           05 NOME-CLIENTE-E               PIC X(30).                    
           05 VALOR-GASTO                  PIC 9(15)V99.                 
           05 FILLER                       PIC X(02).                    
       *                                                                 
        FD ARQSAI                  RECORDING MODE F.                     
        01 SAIDA.                                                        
           05 CPF-S                        PIC X(11).                    
           05 NOME-CLIENTE-S               PIC X(30).                    
		         05 VALOR-TOTAL                  PIC 9(15)V99.                 
           05 FILLER                       PIC X(02).                    
       *-----------------------------------------------------------------
        WORKING-STORAGE                    SECTION.                      
       *-----------------------------------------------------------------
        77 WK-FS-ARQENT                    PIC X(02)     VALUE SPACES.   
        77 WK-FS-ARQSAI                    PIC X(02)     VALUE SPACES.   
       *                                                                 
        01 WK-QUEBRA-ANT                   PIC 9(11)     VALUE 0.        
        01 WK-QUEBRA-ATU                   PIC 9(11)     VALUE 0.        
       *                                                                 
        01 WK-CONT-LEITURA                 PIC 9(04)     VALUE 0.        
        01 WK-CONT-GRAVADO                 PIC 9(04)     VALUE 0.        
        01 WK-CONT-QUEBRA                  PIC 9(04)     VALUE 0.        
       *-----------------------------------------------------------------
        PROCEDURE                          DIVISION.                     
       *-----------------------------------------------------------------
        0000-PRINCIPAL                     SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            DISPLAY '**********************************************'     
            DISPLAY '****************INICIANDO PGM.****************'     
            DISPLAY '**********************************************'     
            DISPLAY '                                              '     
       *                                                                 
            PERFORM 0100-INICIO.                                         
            PERFORM 0200-PROCESSA UNTIL WK-FS-ARQENT EQUAL '10'          
            PERFORM 0900-TERMINO.                                        
       *                                                                 
            STOP RUN.                                                    
       *                                                                 
        0000-PRINCIPAL-FIM. EXIT.                                        
       *-----------------------------------------------------------------
        0100-INICIO                        SECTION.                      
       *-----------------------------------------------------------------
            OPEN INPUT    ARQENT.
		          IF WK-FS-ARQENT  NOT EQUAL '00'                              
               DISPLAY 'FILE-STATUS: '  WK-FS-ARQENT                     
               DISPLAY 'ERRO AO ABRIR ENTRADA'                           
               STOP RUN                                                  
            END-IF.                                                      
       *                                                                 
            OPEN OUTPUT   ARQSAI.                                        
            IF WK-FS-ARQSAI  NOT EQUAL '00'                              
               DISPLAY 'FILE-STATUS: '  WK-FS-ARQSAI                     
               DISPLAY 'ERRO AO ABRIR SAIDA'                             
               STOP RUN                                                  
            END-IF.                                                      
       *                                                                 
            PERFORM 0225-LEITURA                                         
                                                                         
            IF  WK-FS-ARQENT     EQUAL '10'                              
               DISPLAY 'ARQUIVO ENTRADA VAZIO'                           
               GO TO 0100-INICIO-FIM                                     
		          END-IF.                                                      
                                                                         
            MOVE CPF-E              TO  WK-QUEBRA-ATU.                   
            MOVE CPF-E              TO  WK-QUEBRA-ANT.                   
       *                                                                 
            MOVE CPF-E              TO  CPF-S.                           
            MOVE NOME-CLIENTE-E     TO  NOME-CLIENTE-S.                  
       *                                                                 
        0100-INICIO-FIM.    EXIT.                                        
       *-----------------------------------------------------------------
        0200-PROCESSA                      SECTION.                      
       *-----------------------------------------------------------------
            IF WK-QUEBRA-ATU NOT EQUAL  WK-QUEBRA-ANT                    
               PERFORM 0250-GRAVACAO                                     
                                                                         
               MOVE  CPF-E          TO  CPF-S                            
               MOVE  NOME-CLIENTE-E TO  NOME-CLIENTE-S                   
               MOVE  0              TO  VALOR-TOTAL                      
		             MOVE  WK-QUEBRA-ATU  TO  WK-QUEBRA-ANT                    
                                                                         
               ADD  1               TO  WK-CONT-QUEBRA                   
                                                                         
            END-IF.                                                      
       *                                                                 
            ADD VALOR-GASTO         TO  VALOR-TOTAL                      
       *                                                                 
            PERFORM 0225-LEITURA.                                        
       *                                                                 
        0200-PROCESSA-FIM.  EXIT.                                        
       *-----------------------------------------------------------------
        0225-LEITURA                       SECTION.                      
       *-----------------------------------------------------------------
            READ          ARQENT.                                        
                                                                         
            IF  WK-FS-ARQENT NOT EQUAL '00'                              
            AND WK-FS-ARQENT NOT EQUAL '10'                              
 	       DISPLAY 'ERRO AO LER ARQUIVO'                             
               DISPLAY 'FILE-STATUS: ' WK-FS-ARQENT                      
               STOP RUN                                                  
            ELSE                                                         
               IF WK-FS-ARQENT   EQUAL '10'                              
                  GO TO 0225-LEITURA-FIM                                 
               END-IF                                                    
            END-IF.                                                      
       *                                                                 
            ADD  1                  TO  WK-CONT-LEITURA.                 
            MOVE CPF-E              TO  WK-QUEBRA-ATU.                   
       *                                                                 
        0225-LEITURA-FIM.   EXIT.                                        
       *-----------------------------------------------------------------
        0250-GRAVACAO                      SECTION.                      
       *-----------------------------------------------------------------
            WRITE SAIDA                                                  
		          IF  WK-FS-ARQSAI  NOT EQUAL '00'                             
               DISPLAY 'FILE-STATUS: ' WK-FS-ARQENT                      
               DISPLAY 'ERRO NA GRAVACAO'                                
               STOP RUN                                                  
            END-IF.                                                      
                                                                         
            ADD  1               TO  WK-CONT-GRAVADO.                    
                                                                         
        0250-GRAVACAO-FIM.  EXIT.                                        
       *-----------------------------------------------------------------
        0900-TERMINO                       SECTION.                      
       *-----------------------------------------------------------------
            IF  WK-CONT-LEITURA    >=   1                                
               PERFORM 0250-GRAVACAO                                     
               ADD  1                  TO  WK-CONT-QUEBRA                
            END-IF.                                                      
                                                                         
            CLOSE         ARQENT.                                        
		          IF WK-FS-ARQENT  NOT EQUAL '00'                            
               DISPLAY 'FILE-STATUS: '  WK-FS-ARQENT                   
               DISPLAY 'ERRO AO FECHAR ARQENT'                         
               STOP RUN                                                
            END-IF.                                                    
       *                                                               
            CLOSE         ARQSAI.                                      
            IF WK-FS-ARQSAI  NOT EQUAL '00'                            
               DISPLAY 'FILE-STATUS: '  WK-FS-ARQSAI                   
               DISPLAY 'ERRO AO FECHAR ARQSAI'                         
               STOP RUN                                                
            END-IF.                                                    
       *                                                               
            DISPLAY 'QTD DE LEITURAS: ' WK-CONT-LEITURA '          '   
            DISPLAY 'QTD DE GRAVADOS: ' WK-CONT-GRAVADO '          '   
            DISPLAY 'QTD DE QUEBRAS : ' WK-CONT-QUEBRA  '          '   
            DISPLAY '                                              '   
            DISPLAY '**********************************************'.  
            DISPLAY '**********PGM EXECUTADO COM SUCESSO.**********'     
            DISPLAY '**********************************************'.    
                                                                         
        0900-TERMINO-FIM.   EXIT.                                        
       *-----------------------------------------------------------------
                                                                                                                                           

      
