       *-----------------------------------------------------------------
        IDENTIFICATION               DIVISION.                           
        PROGRAM-ID. VRBQHHTL.                                            
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
              SELECT TELINPUT ASSIGN TO TELINPUT                         
              FILE STATUS IS WK-FS-INPUT.                                
       *                                                                 
              SELECT TELOUTPT ASSIGN TO TELOUTPT                         
              FILE STATUS IS WK-FS-OUTPT.                              
       *-----------------------------------------------------------------
        DATA                         DIVISION.                           
        FILE                         SECTION.                            
       *-----------------------------------------------------------------
        FD TELINPUT   RECORDING MODE F.                                  
        01 ENTRADA.                                                      
           05 NUM-ENT                PIC X(20).                          
           05 FILLER                 PIC X(60).                          
       *                                                                 
        FD TELOUTPT   RECORDING MODE F.                                  
        01 SAIDA.                                                        
           05 NUM-SAI                PIC X(13).                          
           05 FILLER                 PIC X(67).                          
       *-----------------------------------------------------------------
        WORKING-STORAGE              SECTION.                            
        77 WK-FS-INPUT               PIC X(02).                          
        77 WK-FS-OUTPT               PIC X(02).                          
       *
		     *                                                                 
        01 WK-CONTADORES.                                                
           05 WK-CONT-LINHA          PIC 9(02) VALUE ZEROES.             
           05 WK-CONT-INPUT          PIC 9(02) VALUE      1.             
           05 WK-CONT-OUTPT          PIC 9(02) VALUE      1.             
           05 WK-CONT-INSPC          PIC 9(02) VALUE      0.             
       *-----------------------------------------------------------------
        PROCEDURE                    DIVISION.                           
        0000-PRINCIPAL               SECTION.                            
            PERFORM 0100-INICIO.                                         
            PERFORM 0200-PROCESSA UNTIL WK-FS-INPUT EQUAL '10'.          
            PERFORM 0900-TERMINO.                                        
       *                                                                 
            STOP RUN.                                                    
       *-----------------------------------------------------------------
        0100-INICIO                  SECTION.                            
            OPEN  INPUT   TELINPUT.                                      
            IF WK-FS-INPUT NOT EQUAL '00'                                
		             DISPLAY 'FILE-STATUS: ' WK-FS-INPUT                       
               DISPLAY 'NAO HA ARQUIVO TELINPUT'                         
               STOP RUN                                                  
            END-IF.                                                      
       *                                                                 
            OPEN  OUTPUT  TELOUTPT.                                      
            IF WK-FS-OUTPT NOT EQUAL '00'                                
               DISPLAY 'FILE-STATUS: ' WK-FS-OUTPT                       
               DISPLAY 'ERRO AO CRIAR ARQ PARA OUTPUT'                   
               STOP RUN                                                  
            END-IF.                                                      
       *                                                                 
            READ          TELINPUT.                                      
            IF WK-FS-INPUT     EQUAL '10'                                
            OR WK-FS-INPUT NOT EQUAL '00'                                
               DISPLAY 'FILE-STATUS: ' WK-FS-INPUT                       
               DISPLAY 'ARQUIVO TELINPUT VAZIO OU ERRO AO LER TELINPUT'  
               STOP RUN                                                  

		          END-IF.                                                      
       *                                                                 
            PERFORM 0150-INSPEC.                                         
       *                                                                 
        0100-INICIO-FIM. EXIT.                                           
       *-----------------------------------------------------------------
        0125-LEITURA                 SECTION.                            
            READ          TELINPUT                                       
            IF WK-FS-INPUT NOT EQUAL '00'                                
               DISPLAY 'FILE-STATUS: ' WK-FS-INPUT                       
               DISPLAY 'FIM DO ARQ TELINPUT'                             
            END-IF.                                                      
       *                                                                 
            PERFORM 0150-INSPEC.                                         
       *                                                                 
        0125-LEITURA-FIM. EXIT.                                          
       *-----------------------------------------------------------------
        0150-INSPEC                  SECTION.                            
             INSPECT NUM-ENT TALLYING                                    
             WK-CONT-INSPC FOR CHARACTERS.                               
        0150-INSPEC-FIM. EXIT.                                           
       *-----------------------------------------------------------------
        0200-PROCESSA                SECTION.                            
            PERFORM 0300-CORRECAO UNTIL WK-CONT-INSPC > 20               
       *                                                                 
            IF  NUM-SAI(8:1) NOT EQUAL SPACES                            
            OR  NUM-SAI(8:1) NOT EQUAL LOW-VALUES                        
               ADD 1 TO WK-CONT-LINHA                                    
               WRITE SAIDA                                               
            END-IF.                                                      
       *                                                                 
            IF WK-FS-OUTPT NOT EQUAL '00'                                
               DISPLAY 'FILE-STATUS: ' WK-FS-OUTPT                       
               DISPLAY 'ERRO NA GRAVACAO DE OUTPUT'                      
               STOP RUN                                                  
		          END-IF.                                                      
       *                                                                 
            MOVE SPACES TO NUM-SAI                                       
            MOVE 1      TO WK-CONT-INPUT                                 
            MOVE 1      TO WK-CONT-OUTPT                                 
            MOVE 0      TO WK-CONT-INSPC                                 
            PERFORM 0125-LEITURA.                                        
        0200-PROCESSA-FIM. EXIT.                                         
       *-----------------------------------------------------------------
        0300-CORRECAO                SECTION.                            
            IF WK-CONT-INPUT EQUAL WK-CONT-INSPC                         
               ADD 1 TO WK-CONT-INSPC                                    
            END-IF.                                                      
       *                                                                 
            IF NUM-ENT(WK-CONT-INPUT:1) IS NUMERIC                       
               MOVE NUM-ENT(WK-CONT-INPUT:1)                             
               TO   NUM-SAI(WK-CONT-OUTPT:1)                             
               ADD 1 TO WK-CONT-OUTPT                                    
 		        END-IF.                                                      
           ADD 1 TO WK-CONT-INPUT.                                      
       0300-CORRECAO-FIM. EXIT.                                         
      *-----------------------------------------------------------------
       0900-TERMINO                 SECTION.                            
           IF WK-CONT-LINHA EQUAL 0                                     
              DISPLAY '        NENHUMA LINHA ESCRITA        '           
              DISPLAY '       INPUT FORA DO PARAMETRO       '           
              DISPLAY 'MINIMO: 8 NUMEROS, MAXIMO: 20 NUMEROS'           
           END-IF.                                                      
      *                                                                 
           CLOSE        TELINPUT                                        
           IF WK-FS-INPUT NOT EQUAL '00'                                
              DISPLAY 'FILE-STATUS: ' WK-FS-INPUT                       
              DISPLAY 'ERRO NO FECHAMENTO DO TELINPUT'                  
              STOP RUN                                                  
           END-IF.                                                      
      *                                                                 
            CLOSE        TELOUTPT.                                       
            IF WK-FS-OUTPT NOT EQUAL '00'                                
               DISPLAY 'FILE-STATUS: ' WK-FS-OUTPT                       
               DISPLAY 'ERRO NO FECHAMENTO DO TELOUTPT'                  
               STOP RUN                                                  
            END-IF.                                                      
       *                                                                 
        0900-TERMINO-FIM. EXIT.                                          
       *-----------------------------------------------------------------                                                                 

      
