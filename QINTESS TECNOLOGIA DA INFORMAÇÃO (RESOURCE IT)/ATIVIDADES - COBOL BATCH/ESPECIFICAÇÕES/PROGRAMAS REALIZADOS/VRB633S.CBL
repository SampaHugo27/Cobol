        IDENTIFICATION                      DIVISION.                   
       *----------------------------------------------------------------
        PROGRAM-ID.                         EX01EM07.                   
        AUTHOR.                             HUGO SAMPAIO.               
        INSTALLATION.                       QINTESS.                    
        DATE-WRITTEN.                                                   
        DATE-COMPILED.                                                  
        SECURITY.                           APENAS O AUTOR PODE         
                                            MODIFICA-LO.                
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        ENVIRONMENT                         DIVISION.                   
       *----------------------------------------------------------------
        CONFIGURATION                       SECTION.                    
       *----------------------------------------------------------------
        SOURCE-COMPUTER.                    IBM-PC.                     
        OBJECT-COMPUTER.                    IBM-PC.                     
	SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.     
       *----------------------------------------------------------------
        INPUT-OUTPUT                        SECTION.                    
       *----------------------------------------------------------------
        FILE-CONTROL.                                                   
                                                                        
            SELECT CADALU  ASSIGN TO CADALU                             
            ORGANIZATION   IS SEQUENTIAL                                
            ACCESS MODE    IS SEQUENTIAL                                
            FILE STATUS    IS WS-FSALU.                                 
       *----------------------------------------------------------------
            SELECT CADATU  ASSIGN TO CADATU                             
            ORGANIZATION   IS SEQUENTIAL                                
            ACCESS MODE    IS SEQUENTIAL                                
            FILE STATUS    IS WS-FSATU.                                 
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        DATA                                DIVISION.                   
       *----------------------------------------------------------------
        FILE                                SECTION.                    
       *----------------------------------------------------------------
        FD  CADALU                                                      
            RECORD CONTAINS 42 CHARACTERS                               
            DATA RECORD     IS REG-ENT                                  
            RECORDING MODE  IS F.                                       
                                                                        
        01  REG-ENT.                                                    
            05 NUMERO-I                     PIC 9(05).                  
            05 NOME-I                       PIC X(20).                  
            05 NOTA1                        PIC 9(02)V99.               
            05 NOTA2                        PIC 9(02)V99.               
            05 NOTA3                        PIC 9(02)V99.               
            05 NOTA4                        PIC 9(02)V99.               
            05 SEXO-I                       PIC X(01).                  
       *----------------------------------------------------------------
        FD  CADATU    
	    RECORD CONTAINS 30 CHARACTERS                               
            DATA RECORD     IS REG-SAI                                  
            RECORDING MODE  IS F.                                       
                                                                        
        01  REG-SAI.                                                    
            05 NUMERO-O                     PIC 9(05).                  
            05 NOME-O                       PIC X(20).                  
            05 MEDIA                        PIC 9(02)V99.               
            05 SEXO-O                       PIC X(01).                  
       *----------------------------------------------------------------
        WORKING-STORAGE                     SECTION.                    
       *----------------------------------------------------------------
        77  WS-FSALU                        PIC X(02) VALUE '00'.       
        77  WS-FSATU                        PIC X(02) VALUE '00'.       
        77  WS-MEDIA                        PIC 9(02)V99.               
        77  WS-QTDE-REG-LIDOS               PIC 9(03) VALUE ZEROS.      
        77  WS-QTDE-REG-GERADOS             PIC 9(03) VALUE ZEROS.      
       *----------------------------------------------------------------   
		     *----------------------------------------------------------------
        PROCEDURE                           DIVISION.                   
       *----------------------------------------------------------------
        0000-PRINCIPAL                      SECTION.                    
       *----------------------------------------------------------------
            PERFORM 0100-INICIO             THRU  0100-99-INICIO-EXIT.  
            PERFORM 0200-PROCESSA           THRU  0200-99-PROCESSA-EXIT 
                                            UNTIL WS-FSALU EQUAL '10'.  
            PERFORM 0300-FINALIZA           THRU  0300-99-FINALIZA-EXIT.
            STOP RUN.                                                   
       *----------------------------------------------------------------
        0000-99-PRINCIPAL-EXIT.             EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0100-INICIO                         SECTION.                    
       *----------------------------------------------------------------
            DISPLAY '*************************************************'.
            DISPLAY '           INICIANDO O PROGRAMA...               '.
		          DISPLAY '*************************************************'.
            DISPLAY '                                                 '.
                                                                        
            OPEN INPUT  CADALU                                          
                 OUTPUT CADATU.                                         
                                                                        
            IF WS-FSALU NOT EQUAL '00'                                  
               DISPLAY 'ERRO DE ABERTURA DO ARQUIVO CADALU'             
               DISPLAY 'FILE STATUS = '     WS-FSALU                    
            END-IF.                                                     
                                                                        
            IF WS-FSATU NOT EQUAL '00'                                  
               DISPLAY 'ERRO DE ABERTURA DO ARQUIVO CADATU'             
               DISPLAY 'FILE STATUS = '     WS-FSATU                    
            END-IF.                                                     
            PERFORM 0210-LEITURA.                                       
                                                                        
            IF WS-QTDE-REG-LIDOS EQUAL 0                                
	       DISPLAY '***********************************************'
               DISPLAY '****** ARQUIVO DE ENTRADA CADALU VAZIO ********'
               DISPLAY '***********************************************'
            END-IF.                                                     
                                                                        
       *----------------------------------------------------------------
        0100-99-INICIO-EXIT.                EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0200-PROCESSA                       SECTION.                    
       *----------------------------------------------------------------
            PERFORM 0220-SELECAO.                                       
            PERFORM 0230-CALCULA-MEDIA.                                 
            PERFORM 0240-GRAVACAO.                                      
            PERFORM 0210-LEITURA.                                       
       *----------------------------------------------------------------
        0200-99-PROCESSA-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0210-LEITURA                        SECTION.                    
       *----------------------------------------------------------------
            READ CADALU.                                                
                                                                        
            IF WS-FSALU EQUAL '10'                                      
               GO TO 0210-99-LEITURA-EXIT                               
            END-IF.                                                     
                                                                        
            ADD 1 TO WS-QTDE-REG-LIDOS.                                 
       *----------------------------------------------------------------
        0210-99-LEITURA-EXIT.               EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0220-SELECAO                        SECTION.                    
       *----------------------------------------------------------------
            IF SEXO-I NOT EQUAL 'F'                                     
               PERFORM 0210-LEITURA                                     
		          END-IF.                                                     
       *----------------------------------------------------------------
        0220-99-SELECAO-EXIT.               EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0230-CALCULA-MEDIA                  SECTION.                    
       *----------------------------------------------------------------
            COMPUTE WS-MEDIA = (NOTA1 + NOTA2 + NOTA3 + NOTA4)/ 4.      
       *----------------------------------------------------------------
        0230-99-CALCULA-MEDIA-EXIT.         EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0240-GRAVACAO                       SECTION.                    
       *----------------------------------------------------------------
            MOVE NUMERO-I TO NUMERO-O.                                  
            MOVE NOME-I   TO NOME-O.                                    
            MOVE WS-MEDIA TO MEDIA.                                     
            MOVE SEXO-I   TO SEXO-O.                                    
		          WRITE REG-SAI.                                              
                                                                        
            IF WS-FSATU NOT EQUAL '00'                                  
               DISPLAY 'ERRO NA GRAVACAO DO ARQUIVO DE SAIDA CADATU'    
               DISPLAY 'FILE STATUS =  '    WS-FSATU                    
            END-IF.                                                     
                                                                        
            ADD 1 TO WS-QTDE-REG-GERADOS.                               
       *----------------------------------------------------------------
        0240-99-GRAVACAO-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0300-FINALIZA                       SECTION.                    
       *----------------------------------------------------------------
            CLOSE CADALU                                                
                  CADATU.                                               
                                                                        
            IF WS-FSALU NOT EQUAL '00'                                  
	       DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA CADALU'
               DISPLAY 'FILE STATUS =  '    WS-FSALU                    
            END-IF.                                                     
                                                                        
            IF WS-FSATU NOT EQUAL '00'                                  
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE SAIDA CADATU'  
               DISPLAY 'FILE STATUS =  '    WS-FSATU                    
            END-IF.                                                     
                                                                        
            PERFORM 0310-RESUMO-GERAL.                                  
                                                                        
            DISPLAY '*************************************************'.
            DISPLAY '         FIM DO PROGRAMA - THE END               '.
            DISPLAY '*************************************************'.
            DISPLAY '                                                 '.
       *----------------------------------------------------------------
        0300-99-FINALIZA-EXIT.              EXIT.                       
       *----------------------------------------------------------------  
       *----------------------------------------------------------------
        0310-RESUMO-GERAL                   SECTION.                    
       *----------------------------------------------------------------
            DISPLAY '*************************************************'.
            DISPLAY '       RESUMO/BALANCO GERAL DO PROGRAMA          '.
            DISPLAY '*************************************************'.
            DISPLAY '*************************************************'.
            DISPLAY '**** REGISTROS LIDOS    =   ' WS-QTDE-REG-LIDOS   .
            DISPLAY '**** REGISTROS GERADOS  =   ' WS-QTDE-REG-GERADOS .
            DISPLAY '*************************************************'.
            DISPLAY '*************************************************'.
       *----------------------------------------------------------------
        0310-99-RESUMO-GERAL-EXIT.          EXIT.                       
       *----------------------------------------------------------------                                             

      
