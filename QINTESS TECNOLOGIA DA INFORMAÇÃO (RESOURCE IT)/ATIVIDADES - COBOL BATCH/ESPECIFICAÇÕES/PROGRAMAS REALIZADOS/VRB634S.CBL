	IDENTIFICATION                      DIVISION.                   
       *----------------------------------------------------------------
        PROGRAM-ID.                         EM01EX08                    
        AUTHOR.                             HUGO SAMPAIO.               
        INSTALLATION.                       QINTESS.                    
        DATE-WRITTEN.                                                   
        DATE-COMPILED.                                                  
        SECURITY.                           APENAS O AUTOR PODE         
                                            MODIFICA-LO.                
                                                                        
       *----------------------------------------------------------------
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
                                                                        
            SELECT CADFUN ASSIGN TO CADFUN                              
            ORGANIZATION  IS SEQUENTIAL                                 
            ACCESS MODE   IS SEQUENTIAL                                 
            FILE STATUS   IS WS-FSFUN.                                  
       *----------------------------------------------------------------
            SELECT CADSAI ASSIGN TO CADSAI                              
            ORGANIZATION  IS SEQUENTIAL                                 
            ACCESS MODE   IS SEQUENTIAL                                 
            FILE STATUS   IS WS-FSSAI.                                  
       *----------------------------------------------------------------
       *----------------------------------------------------------------
	DATA                                DIVISION.                   
       *----------------------------------------------------------------
        FILE                                SECTION.                    
       *----------------------------------------------------------------
        FD  CADFUN                                                      
            RECORD CONTAINS 32 CHARACTERS                               
            DATA RECORD IS REG-ENT                                      
            RECORDING MODE IS F.                                        
                                                                        
        01  REG-ENT.                                                    
            05 CODIGO-I                     PIC 9(05).                  
            05 NOME-I                       PIC X(20).                  
            05 SALARIO-BRUTO                PIC 9(05)V99.               
       *----------------------------------------------------------------
        FD  CADSAI                                                      
            RECORD CONTAINS 32 CHARACTERS                               
            DATA RECORD IS REG-SAI                                      
            RECORDING MODE IS F.                         
		                                                                      
        01  REG-SAI.                                                    
            05 CODIGO-O                     PIC 9(05).                  
            05 NOME-O                       PIC X(20).                  
            05 SALARIO-REAJUSTADO           PIC 9(05)V99.               
       *----------------------------------------------------------------
        WORKING-STORAGE                     SECTION.                    
       *----------------------------------------------------------------
        77  WS-FSFUN                        PIC X(02)    VALUE '00'.    
        77  WS-FSSAI                        PIC X(02)    VALUE '00'.    
        77  WS-SALARIO-NOVO                 PIC 9(05)V99 VALUE ZEROS.   
        77  WS-QTDE-REG-LIDOS               PIC 9(03)    VALUE ZEROS.   
        77  WS-QTDE-REG-GERADOS             PIC 9(03)    VALUE ZEROS.   
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        PROCEDURE                           DIVISION.                   
       *----------------------------------------------------------------
        0000-PRINCIPAL                      SECTION.                    
       *----------------------------------------------------------------
            PERFORM 0100-INICIO             THRU  0100-99-INICIO-EXIT.  
            PERFORM 0200-PROCESSA           THRU  0200-99-PROCESSA-EXIT 
                                            UNTIL WS-FSFUN EQUAL '10'.  
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
                                                                        
            OPEN INPUT  CADFUN                                          
		               OUTPUT CADSAI.                                         
                                                                        
            IF  WS-FSFUN NOT EQUAL '00'                                 
                DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA CADFUN' 
                DISPLAY 'FILE STATUS =  '   WS-FSFUN                    
            END-IF.                                                     
                                                                        
            IF  WS-FSSAI NOT EQUAL '00'                                 
                DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA CADSAI' 
                DISPLAY 'FILE STATUS =  '   WS-FSSAI                    
            END-IF.                                                     
                                                                        
            PERFORM 0210-LEITURA.                                       
                                                                        
            IF WS-QTDE-REG-LIDOS EQUAL 0                                
               DISPLAY '***********************************************'
               DISPLAY '****** ARQUIVO DE ENTRADA CADFUN VAZIO ********'
               DISPLAY '***********************************************'
	    END-IF.                                                     
                                                                        
       *----------------------------------------------------------------
        0100-99-INICIO-EXIT.                EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0200-PROCESSA                       SECTION.                    
       *----------------------------------------------------------------
            PERFORM  0220-CALCULA-SALARIO-NOVO.                         
            PERFORM  0230-GRAVACAO.                                     
            PERFORM  0210-LEITURA.                                      
       *----------------------------------------------------------------
        0200-99-PROCESSA-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0210-LEITURA                        SECTION.                    
       *----------------------------------------------------------------
            READ CADFUN.                  
		                                                                      
            IF WS-FSFUN EQUAL '10'                                      
               GO TO   0210-99-LEITURA-EXIT                             
            END-IF.                                                     
                                                                        
            ADD   1               TO        WS-QTDE-REG-LIDOS.          
       *----------------------------------------------------------------
        0210-99-LEITURA-EXIT.               EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0220-CALCULA-SALARIO-NOVO           SECTION.                    
       *----------------------------------------------------------------
            IF SALARIO-BRUTO <= 1000                                    
               COMPUTE WS-SALARIO-NOVO = 1,12 * SALARIO-BRUTO                                                            
            END-IF.                                                     
       *----------------------------------------------------------------
            IF SALARIO-BRUTO >= 1000 AND SALARIO-BRUTO <= 2000          
	       COMPUTE WS-SALARIO-NOVO = 1,11 * SALARIO-BRUTO           
            END-IF.                                                     
       *----------------------------------------------------------------
            IF SALARIO-BRUTO > 2000                                     
               COMPUTE WS-SALARIO-NOVO = 1,1 * SALARIO-BRUTO            
            END-IF.                                                     
       *----------------------------------------------------------------
        0220-CALCULA-SALARIO-NOVO-EXIT.     EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0230-GRAVACAO                       SECTION.                    
       *----------------------------------------------------------------
            MOVE  CODIGO-I        TO        CODIGO-O.                   
            MOVE  NOME-I          TO        NOME-O.                     
            MOVE  WS-SALARIO-NOVO TO        SALARIO-REAJUSTADO.         
            WRITE REG-SAI.                                              
                                                                        
            IF WS-FSSAI NOT EQUAL '00'
	       DISPLAY 'ERRO DE GRAVACAO NO ARQUIVO DE SAIDA CADSAI'    
               DISPLAY 'FILE STATUS =  '    WS-FSSAI                    
            END-IF.                                                     
                                                                        
            ADD   1               TO        WS-QTDE-REG-GERADOS.        
       *----------------------------------------------------------------
        0230-99-GRAVACAO-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0300-FINALIZA                       SECTION.                    
       *----------------------------------------------------------------
            CLOSE CADFUN                                                
                  CADSAI.                                               
                                                                        
            IF WS-FSFUN NOT EQUAL '00'                                  
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA CADFUN'
               DISPLAY 'FILE STATUS =  '    WS-FSFUN                    
            END-IF.   
		                                                                      
            IF WS-FSSAI NOT EQUAL '00'                                  
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE SAIDA CADSAI'  
               DISPLAY 'FILE STATUS =  '    WS-FSSAI                    
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
       *----------------------------------------------------------------
        0310-99-RESUMO-GERAL-EXIT.          EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------                                                                                    
                              
      
