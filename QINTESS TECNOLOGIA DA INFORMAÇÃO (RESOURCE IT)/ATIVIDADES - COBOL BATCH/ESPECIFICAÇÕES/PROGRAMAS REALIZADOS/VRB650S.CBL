	IDENTIFICATION                      DIVISION.                    
       *----------------------------------------------------------------*
        PROGRAM-ID.                         EM02EX01.                    
        AUTHOR.                             HUGO SAMPAIO.                
        INSTALLATION.                       QINTESS.                     
        DATE-WRITTEN.                                                    
        DATE-COMPILED.                                                   
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        ENVIRONMENT                         DIVISION.                    
       *----------------------------------------------------------------*
        CONFIGURATION                       SECTION.                     
       *----------------------------------------------------------------*
        SOURCE-COMPUTER.                    IBM-PC.                      
        OBJECT-COMPUTER.                    IBM-PC.                      
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.      
       *----------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
                                                                         
            SELECT CADSOC1  ASSIGN TO CADSOC1                            
            ORGANIZATION    IS SEQUENTIAL                                
            ACCESS MODE     IS SEQUENTIAL                                
            FILE   STATUS   IS WS-FSCADSOC1.                             
       *----------------------------------------------------------------*
            SELECT CADSOC2  ASSIGN TO CADSOC2                            
            ORGANIZATION    IS SEQUENTIAL                                
            ACCESS MODE     IS SEQUENTIAL                                
            FILE   STATUS   IS WS-FSCADSOC2.                             
       *----------------------------------------------------------------*
            SELECT RELSOCIO ASSIGN TO RELSOCIO                           
            FILE   STATUS   IS WS-FSRELSOCIO.                            
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        DATA                                DIVISION.
       *----------------------------------------------------------------*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD CADSOC1                                                       
            LABEL RECORD    ARE STANDARD                                 
            RECORD CONTAINS 49 CHARACTERS                                
            DATA  RECORD IS REG-ENT                                      
            RECORDING MODE IS F.                                         
                                                                         
        01 REG-ENT.                                                      
            05 CODIGO-PAGAMENTO1            PIC 9(02).                   
            05 NUMERO-SOCIO1                PIC 9(06).                   
            05 NOME-SOCIO1                  PIC X(30).                   
            05 VALOR-PAGAMENTO1             PIC 9(09)V9(02).             
       *----------------------------------------------------------------*
        FD CADSOC2                                                       
            LABEL RECORD    ARE STANDARD                                 
            RECORD CONTAINS 47 CHARACTERS
	    DATA  RECORD IS REG-SAI                                      
            RECORDING MODE IS F.                                         
                                                                         
        01 REG-SAI.                                                      
            05 NUMERO-SOCIO2                PIC 9(06).                   
            05 NOME-SOCIO2                  PIC X(30).                   
            05 VALOR-PAGAMENTO2             PIC 9(09)V9(02).             
       *----------------------------------------------------------------*
        FD RELSOCIO                                                      
            LABEL RECORD IS OMITTED.                                     
                                                                         
        01 REG-ATR                          PIC X(80).                   
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.                     
       *----------------------------------------------------------------*
        77  WS-CTLIN         PIC 9(02)       VALUE 40.                   
        77  WS-CTPAG         PIC 9(02)       VALUE ZEROS.                
        77  WS-VALOR-TOTAL   PIC 9(12)V9(02) VALUE ZEROES.
		                                                                       
        01  WS-FSCADSOC1     PIC X(02)      VALUE '00'.                  
        77  WS-FSCADSOC2     PIC X(02)      VALUE '00'.                  
        77  WS-FSRELSOCIO    PIC X(02)      VALUE '00'.                  
        77  WS-QTDE-LIDOS    PIC 9(03)      VALUE ZEROS.                 
        77  WS-QTDE-GRAVADOS PIC 9(03)      VALUE ZEROS.                 
        77  WS-TOTAL-SOCIOS  PIC 9(03)      VALUE ZEROS.                 
       *----------------------------------------------------------------*
       *          LAYOUT DO RELATORIO - AREA DE CABECALHOS              *
       *----------------------------------------------------------------*
        01  CAB-01.                                                      
            05 FILLER     PIC X(70) VALUE   SPACES.                      
            05 FILLER     PIC X(05) VALUE   'PAG. '.                     
            05 VAR-PAG    PIC Z9.                                        
            05 FILLER     PIC X(03) VALUE   SPACES.                      
       *----------------------------------------------------------------*
        01  CAB-02.                                                      
            05 FILLER     PIC X(26) VALUE   SPACES.
	    05 FILLER     PIC X(27) VALUE  'RELACAO DE SOCIOS ATRASADOS'.
            05 FILLER     PIC X(27) VALUE   SPACES.                      
       *----------------------------------------------------------------*
        01  CAB-03.                                                      
            05 FILLER     PIC X(05) VALUE   SPACES.                      
            05 FILLER     PIC X(15) VALUE   'NUMERO DO SOCIO'.           
            05 FILLER     PIC X(10) VALUE   SPACES.                      
            05 FILLER     PIC X(13) VALUE   'NOME DO SOCIO'.             
            05 FILLER     PIC X(15) VALUE   SPACES.                      
            05 FILLER     PIC X(22) VALUE   'VALOR DO PAGAMENTO'.        
                                                                         
       *----------------------------------------------------------------*
       *          LAYOUT DO RELATORIO - AREA DE DETALHES                *
       *----------------------------------------------------------------*
        01  DETALHE.                                                     
            05 FILLER     PIC X(09) VALUE   SPACES.                      
            05 NUM        PIC 999.999.                                   
            05 FILLER     PIC X(06) VALUE   SPACES.
	    05 NOME       PIC X(30).                                     
            05 FILLER     PIC X(08) VALUE   SPACES.                      
            05 VAL        PIC ZZZ.ZZZ.ZZ9,99.                            
            05 FILLER     PIC X(06) VALUE   SPACES.                      
       *----------------------------------------------------------------*
        01  DETALHE-ARQ-VAZIO.                                           
            05 FILLER     PIC X(09) VALUE   SPACES.                      
            05 FILLER     PIC X(24) VALUE   'ARQUIVO DE ENTRADA VAZIO'.  
            05 FILLER     PIC X(44) VALUE   SPACES.                      
       *----------------------------------------------------------------*
       *          LAYOUT DO RELATORIO - AREA DE TOTALIZADORES           *
       *----------------------------------------------------------------*
        01  TOT-01.                                                      
            05 FILLER     PIC X(05) VALUE   SPACES.                      
            05 FILLER     PIC X(28) VALUE 'TOTAL DE SOCIOS ATRASADOS : '.
            05 TOT-SOCIO  PIC 9(05) VALUE   ZEROES.                      
            05 FILLER      PIC X(42) VALUE  SPACES.                      
       *----------------------------------------------------------------*
	01  TOT-02.                                                      
            05 FILLER      PIC X(05) VALUE  SPACES.                      
            05 FILLER      PIC X(23) VALUE 'VALOR TOTAL ATRASADO : '.    
            05 MASC-VAL-T  PIC ZZZ.ZZZ.ZZZ.ZZ9,99.                       
            05 FILLER      PIC X(34) VALUE  SPACES.                      
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        PROCEDURE                           DIVISION.                    
       *----------------------------------------------------------------*
        0000-PRINCIPAL                      SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 0100-INICIO      THRU   0100-99-INICIO-EXIT.         
            PERFORM 0200-PROCESSA    THRU   0200-99-PROCESSA-EXIT        
                                     UNTIL  WS-FSCADSOC1 EQUAL '10'.     
            PERFORM 0300-FINALIZA    THRU   0300-99-FINALIZA-EXIT.       
            STOP RUN.                                                    
       *----------------------------------------------------------------*
        0000-99-PRINCIPAL-EXIT.             EXIT.
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0100-INICIO                         SECTION.                     
       *----------------------------------------------------------------*
            DISPLAY '*************************************************'. 
            DISPLAY '           INICIANDO O PROGRAMA...               '. 
            DISPLAY '*************************************************'. 
            DISPLAY '                                                 '. 
                                                                         
            OPEN INPUT  CADSOC1.                                         
                                                                         
            IF WS-FSCADSOC1  NOT EQUAL '00'                              
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA CADSOC1'  
               DISPLAY 'FILE STATUS =  '    WS-FSCADSOC1                 
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN OUTPUT CADSOC2. 
		                                                                       
            IF WS-FSCADSOC2  NOT EQUAL '00'                              
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE SAIDA CADSOC2'    
               DISPLAY 'FILE STATUS =  '    WS-FSCADSOC2                 
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN OUTPUT RELSOCIO.                                        
                                                                         
            IF WS-FSRELSOCIO NOT EQUAL '00'                              
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE SAIDA RELATO'     
               DISPLAY 'FILE STATUS =  '    WS-FSRELSOCIO                
               STOP RUN                                                  
            END-IF.                                                      
                                                                         
            PERFORM 0205-LEITURA.                                        
                                                                         
            IF WS-QTDE-LIDOS EQUAL 0
	       DISPLAY '***********************************************' 
               DISPLAY '********** ARQUIVO DE ENTRADA VAZIO ***********' 
               DISPLAY '***********************************************' 
               MOVE    ZEROS             TO WS-CTLIN                     
               PERFORM 0330-IMPRIMA-ARQ-VAZIO                            
            END-IF.                                                      
                                                                         
       *----------------------------------------------------------------*
        0100-99-INICIO-EXIT.                EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0200-PROCESSA                       SECTION.                     
       *----------------------------------------------------------------*
            IF CODIGO-PAGAMENTO1 EQUAL 1                                 
               PERFORM 0210-GRAVACAO                                     
            END-IF.                                                      
                                                                         
            IF CODIGO-PAGAMENTO1 EQUAL 2
	       IF WS-CTLIN GREATER 39                                    
                  PERFORM 0220-CABECALHO-PARTE1                          
                  PERFORM 0230-CABECALHO-PARTE2                          
                  MOVE  SPACES TO REG-ATR                                
                  WRITE REG-ATR                                          
               END-IF                                                    
                                                                         
               PERFORM 0240-IMPRIMA-DETALHE                              
               ADD     1                    TO WS-TOTAL-SOCIOS           
               ADD     VALOR-PAGAMENTO1     TO WS-VALOR-TOTAL            
               DISPLAY WS-TOTAL-SOCIOS                                   
               DISPLAY WS-VALOR-TOTAL                                    
                                                                         
                                                                         
            END-IF.                                                      
                                                                         
            PERFORM 0205-LEITURA.                                        
       *----------------------------------------------------------------*
	0200-99-PROCESSA-EXIT.              EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0205-LEITURA                        SECTION.                     
       *----------------------------------------------------------------*
            READ CADSOC1.                                                
                                                                         
            IF   WS-FSCADSOC1 EQUAL '10'                                 
                 GO TO 0205-99-LEITURA-EXIT                              
            END-IF.                                                      
                                                                         
            ADD  1  TO WS-QTDE-LIDOS.                                    
                                                                         
       *----------------------------------------------------------------*
        0205-99-LEITURA-EXIT.               EXIT.                        
       *----------------------------------------------------------------*
        0210-GRAVACAO                       SECTION.                     
       *----------------------------------------------------------------*
	    MOVE  NUMERO-SOCIO1    TO       NUMERO-SOCIO2.               
            MOVE  NOME-SOCIO1      TO       NOME-SOCIO2.                 
            MOVE  VALOR-PAGAMENTO1 TO       VALOR-PAGAMENTO2.            
            WRITE REG-SAI.                                               
            ADD   1                TO       WS-QTDE-GRAVADOS.            
       *----------------------------------------------------------------*
        0210-99-GRAVACAO-EXIT.              EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0220-CABECALHO-PARTE1               SECTION.                     
       *----------------------------------------------------------------*
            MOVE  SPACES    TO REG-ATR.                                  
            WRITE REG-ATR.                                               
            WRITE REG-ATR.                                               
            ADD   1         TO WS-CTPAG.                                 
            MOVE  WS-CTPAG  TO VAR-PAG.                                  
            WRITE REG-ATR   FROM CAB-01.                                 
            MOVE  SPACES    TO REG-ATR.
	    WRITE REG-ATR.                                               
            WRITE REG-ATR.                                               
            WRITE REG-ATR   FROM CAB-02.                                 
                                                                         
            IF WS-FSCADSOC1   EQUAL '10'                                 
               GO TO          0220-99-CABECALHO-PARTE1-EXIT              
            END-IF.                                                      
                                                                         
            MOVE  SPACES    TO REG-ATR.                                  
            WRITE REG-ATR.                                               
            WRITE REG-ATR.                                               
            MOVE  7         TO WS-CTLIN.                                 
       *----------------------------------------------------------------*
        0220-99-CABECALHO-PARTE1-EXIT.      EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0230-CABECALHO-PARTE2               SECTION.                     
       *----------------------------------------------------------------*
	    WRITE REG-ATR FROM CAB-03.                                   
            ADD   2       TO   WS-CTLIN.                                 
       *----------------------------------------------------------------*
        0230-99-CABECALHO-PARTE2-EXIT.      EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0240-IMPRIMA-DETALHE                SECTION.                     
       *----------------------------------------------------------------*
            MOVE NUMERO-SOCIO1    TO NUM.                                
            MOVE NOME-SOCIO1      TO NOME.                               
            MOVE VALOR-PAGAMENTO1 TO VAL.                                
            WRITE REG-ATR         FROM DETALHE.                          
                                                                         
            IF WS-FSRELSOCIO   NOT EQUAL '00'                            
               DISPLAY 'NAO FOI POSSIVEL GRAVAR EM RELSOCIO'             
               DISPLAY 'FILE STATUS = '     WS-FSRELSOCIO                
               STOP RUN                                                  
            END-IF.
		                                                                       
            ADD   1       TO WS-CTLIN.                                   
       *----------------------------------------------------------------*
        0240-IMPRIMA-DETALHE-EXIT.          EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0300-FINALIZA                       SECTION.                     
       *----------------------------------------------------------------*
            IF WS-CTLIN GREATER 36                                       
               MOVE SPACES TO REG-ATR                                    
               WRITE   REG-ATR                                           
               PERFORM 0220-CABECALHO-PARTE1                             
            END-IF                                                       
                                                                         
            PERFORM 0310-IMPRIMA-TOT.                                    
                                                                         
            CLOSE   CADSOC1.
            IF WS-FSCADSOC1  NOT EQUAL '00'                              
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO CADSOC1'           
               DISPLAY 'FILE STATUS = '     WS-FSCADSOC1                 
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            CLOSE   CADSOC2.                                             
                                                                         
            IF WS-FSCADSOC2  NOT EQUAL  '00'                             
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO CADSOC2'           
               DISPLAY 'FILE STATUS = '     WS-FSCADSOC2                 
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            CLOSE   RELSOCIO.                                            
                                                                         
            IF WS-FSRELSOCIO NOT EQUAL  '00'                             
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO ARQSAI'
	       DISPLAY 'FILE STATUS = '     WS-FSRELSOCIO                
               STOP RUN                                                  
            END-IF.                                                      
                                                                         
            DISPLAY '                                                 '. 
            DISPLAY '*************************************************'. 
            DISPLAY '         FIM DO PROGRAMA - THE END               '. 
            DISPLAY '*************************************************'. 
                                                                         
            PERFORM 0340-RESUMO-GERAL.                                   
                                                                         
       *----------------------------------------------------------------*
        0300-99-FINALIZA-EXIT.              EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0310-IMPRIMA-TOT                    SECTION.                     
       *----------------------------------------------------------------*
            MOVE  SPACES          TO   REG-ATR.
	    WRITE REG-ATR.                                               
            WRITE REG-ATR.                                               
            MOVE  WS-TOTAL-SOCIOS TO   TOT-SOCIO.                        
            MOVE  WS-VALOR-TOTAL  TO   MASC-VAL-T.                       
                                                                         
            WRITE REG-ATR  FROM TOT-01.                                  
            WRITE REG-ATR  FROM TOT-02.                                  
                                                                         
       *----------------------------------------------------------------*
        0310-99-IMPRIMA-TOT-EXIT.           EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0330-IMPRIMA-ARQ-VAZIO              SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 0220-CABECALHO-PARTE1                                
            WRITE REG-ATR                   FROM DETALHE-ARQ-VAZIO.      
       *---------------------------------------------------------------- 
        0330-99-IMPRIMA-ARQ-VAZIO-EXIT.     EXIT.
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0340-RESUMO-GERAL                   SECTION.                    
       *----------------------------------------------------------------
       * RESULTADO DISPONIVEL NA SAIDA SYSOUT                           
                                                                        
            DISPLAY '*************************************************'.
            DISPLAY '       RESUMO/BALANCO GERAL DO PROGRAMA          '.
            DISPLAY '*************************************************'.
            DISPLAY '*************************************************'.
            DISPLAY '**** REGISTROS LIDOS    =   ' WS-QTDE-LIDOS       .
            DISPLAY '**** REGISTROS GERADOS  =   ' WS-QTDE-GRAVADOS    .
            DISPLAY '*************************************************'.
            DISPLAY '*************************************************'.
       *----------------------------------------------------------------
        0310-RESUMO-GERAL-EXIT.             EXIT.                       
       *----------------------------------------------------------------                                                                                                           
                                                                                                                                                                                                                                                                                                       
                                                                                                                                    
