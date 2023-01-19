	IDENTIFICATION                      DIVISION.                    
       *----------------------------------------------------------------*
        PROGRAM-ID.            EM02EX02.                                 
        AUTHOR.                HUGO SAMPAIO.                             
        INSTALLATION.          QINTESS.                                  
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
                                                                         
            SELECT CADCLI ASSIGN TO CADCLI                               
            ORGANIZATION  IS SEQUENTIAL                                  
            ACCESS MODE   IS SEQUENTIAL                                  
            FILE STATUS   IS WS-FSCADCLI.                                
       *----------------------------------------------------------------*
            SELECT RELCLI ASSIGN TO RELCLI                               
            FILE STATUS   IS WS-FSRELCLI.                                
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        DATA                                DIVISION.                    
       *----------------------------------------------------------------*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD  CADCLI                                                       
            LABEL  RECORD   ARE STANDARD
	    RECORD CONTAINS 89  CHARACTERS                               
            DATA   RECORD   IS  REG-ENT                                  
            RECORDING MODE  IS  F.                                       
                                                                         
        01  REG-ENT.                                                     
            05 CPF-ENT                      PIC 9(11).                   
            05 NOME-CLIENTE                 PIC X(30).                   
            05 ESTADO                       PIC X(02).                   
            05 CIDADE                       PIC X(30).                   
            05 TELEFONE                     PIC 9(08).                   
            05 DATA-ULTIMA-VISITA           PIC 9(08).                   
       *----------------------------------------------------------------*
        FD  RELCLI                                                       
            LABEL  RECORD   IS  OMITTED.                                 
                                                                         
        01  REG-ATR                         PIC X(80).                   
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.
       *----------------------------------------------------------------*
        77  WS-CTLIN                        PIC 9(02) VALUE 36.          
        77  WS-CTPAG                        PIC 9(03) VALUE ZEROS.       
        77  WS-TOTAL-CLI                    PIC 9(04) VALUE ZEROS.       
                                                                         
        77  WS-QTDE-LIDOS                   PIC 9(03) VALUE ZEROS.       
        77  WS-QTDE-GRAVADOS                PIC 9(03) VALUE ZEROS.       
        77  WS-FSCADCLI                     PIC X(02) VALUE '00'.        
        77  WS-FSRELCLI                     PIC X(02) VALUE '00'.        
       *----------------------------------------------------------------*
       *           LAYOUT DO RELATORIO - AREA DE CABECALHOS             *
       *----------------------------------------------------------------*
        01  CAB-01.                                                      
            05 FILLER        PIC X(70) VALUE SPACES.                     
            05 FILLER        PIC X(05) VALUE 'PAG. '.                    
            05 VAR-PAG       PIC 99.                                     
            05 FILLER        PIC X(03) VALUE SPACES.                     
       *----------------------------------------------------------------*
	01  CAB-02.                                                      
            05 FILLER        PIC X(31) VALUE SPACES.                     
            05 FILLER        PIC X(17) VALUE 'TURISMAR TURISMOS'.        
            05 FILLER        PIC X(32) VALUE SPACES.                     
       *----------------------------------------------------------------*
        01  CAB-03.                                                      
            05 FILLER        PIC X(22) VALUE SPACES.                     
            05 FILLER        PIC X(35)                                   
                             VALUE 'CLIENTES NO PERIODO DE: 2010 A 2011'.
            05 FILLER        PIC X(23) VALUE SPACES.                     
       *----------------------------------------------------------------*
        01  CAB-04.                                                      
            05 FILLER        PIC X(12) VALUE SPACES.                     
            05 FILLER        PIC X(04) VALUE 'NOME'.                     
            05 FILLER        PIC X(26) VALUE SPACES.                     
            05 FILLER        PIC X(06) VALUE 'ESTADO'.                   
            05 FILLER        PIC X(11) VALUE SPACES.                     
            05 FILLER        PIC X(08) VALUE 'TELEFONE'.
            05 FILLER        PIC X(13) VALUE SPACES.                     
       *----------------------------------------------------------------*
        01  CAB-05.                                                      
            05 FILLER        PIC X(80) VALUE ALL '-'.                    
       *----------------------------------------------------------------*
       *           LAYOUT DO RELATORIO - AREA DE DETALHES               *
       *----------------------------------------------------------------*
        01  DETALHE.                                                     
            05 FILLER        PIC X(06) VALUE SPACES.                     
            05 NOME          PIC X(30).                                  
            05 FILLER        PIC X(08) VALUE SPACES.                     
            05 EST           PIC X(02).                                  
            05 FILLER        PIC X(13) VALUE SPACES.                     
            05 TEL           PIC X(09) VALUE 'XXXX-XXXX'.                
            05 FILLER        PIC X(08) VALUE SPACES.                     
       *----------------------------------------------------------------*
        01  DETALHE-ARQ-VAZIO.                                           
	    05 FILLER        PIC X(01) VALUE SPACES.                     
            05 FILLER        PIC X(24) VALUE 'ARQUIVO DE ENTRADA VAZIO'. 
            05 FILLER        PIC X(55) VALUE SPACES.                     
       *----------------------------------------------------------------*
       *           LAYOUT DO RELATORIO - AREA DE TOTALIZADORES          *
       *----------------------------------------------------------------*
        01  TOT-01.                                                      
            05 FILLER        PIC X(05) VALUE SPACES.                     
            05 FILLER        PIC X(19) VALUE 'TOTAL DE CLIENTES: '.      
            05 TOTAL-CLI     PIC 9(05) VALUE ZEROS.                      
            05 FILLER        PIC X(51) VALUE SPACES.                     
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        PROCEDURE                           DIVISION.                    
       *----------------------------------------------------------------*
        0000-PRINCIPAL                      SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 0100-INICIO             THRU  0100-99-INICIO-EXIT.
	    PERFORM 0200-PROCESSA           THRU  0200-99-PROCESSA-EXIT  
                                            UNTIL WS-FSCADCLI EQUAL '10'.
            PERFORM 0300-FINALIZA           THRU  0300-99-FINALIZA-EXIT. 
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
                                                                         
            OPEN INPUT  CADCLI.                                          
                                                                         
            IF WS-FSCADCLI NOT EQUAL '00'
	       DISPLAY 'ERRO NO ARQUIVO DE ENTRADA CADCLI'               
               DISPLAY 'FILE STATUS =  '    WS-FSCADCLI                  
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN OUTPUT RELCLI.                                          
                                                                         
            IF WS-FSRELCLI NOT EQUAL '00'                                
               DISPLAY 'ERRO NO ARQUIVO DE SAIDA RELCLI'                 
               DISPLAY 'FILE STATUS =  '    WS-FSRELCLI                  
               STOP RUN                                                  
            END-IF.                                                      
                                                                         
            PERFORM 0210-LEITURA.                                        
                                                                         
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
            IF WS-CTLIN GREATER 35                                       
               PERFORM  0220-CABECALHO-PARTE1                            
               PERFORM  0230-CABECALHO-PARTE2                            
            END-IF.                                                      
                                                                         
            PERFORM 0240-IMPRIMA-DET.                                    
       *----------------------------------------------------------------*
        0200-99-PROCESSA-EXIT.              EXIT.
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0210-LEITURA                        SECTION.                     
       *----------------------------------------------------------------*
            READ CADCLI.                                                 
                                                                         
            IF   WS-FSCADCLI EQUAL '10'                                  
                 GO TO 0210-99-LEITURA-EXIT                              
            END-IF.                                                      
                                                                         
            ADD  1  TO WS-QTDE-LIDOS.                                    
       *----------------------------------------------------------------*
        0210-99-LEITURA-EXIT.               EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0220-CABECALHO-PARTE1               SECTION.                     
       *----------------------------------------------------------------*
            MOVE    SPACES     TO   REG-ATR.
	    WRITE   REG-ATR.                                             
            WRITE   REG-ATR.                                             
            ADD     1          TO   WS-CTPAG.                            
            MOVE    WS-CTPAG   TO   VAR-PAG.                             
            WRITE   REG-ATR    FROM CAB-01.                              
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR    FROM CAB-02.                              
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR.                                             
       *----------------------------------------------------------------*
        0220-99-CABECALHO-PARTE1-EXIT.      EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0230-CABECALHO-PARTE2               SECTION.                     
       *----------------------------------------------------------------*
            WRITE   REG-ATR    FROM CAB-03.                              
            WRITE   REG-ATR.
	    WRITE   REG-ATR.                                             
            WRITE   REG-ATR    FROM CAB-04.                              
            WRITE   REG-ATR    FROM CAB-05.                              
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR.                                             
            MOVE    15         TO   WS-CTLIN.                            
       *----------------------------------------------------------------*
        0230-99-CABECALHO-PARTE2-EXIT.      EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0240-IMPRIMA-DET                    SECTION.                     
       *----------------------------------------------------------------*
            MOVE  NOME-CLIENTE TO   NOME.                                
            MOVE  ESTADO       TO   EST.                                 
            MOVE  TELEFONE     TO   TEL.                                 
            WRITE REG-ATR      FROM DETALHE.                             
            ADD   1            TO   WS-CTLIN.                            
            ADD   1            TO   WS-TOTAL-CLI.
		                                                                       
            IF WS-FSRELCLI NOT EQUAL '00'                                
               DISPLAY 'ERRO NA IMPRESSAO DO DETALHE DO ARQUIVO RELCLI'  
               DISPLAY 'FILE STATUS = '     WS-FSRELCLI                  
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
        0240-99-IMPRIMA-DET-EXIT.           EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0300-FINALIZA                       SECTION.                     
       *----------------------------------------------------------------*
            IF WS-CTLIN GREATER 36                                       
               PERFORM  0220-CABECALHO-PARTE1                            
            END-IF.                                                      
                                                                         
            PERFORM 0320-IMPRIMA-TOT.                                    
            CLOSE CADCLI.
		                                                                       
            IF WS-FSCADCLI NOT EQUAL '00'                                
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA CADCLI' 
               DISPLAY 'FILE STATUS = '     WS-FSCADCLI                  
               STOP RUN                                                  
            END-IF.                                                      
       *----------------------------------------------------------------*
            CLOSE RELCLI.                                                
                                                                         
            IF WS-FSRELCLI NOT EQUAL '00'                                
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO'      
               DISPLAY 'FILE STATUS = '     WS-FSRELCLI                  
               STOP RUN                                                  
            END-IF.                                                      
                                                                         
            DISPLAY '                                                 '. 
            DISPLAY '*************************************************'. 
            DISPLAY '         FIM DO PROGRAMA - THE END               '. 
	    DISPLAY '*************************************************'.
                                                                        
            PERFORM 0310-RESUMO-GERAL.                                  
       *----------------------------------------------------------------
        0300-99-FINALIZA-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0310-RESUMO-GERAL                   SECTION.                    
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
       *----------------------------------------------------------------*
        0310-99-RESUMO-GERAL-EXIT.          EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0320-IMPRIMA-TOT                    SECTION.                     
       *----------------------------------------------------------------*
            MOVE  WS-TOTAL-CLI TO   TOTAL-CLI.                           
            WRITE REG-ATR      FROM TOT-01.                              
       *----------------------------------------------------------------*
        0320-99-IMPRIMA-TOT-EXIT.           EXIT.                        
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        0330-IMPRIMA-ARQ-VAZIO              SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 0220-CABECALHO-PARTE1.                               
            WRITE   REG-ATR FROM DETALHE-ARQ-VAZIO.                      
       *----------------------------------------------------------------*
        0330-IMPRIMA-ARQ-VAZIO-EXIT.        EXIT.                        
       *----------------------------------------------------------------*                                                                                                                     
                 
