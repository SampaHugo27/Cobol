	IDENTIFICATION                      DIVISION.                   
       *----------------------------------------------------------------
        PROGRAM-ID.     EXRELATO.                                       
        AUTHOR.         HUGO SAMPAIO.                                   
        INSTALLATION.   QINTESS.                                        
        DATE-WRITTEN.                                                   
        DATE-COMPILED.                                                  
                                                                        
       *REMARKS.        O PROGRAMA LE OS REGISTROS DO ARQUIVO DE ENTRADA
       *                ARQENT, EXIBINDO O TOTAL DE GASTOS DE UM DADO   
       *                CLIENTE USANDO COMO CHAVE DE QUEBRA O CAMPO CPF 
       *                E CAMPO VALOR GASTO PARA SUMARIZAR. LOGO EM     
       *                SEGUIDA, SAO GRAVADOS PARA CADA REGISTRO DE     
       *                CLIENTE SEUS DADOS QUE SAO CPF, NOME-CLIENTE,   
       *                E VALOR-TOTAL SUMARIZADO POR CLIENTE NO ARQUIVO 
       *                DE SAIDA ARQSAI E DEPOIS ESSES MESMOS DADOS SAO 
       *                POSTERIORMENTE GRAVADOS EM UM RELATORIO.        
       *                                                    
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
            SELECT ARQENT  ASSIGN TO ARQENT                             
            ORGANIZATION   IS  SEQUENTIAL                               
            ACCESS MODE    IS  SEQUENTIAL                               
            FILE STATUS    IS  WS-FSARQENT.                             
       *----------------------------------------------------------------
            SELECT ARQSAI  ASSIGN TO ARQSAI     
		          ORGANIZATION   IS  SEQUENTIAL                               
            ACCESS MODE    IS  SEQUENTIAL                               
            FILE STATUS    IS  WS-FSARQSAI.                             
       *----------------------------------------------------------------
            SELECT RELATO ASSIGN TO RELATO                              
            FILE STATUS    IS  WS-FSRELATO.                             
       *----------------------------------------------------------------
        DATA                                DIVISION.                   
       *----------------------------------------------------------------
        FILE                                SECTION.                    
       *----------------------------------------------------------------
        FD  ARQENT                                                      
            LABEL RECORD   ARE STANDARD                                 
            RECORD         CONTAINS 60 CHARACTERS                       
            DATA RECORD    IS REG-ENT                                   
            RECORDING MODE IS F.                                        
                                                                        
        01  REG-ENT.                                                    
	    05  CPF-ENT                     PIC 9(11).                  
            05  NOME-ENT                    PIC X(30).                  
            05  VALOR-GASTO                 PIC 9(15)V99.               
            05  FILLER                      PIC X(02).                  
       *----------------------------------------------------------------
        FD  ARQSAI                                                      
            LABEL RECORD   ARE STANDARD                                 
            RECORD         CONTAINS 60 CHARACTERS                       
            DATA RECORD    IS REG-SAI                                   
            RECORDING MODE IS F.                                        
                                                                        
        01  REG-SAI.                                                    
            05  CPF-SAI                     PIC 9(11).                  
            05  NOME-SAI                    PIC X(30).                  
            05  VALOR-TOTAL                 PIC 9(15)V99.               
            05  FILLER                      PIC X(02).                  
       *----------------------------------------------------------------
        FD  RELATO                                             
            LABEL RECORD IS OMITTED.                                    
                                                                        
        01 REG-ATR                          PIC X(80).                  
       *----------------------------------------------------------------
        WORKING-STORAGE                     SECTION.                    
       *----------------------------------------------------------------
        77 WS-FSARQENT                      PIC X(02) VALUE '00'.       
        77 WS-FSARQSAI                      PIC X(02) VALUE '00'.       
        77 WS-FSRELATO                      PIC X(02) VALUE '00'.       
        77 WS-TOTAL-GASTO                   PIC 9(15)V99  VALUE ZEROS.  
        77 WS-TOTAL-RELAT                   PIC 9(15)V99  VALUE ZEROS.  
                                                                        
        77 WS-CHAVE-QUEBRA-ANT              PIC 9(11)    VALUE ZEROS.   
        77 WS-CHAVE-QUEBRA-ATU              PIC 9(11)    VALUE ZEROS.   
        77 WS-QTDE-REG-GERADOS              PIC 9(03)    VALUE ZEROS.   
        77 WS-QTDE-REG-LIDOS                PIC 9(03)    VALUE ZEROS.   
        77 WS-CT-LIN                        PIC 99       VALUE 50.      
        77 WS-CT-PAG                        PIC 999      VALUE ZEROS.
		                                                                      
       *---------------------------------------------------------------*
       *         LAYOUT DO RELATORIO - AREA DE CABEÇALHOS              *
       *---------------------------------------------------------------*
        01 CAB-01.                                                      
           05 FILLER        PIC X(29) VALUE SPACES.                     
           05 FILLER        PIC X(16) VALUE 'BRADESCO CARTOES'.         
           05 FILLER        PIC X(19) VALUE SPACES.                     
           05 FILLER        PIC X(06) VALUE 'DATA: '.                   
           05 DIA           PIC 99.                                     
           05 FILLER        PIC X(01) VALUE '/'.                        
           05 MES           PIC 99.                                     
           05 FILLER        PIC X(01) VALUE '/'.                        
           05 ANO           PIC 9999.                                   
       *----------------------------------------------------------------
        01 CAB-02.                                                      
           05 FILLER       PIC X(01) VALUE SPACES.                      
           05 FILLER       PIC X(08) VALUE 'PVRB636S'.                  
           05 FILLER       PIC X(12) VALUE SPACES.                      
           05 FILLER       PIC X(25) VALUE 'RELATORIO TOTALIZADOR DE '. 
           05 FILLER       PIC X(06) VALUE 'GASTOS'.                    
           05 FILLER       PIC X(17) VALUE SPACES.                      
           05 FILLER       PIC X(08) VALUE 'PAGINA: '.                  
           05 PAGINA       PIC 999.                                     
       *----------------------------------------------------------------
        01 CAB-03.                                                      
           05 FILLER       PIC X(80) VALUE ALL '-'.                     
       *----------------------------------------------------------------
        01 CAB-04.                                                      
           05 FILLER       PIC X(01) VALUE SPACES.                      
           05 FILLER       PIC X(03) VALUE 'CPF'.                       
           05 FILLER       PIC X(15) VALUE SPACES.                      
           05 FILLER       PIC X(04) VALUE 'NOME'.                      
           05 FILLER       PIC X(41) VALUE SPACES.                      
           05 FILLER       PIC X(11) VALUE 'VALOR TOTAL'.               
		         05 FILLER       PIC X(05) VALUE SPACES.                      
       *---------------------------------------------------------------*
       *         LAYOUT DO RELATORIO - AREA DE DETALHE                 *
       *---------------------------------------------------------------*
        01 DETALHE.                                                     
           05 FILLER        PIC X(01) VALUE SPACES.                     
           05 CPF-ONZE      PIC 9(09).                                  
           05 FILLER        PIC X(01) VALUE '-'.                        
           05 CPF-DOIS      PIC 9(02).                                  
           05 FILLER        PIC X(06) VALUE SPACES.                     
           05 NOME          PIC X(30).                                  
           05 FILLER        PIC X(04).                                  
           05 VALOR-TOT-C   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.    
           05 FILLER        PIC X(05) VALUE SPACES.                     
       *---------------------------------------------------------------*
        01 DETALHE-VAZIO.                                               
           05 FILLER        PIC X(01) VALUE SPACES.                     
           05 FILLER        PIC X(24) VALUE 'ARQUIVO DE ENTRADA VAZIO'. 
	   05 FILLER        PIC X(55) VALUE SPACES.                      
       *---------------------------------------------------------------* 
       *         LAYOUT DO RELATORIO - AREA DE TOTALIZADORES           * 
       *---------------------------------------------------------------* 
        01 TOT-01.                                                       
           05 FILLER        PIC X(01) VALUE SPACES.                      
           05 FILLER        PIC X(26) VALUE 'TOTAL REGISTROS IMPRESSOS:'.
           05 FILLER        PIC X(10) VALUE SPACES.                      
           05 TOT-REG       PIC Z.ZZZ.ZZZ.ZZ9  VALUE ZEROS.              
           05 FILLER        PIC X(30).                                   
       *---------------------------------------------------------------* 
        01 TOT-02.                                                       
           05 FILLER        PIC X(01) VALUE SPACES.                      
           05 FILLER        PIC X(26) VALUE 'VALOR TOTAL SUMARIZADO...:'.
           05 FILLER        PIC X(01) VALUE SPACE.                       
           05 VALOR-TOT-F   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.     
       *---------------------------------------------------------------* 
       *---------------------------------------------------------------*
		      PROCEDURE                           DIVISION.                   
       *---------------------------------------------------------------*
        0000-PRINCIPAL                      SECTION.                    
       *----------------------------------------------------------------
            PERFORM 0100-INICIO       THRU  0100-99-INICIO-EXIT.        
            PERFORM 0200-PROCESSA     THRU  0200-99-PROCESSA-EXIT       
                                      UNTIL WS-FSARQENT EQUAL '10'.     
            PERFORM 0300-FINALIZA     THRU  0300-99-FINALIZA-EXIT.      
            STOP RUN.                                                   
       *----------------------------------------------------------------
        0000-99-PRINCIPAL-EXIT.       EXIT.                             
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0100-INICIO                         SECTION.                    
       *----------------------------------------------------------------
            DISPLAY '*************************************************'.
            DISPLAY '           INICIANDO O PROGRAMA...               '.
            DISPLAY '*************************************************'.
            DISPLAY '                                                 '.
                                                                        
            OPEN INPUT  ARQENT.                                         
            IF WS-FSARQENT NOT EQUAL  '00'                              
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQENT'  
               DISPLAY 'FILE STATUS = '     WS-FSARQENT                 
               STOP RUN                                                 
            END-IF.                                                     
                                                                        
            OPEN OUTPUT  ARQSAI.                                        
            IF WS-FSARQSAI NOT EQUAL  '00'                              
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO DE SAIDA ARQSAI'    
               DISPLAY 'FILE STATUS = '     WS-FSARQSAI                 
               STOP RUN                                                 
            END-IF.                                                     
                                                                        
            OPEN OUTPUT  RELATO.                                        
            IF WS-FSRELATO NOT EQUAL  '00' 
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO RELATORIO'          
               DISPLAY 'FILE STATUS = '     WS-FSRELATO                 
               STOP RUN                                                 
            END-IF.                                                     
                                                                        
            PERFORM 0220-LEITURA.                                       
                                                                        
            IF WS-QTDE-REG-LIDOS  GREATER 0                             
               MOVE WS-CHAVE-QUEBRA-ATU  TO WS-CHAVE-QUEBRA-ANT         
            ELSE                                                        
               DISPLAY '***********************************************'
               DISPLAY '********** ARQUIVO DE ENTRADA VAZIO ***********'
               DISPLAY '***********************************************'
               MOVE    ZEROS             TO WS-CT-LIN                   
               PERFORM 0330-IMPRIMA-ARQ-VAZIO                           
                                                                        
            END-IF.    
		                                                                      
       *----------------------------------------------------------------
        0100-99-INICIO-EXIT.                EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0200-PROCESSA                       SECTION.                    
       *----------------------------------------------------------------
            IF WS-CHAVE-QUEBRA-ATU       NOT EQUAL  WS-CHAVE-QUEBRA-ANT 
               PERFORM 0230-GRAVACAO                                    
               PERFORM 0260-IMPRIMA-DET                                 
               MOVE WS-CHAVE-QUEBRA-ATU     TO WS-CHAVE-QUEBRA-ANT      
               ADD  WS-TOTAL-GASTO          TO WS-TOTAL-RELAT           
               MOVE  ZEROS                  TO WS-TOTAL-GASTO           
                                                                        
            END-IF.                                                     
                                                                        
       * MOVIMENTACAO PARA O ARQUIVO ARQSAI                             
                                                                       
            MOVE CPF-ENT                    TO CPF-SAI                   
            MOVE NOME-ENT                   TO NOME-SAI                  
                                                                         
            ADD VALOR-GASTO                 TO WS-TOTAL-GASTO.           
            MOVE WS-TOTAL-GASTO             TO VALOR-TOTAL.              
                                                                         
       * MOVIMENTACAO PARA O ARQUIVO RELATO                              
                                                                         
            IF WS-CT-LIN GREATER 49                                      
               IF PAGINA GREATER 0                                       
                  MOVE  SPACES              TO REG-ATR                   
                  WRITE REG-ATR                                          
               END-IF                                                    
                                                                         
               MOVE ZEROS TO WS-CT-LIN                                   
               PERFORM 0240-CABECALHO-PARTE1                             
               PERFORM 0250-CABECALHO-PARTE2                             
		          END-IF.                                                     
                                                                        
            MOVE  SPACES         TO         REG-ATR.                    
            MOVE  CPF-ENT(1:9)   TO         CPF-ONZE.                   
            MOVE  CPF-ENT(10:2)  TO         CPF-DOIS.                   
            MOVE  NOME-ENT       TO         NOME.                       
            MOVE  WS-TOTAL-GASTO TO         VALOR-TOT-C.                
                                                                        
            PERFORM 0220-LEITURA.                                       
                                                                        
       *----------------------------------------------------------------
        0200-99-PROCESSA-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0220-LEITURA                        SECTION.                    
       *----------------------------------------------------------------
            READ ARQENT.                                                
	    IF   WS-FSARQENT = '10'                                     
                 GO TO 0220-99-LEITURA-EXIT                             
            END-IF.                                                     
                                                                        
            MOVE CPF-ENT         TO         WS-CHAVE-QUEBRA-ATU.        
            ADD  1               TO         WS-QTDE-REG-LIDOS.          
       *----------------------------------------------------------------
        0220-99-LEITURA-EXIT.               EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0230-GRAVACAO                       SECTION.                    
       *----------------------------------------------------------------
            WRITE REG-SAI.                                              
                                                                        
            IF WS-FSARQSAI NOT EQUAL '00'                               
               DISPLAY 'NAO FOI POSSIVEL GRAVAR EM ARQSAI'              
               DISPLAY 'FILE STATUS = '     WS-FSARQSAI                 
               STOP RUN
	    END-IF                                                      
                                                                        
            ADD   1                   TO    WS-QTDE-REG-GERADOS.        
       *----------------------------------------------------------------
        0230-99-GRAVACAO-EXIT.              EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0240-CABECALHO-PARTE1               SECTION.                    
       *----------------------------------------------------------------
       * CABECALHO "PADRAO"                                             
                                                                        
            ADD   1                         TO     WS-CT-PAG.           
            MOVE  WS-CT-PAG                 TO     PAGINA.              
            MOVE FUNCTION CURRENT-DATE(7:2) TO     DIA.                 
            MOVE FUNCTION CURRENT-DATE(5:2) TO     MES.                 
            MOVE FUNCTION CURRENT-DATE(1:4) TO     ANO.                 
            WRITE REG-ATR                   FROM   CAB-01.              
            WRITE REG-ATR                   FROM   CAB-02. 
		          WRITE REG-ATR                   FROM   CAB-03.              
            MOVE  SPACES                    TO     REG-ATR.             
            WRITE REG-ATR.                                              
       *----------------------------------------------------------------
        0240-99-CABECALHO-PARTE1-EXIT.      EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0250-CABECALHO-PARTE2               SECTION.                    
       *----------------------------------------------------------------
       * MONTAGEM DO CABECALHO USANDO CAMPOS CPF, NOME E VALOR TOTAL    
                                                                        
            WRITE REG-ATR                   FROM   CAB-04.              
            MOVE  5                         TO     WS-CT-LIN.           
       *----------------------------------------------------------------
        0250-99-CABECALHO-PARTE2-EXIT.      EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0260-IMPRIMA-DET                    SECTION.                    
       *----------------------------------------------------------------
       * IMPRIMA A LINHA DE DETALHE (REGISTRO)                          
                                                                        
            WRITE REG-ATR        FROM       DETALHE.                    
                                                                        
            IF WS-FSRELATO NOT EQUAL  '00'                              
               DISPLAY 'NAO FOI POSSIVEL GRAVAR EM RELATO'              
               DISPLAY 'FILE STATUS = '     WS-FSRELATO                 
               STOP RUN                                                 
            END-IF.                                                     
                                                                        
            ADD   1              TO         WS-CT-LIN.                  
       *----------------------------------------------------------------
        0260-IMPRIMA-DET-EXIT.              EXIT.                       
       *----------------------------------------------------------------
        0300-FINALIZA                       SECTION.                    
       *----------------------------------------------------------------
            IF WS-QTDE-REG-LIDOS  GREATER 0       
       	       PERFORM 0230-GRAVACAO                                    
               PERFORM 0260-IMPRIMA-DET                                 
               ADD  WS-TOTAL-GASTO          TO WS-TOTAL-RELAT           
            END-IF.                                                     
                                                                        
            IF WS-CT-LIN GREATER 47                                     
               MOVE SPACES TO REG-ATR                                   
               WRITE REG-ATR                                            
               PERFORM 0240-CABECALHO-PARTE1                            
            END-IF.                                                     
                                                                        
            PERFORM 0320-IMPRIMA-TOT.                                   
                                                                        
            CLOSE ARQENT.                                               
                                                                        
            IF WS-FSARQENT NOT EQUAL  '00'                              
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO ARQENT'           
               DISPLAY 'FILE STATUS = '     WS-FSARQENT 
		             STOP RUN                                                 
            END-IF.                                                     
                                                                        
            CLOSE ARQSAI.                                               
            IF WS-FSARQSAI NOT EQUAL  '00'                              
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO ARQSAI'           
               DISPLAY 'FILE STATUS = '     WS-FSARQSAI                 
               STOP RUN                                                 
            END-IF.                                                     
                                                                        
            CLOSE RELATO.                                               
            IF WS-FSRELATO NOT EQUAL  '00'                              
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO RELATORIO'        
               DISPLAY 'FILE STATUS = '     WS-FSRELATO                 
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
            DISPLAY '**** REGISTROS LIDOS    =   ' WS-QTDE-REG-LIDOS   .  
            DISPLAY '**** REGISTROS GERADOS  =   ' WS-QTDE-REG-GERADOS .
            DISPLAY '*************************************************'.
            DISPLAY '*************************************************'.
       *----------------------------------------------------------------
        0310-RESUMO-GERAL-EXIT.             EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------
        0320-IMPRIMA-TOT                    SECTION.                    
       *----------------------------------------------------------------
            MOVE  SPACES                    TO REG-ATR.                 
            WRITE REG-ATR.                                              
            MOVE  WS-QTDE-REG-GERADOS       TO   TOT-REG.               
            WRITE REG-ATR                   FROM TOT-01.                
            MOVE  WS-TOTAL-RELAT            TO   VALOR-TOT-F.           
            WRITE REG-ATR                   FROM TOT-02.                
       *----------------------------------------------------------------
        0320-IMPRIMA-TOT-EXIT.              EXIT.                       
       *----------------------------------------------------------------    
       *----------------------------------------------------------------
        0330-IMPRIMA-ARQ-VAZIO              SECTION.                    
       *----------------------------------------------------------------
            PERFORM 0240-CABECALHO-PARTE1                               
            WRITE REG-ATR                   FROM DETALHE-VAZIO.         
       *----------------------------------------------------------------
        0330-IMPRIMA-ARQ-VAZIO-EXIT.        EXIT.                       
       *----------------------------------------------------------------
       *----------------------------------------------------------------          
                                                                                                                                                                                                                
