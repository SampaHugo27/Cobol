	IDENTIFICATION                      DIVISION.                    
       *================================================================*
        PROGRAM-ID.          BLEX02.                                     
        AUTHOR.              HUGO SAMPAIO.                               
        INSTALLATION.        QINTESS.                                    
       *================================================================*
       *----------------------------------------------------------------*
       * OBJETIVO: IDENTIFICAR TODOS OS CLIENTES QUE ESTAO COM          *
       * RESTRICOES NO SERASA UTILIZANDO BALANCE-LINE 1 PARA N.         *
       *----------------------------------------------------------------*
       *================================================================*
        ENVIRONMENT                         DIVISION.                    
       *================================================================*
        CONFIGURATION                       SECTION.                     
       *----------------------------------------------------------------*
        SOURCE-COMPUTER.                    IBM-PC.                      
        OBJECT-COMPUTER.                    IBM-PC.                      
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.
       *----------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.                     
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
                                                                         
            SELECT ARQENT         ASSIGN TO ARQENT                       
            FILE STATUS           IS        WS-FS-ENT.                   
                                                                         
            SELECT SERASA         ASSIGN TO SERASA                       
            FILE STATUS           IS        WS-FS-SER.                   
                                                                         
            SELECT ARQSAI         ASSIGN TO ARQSAI                       
            FILE STATUS           IS        WS-FS-SAI.                   
                                                                         
            SELECT RELATO         ASSIGN TO RELATO                       
            FILE STATUS           IS        WS-FS-REL.                   
       *================================================================*
        DATA                                DIVISION.
       *================================================================*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD  ARQENT                          RECORDING MODE F.            
                                                                         
        01  REG-ENT.                                                     
            05 CPF-ENT                      PIC 9(11).                   
            05 NOME-CLIENTE-ENT             PIC X(30).                   
            05 VALOR-GASTO                  PIC 9(15)V99.                
            05 CONTA-ENT                    PIC 9(10).                   
            05 FILLER                       PIC X(02).                   
       *----------------------------------------------------------------*
        FD  SERASA                          RECORDING MODE F.            
                                                                         
        01  REG-SER.                                                     
            05 CPF-SER                      PIC 9(11).                   
            05 NOME-CLIENTE-SER             PIC X(30).                   
            05 TOTAL-DIVIDA-SER             PIC 9(15)V99.
	    05 FILLER                       PIC X(02).                   
       *----------------------------------------------------------------*
        FD  ARQSAI                          RECORDING MODE F.            
                                                                         
        01  REG-SAI.                                                     
            05 CPF-SAI                      PIC 9(11).                   
            05 NOME-CLIENTE-SAI             PIC X(30).                   
            05 TOTAL-DIVIDA-SAI             PIC 9(15)V99.                
            05 CONTA-SAI                    PIC 9(10).                   
            05 FILLER                       PIC X(02).                   
       *----------------------------------------------------------------*
        FD  RELATO                                                       
            LABEL RECORD   IS OMITTED.                                   
                                                                         
        01  REG-ATR                         PIC X(80).                   
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.                     
       *----------------------------------------------------------------*
       *                   DECLARACAO DE VARIAVEIS                      *
       *----------------------------------------------------------------*
       *                   VARIAVEIS DE FILE-STATUS                     *
       *----------------------------------------------------------------*
        77  WS-FS-ENT                       PIC X(02)     VALUE '00'.    
        77  WS-FS-SER                       PIC X(02)     VALUE '00'.    
        77  WS-FS-SAI                       PIC X(02)     VALUE '00'.    
        77  WS-FS-REL                       PIC X(02)     VALUE '00'.    
       *----------------------------------------------------------------*
       *                         CONTADORES                             *
       *----------------------------------------------------------------*
        77  WS-CT-LIN                       PIC 99        VALUE 51.      
        77  WS-CT-PAG                       PIC 999       VALUE ZEROS.   
        77  WS-QTDE-BATIMENTOS              PIC 999       VALUE ZEROS.   
        77  WS-QTDE-REG-ENT                 PIC 9(03)     VALUE ZEROS.   
        77  WS-QTDE-REG-SAI                 PIC 9(03)     VALUE ZEROS.   
        77  WS-QTDE-REG-SER                 PIC 9(03)     VALUE ZEROS.   
        77  WS-QTDE-REG-REL                 PIC 9(03)     VALUE ZEROS.
       *----------------------------------------------------------------*
       *                            FLAG                                *
       *----------------------------------------------------------------*
        77  WS-FLAG                         PIC 9         VALUE ZEROS.   
       *----------------------------------------------------------------*
       *                         ACUMULADORES                           *
       *----------------------------------------------------------------*
        77  WS-TOTAL-RELAT                  PIC 9(15)V99  VALUE ZEROS.   
       *----------------------------------------------------------------*
        01  WS-CPF-ENT-X.                                                
            05 WS-CPF-ENT                   PIC 9(11)     VALUE ZEROS.   
                                                                         
        01  WS-CPF-SER-X.                                                
            05 WS-CPF-SER                   PIC X(11)     VALUE ZEROS.   
       *----------------------------------------------------------------*
       *                       AREA DE ABEND                            *
       *----------------------------------------------------------------*
        77  WS-ABENDA                       PIC X(08)   VALUE 'ABENDA31'.
		      01  WS-AREA.                                                     
            05 WS-AREA-PGM                  PIC X(08)   VALUE 'VRB649S'. 
            05 WS-AREA-STAT                 PIC X(03)   VALUE SPACES.    
            05 WS-AREA-MSG                  PIC X(50)   VALUE SPACES.    
                                                                         
        01  WS-CODIGO-AREA                  PIC X(30)   VALUE SPACES.    
       *----------------------------------------------------------------*
       *         LAYOUT DO RELATORIO - AREA DE CABEÇALHOS               *
       *----------------------------------------------------------------*
        01  CAB-01.                                                      
            05 FILLER       PIC X(29) VALUE SPACES.                      
            05 FILLER       PIC X(16) VALUE 'BRADESCO CARTOES'.          
            05 FILLER       PIC X(19) VALUE SPACES.                      
            05 FILLER       PIC X(06) VALUE 'DATA: '.                    
            05 DIA          PIC 99.                                      
            05 FILLER       PIC X(01) VALUE '/'.                         
            05 MES          PIC 99.                                      
            05 FILLER       PIC X(01) VALUE '/'.
	    05 ANO          PIC 9999.                                    
       *----------------------------------------------------------------*
        01  CAB-02.                                                      
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(08) VALUE 'VRB649S '.                  
            05 FILLER       PIC X(08) VALUE SPACES.                      
            05 FILLER       PIC X(18) VALUE 'RELATORIO CLIENTES'.        
            05 FILLER       PIC X(18) VALUE ' COM RESTRICOES - '.        
            05 FILLER       PIC X(16) VALUE 'SERASA          '.          
            05 FILLER       PIC X(08) VALUE 'PAGINA: '.                  
            05 PAGINA       PIC 999.                                     
       *----------------------------------------------------------------*
        01  CAB-03.                                                      
            05 FILLER       PIC X(80)       VALUE ALL '-'.               
       *----------------------------------------------------------------*
        01 CAB-04.                                                       
           05 FILLER        PIC X(01) VALUE SPACES.                      
           05 FILLER        PIC X(03) VALUE 'CPF'.
	   05 FILLER        PIC X(15) VALUE SPACES.                      
           05 FILLER        PIC X(04) VALUE 'NOME'.                      
           05 FILLER        PIC X(34) VALUE SPACES.                      
           05 FILLER        PIC X(18) VALUE 'VALOR TOTAL DIVIDA'.        
           05 FILLER        PIC X(05) VALUE SPACES.                      
       *----------------------------------------------------------------*
       *            LAYOUT DO RELATORIO - AREA DE DETALHE               *
       *----------------------------------------------------------------*
        01  DETALHE.                                                     
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 CPF-NOVE     PIC 9(09).                                   
            05 FILLER       PIC X(01) VALUE '-'.                         
            05 CPF-DOIS     PIC 9(02).                                   
            05 FILLER       PIC X(06) VALUE SPACES.                      
            05 NOME         PIC X(30).                                   
            05 FILLER       PIC X(04).                                   
            05 VALOR-TOT-D  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.     
            05 FILLER       PIC X(05) VALUE SPACES.
       *----------------------------------------------------------------*
        01  DETALHE-ARQENT-VAZIO.                                        
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(26) VALUE 'ARQUIVO DE ENTRADA ARQENT '.
            05 FILLER       PIC X(08) VALUE 'VAZIO'.                     
            05 FILLER       PIC X(45) VALUE SPACES.                      
       *----------------------------------------------------------------*
        01  DETALHE-SERASA-VAZIO.                                        
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(26) VALUE 'ARQUIVO DE ENTRADA SERASA '.
            05 FILLER       PIC X(05) VALUE 'VAZIO'.                     
            05 FILLER       PIC X(45) VALUE SPACES.                      
       *----------------------------------------------------------------*
        01  DETALHE-ZERO-BATIMENTOS.                                     
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(19) VALUE 'NAO HA PESSOAS COM '.       
            05 FILLER       PIC X(19) VALUE 'RESTRICAO NO SERASA'.       
            05 FILLER       PIC X(41) VALUE SPACES.
       *----------------------------------------------------------------*
       *         LAYOUT DO RELATORIO - AREA DE TOTALIZADORES            *
       *----------------------------------------------------------------*
        01  TOT-01.                                                      
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(26) VALUE 'VALOR TOTAL DIVIDA........'.
            05 FILLER       PIC X(22) VALUE '.....................:'.    
            05 VALOR-TOT-F  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.     
            05 FILLER       PIC X(05) VALUE SPACES.                      
       *----------------------------------------------------------------*
        01  TOT-02.                                                      
            05 FILLER       PIC X(01) VALUE SPACES.                      
            05 FILLER       PIC X(26) VALUE 'TOTAL REGISTROS IMPRESSOS:'.
            05 FILLER       PIC X(10) VALUE SPACES.                      
            05 TOT-IMP      PIC Z.ZZZ.ZZZ.ZZ9.                           
            05 FILLER       PIC X(30) VALUE SPACES.                      
       *================================================================*
        PROCEDURE                           DIVISION.
       *================================================================*
        0000-PRINCIPAL                      SECTION.                     
       *================================================================*
            PERFORM 1000-INICIO             THRU  1000-99-INICIO-EXIT.   
            PERFORM 2000-PROCESSA           THRU  2000-99-PROCESSA-EXIT  
                                            UNTIL WS-FS-ENT EQUAL '10'   
                                            AND   WS-FS-SER EQUAL '10'.  
            PERFORM 5000-FIM                THRU  5000-99-FIM-EXIT.      
            STOP RUN.                                                    
       *----------------------------------------------------------------*
        0000-99-PRINCIPAL-EXIT.             EXIT.                        
       *================================================================*
        1000-INICIO                         SECTION.                     
       *================================================================*
            DISPLAY '**************************************************'.
            DISPLAY '           INICIANDO O PROGRAMA...                '.
            DISPLAY '**************************************************'.
            DISPLAY '                                                  '.
       *----------------------------------------------------------------*
       * ABERTURA DE ARQUIVOS E VERIFICACAO DE FILE STATUS              *
       *----------------------------------------------------------------*
            MOVE '1000-INICIO'              TO         WS-CODIGO-AREA.   
                                                                         
            OPEN  INPUT   ARQENT.                                        
            IF    WS-FS-ENT                 NOT EQUAL '00'               
                  DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA    
                  DISPLAY 'STATUS:  '                  WS-FS-ENT         
                  MOVE WS-FS-ENT            TO         WS-AREA-STAT      
                  MOVE 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQENT'   
                                            TO         WS-AREA-MSG       
                  CALL WS-ABENDA            USING      WS-AREA           
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN  INPUT   SERASA.                                        
            IF    WS-FS-ENT                 NOT EQUAL  '00'              
                  DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA
		  DISPLAY 'STATUS:  '                  WS-FS-SER         
                  MOVE WS-FS-SER            TO         WS-AREA-STAT      
                  MOVE 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQENT'   
                                            TO         WS-AREA-MSG       
                  CALL WS-ABENDA            USING      WS-AREA           
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN  OUTPUT   ARQSAI.                                       
            IF    WS-FS-SAI                 NOT EQUAL  '00'              
                  DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA    
                  DISPLAY 'STATUS:  '                  WS-FS-SAI         
                  MOVE  WS-FS-SAI           TO         WS-AREA-STAT      
                  MOVE  'ERRO NA ABERTURA DO ARQUIVO DE SAIDA ARQSAI'    
                                            TO         WS-AREA-MSG       
                  CALL  WS-ABENDA           USING      WS-AREA           
            END-IF.                                                      
       *----------------------------------------------------------------*
            OPEN  OUTPUT    RELATO.
	    IF    WS-FS-REL                 NOT EQUAL  '00'              
                  DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA    
                  DISPLAY 'STATUS:  '                  WS-FS-REL         
                  MOVE    WS-FS-REL         TO         WS-AREA-STAT      
                  MOVE    'ERRO NA ABERTURA DO ARQUIVO DE RELATORIO'     
                                            TO         WS-AREA-MSG       
                  CALL    WS-ABENDA         USING      WS-AREA           
            END-IF.                                                      
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
            PERFORM 2220-LEITURA-ARQENT.                                 
                                                                         
            IF WS-QTDE-REG-ENT  EQUAL 0                                  
               DISPLAY '************************************************'
               DISPLAY '******* ARQUIVO DE ENTRADA ARQENT VAZIO ********'
               DISPLAY '************************************************'
               PERFORM 5200-IMPRIMA-ARQENT-VAZIO                         
            END-IF.
       *----------------------------------------------------------------*
            PERFORM 2230-LEITURA-SERASA.                                 
                                                                         
            IF WS-QTDE-REG-SER  EQUAL 0                                  
               DISPLAY '************************************************'
               DISPLAY '******* ARQUIVO DE ENTRADA SERASA VAZIO ********'
               DISPLAY '************************************************'
               PERFORM 5300-IMPRIMA-SERASA-VAZIO                         
            END-IF.                                                      
       *----------------------------------------------------------------*
       * VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS                *
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1000-99-INICIO-EXIT.                EXIT.                        
       *================================================================*
        2000-PROCESSA                       SECTION.                     
       *================================================================*
       * VERIFICA SE CPF-ENT EH IGUAL A CPF-SER. CASO SEJA VERDADE,     *
       * GRAVA OS CAMPOS CPF, NOME, TOTAL-DIVIDA NOS ARQUIVOS ARQSAI E  *
       * RELATO.                                                        *
       *----------------------------------------------------------------*
            IF WS-CPF-ENT         =   WS-CPF-SER                         
               ADD    1      TO   WS-QTDE-BATIMENTOS                     
               PERFORM            3000-RELATORIO                         
               MOVE   1      TO   WS-FLAG                                
               PERFORM            2240-GRAVACAO-SAI                      
               PERFORM            2220-LEITURA-ARQENT                    
            ELSE                                                         
               IF WS-CPF-ENT      <   WS-CPF-SER                         
                  PERFORM         2220-LEITURA-ARQENT                    
                  MOVE ZEROS TO   WS-FLAG                                
               ELSE                                                      
                  IF WS-CPF-ENT   >   WS-CPF-SER                         
                     PERFORM      2230-LEITURA-SERASA                    
                     MOVE  ZEROS TO WS-FLAG                              
                  END-IF
	       END-IF                                                    
            END-IF.                                                      
       *----------------------------------------------------------------*
        2000-99-PROCESSA-EXIT.              EXIT.                        
       *================================================================*
        2240-GRAVACAO-SAI                   SECTION.                     
       *================================================================*
       * GRAVACAO EM ARQSAI                                             *
       *----------------------------------------------------------------*
            MOVE '2240-GRAVACAO'            TO    WS-CODIGO-AREA.        
                                                                         
            MOVE  CPF-ENT                   TO    CPF-SAI.               
            MOVE  NOME-CLIENTE-ENT          TO    NOME-CLIENTE-SAI.      
            MOVE  TOTAL-DIVIDA-SER          TO    TOTAL-DIVIDA-SAI.      
            MOVE  CONTA-ENT                 TO    CONTA-SAI.             
            WRITE REG-SAI.                                               
       *----------------------------------------------------------------*
       * VERIFICACAO DE FILE STATUS PARA GRAVACAO EM ARQSAI'            * 
       *----------------------------------------------------------------*
            IF WS-FS-SAI                    NOT EQUAL '00'               
               DISPLAY    'ERRO NO: '             WS-CODIGO-AREA         
               DISPLAY    'STATUS:  '             WS-FS-SAI              
               MOVE       WS-FS-SAI         TO    WS-AREA-STAT           
               MOVE       'NAO FOI POSSIVEL GRAVAR EM ARQSAI'            
                                            TO    WS-AREA-MSG            
               CALL       WS-ABENDA         USING WS-AREA                
            END-IF.                                                      
                                                                         
            ADD   1                         TO    WS-QTDE-REG-SAI.       
       *----------------------------------------------------------------*
        2240-99-GRAVACAO-SAI-EXIT.          EXIT.                        
       *================================================================*
        2220-LEITURA-ARQENT                 SECTION.                     
       *================================================================*
       * LEITURA DOS REGISTROS EM ARQENT                                *
       *----------------------------------------------------------------*
	    MOVE '2220-LEITURA-ARQENT'      TO    WS-CODIGO-AREA.        
                                                                         
            READ ARQENT.                                                 
                                                                         
            IF  WS-FS-ENT  NOT EQUAL '00'   AND WS-FS-ENT  NOT EQUAL '10'
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-ENT              
               MOVE WS-FS-ENT               TO    WS-AREA-STAT           
               MOVE 'ERRO AO LER ARQUIVO DE ENTRADA ARQENT'              
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
                                                                         
            ELSE                                                         
               IF   WS-FS-ENT = '10'                                     
                    MOVE HIGH-VALUES        TO   WS-CPF-ENT-X            
                    GO TO  2220-99-LEITURA-ARQENT-EXIT                   
               END-IF                                                    
            END-IF.
		                                                                       
            MOVE CPF-ENT                    TO    WS-CPF-ENT.            
            ADD  1                          TO    WS-QTDE-REG-ENT.       
       *----------------------------------------------------------------*
        2220-99-LEITURA-ARQENT-EXIT.        EXIT.                        
       *================================================================*
        2230-LEITURA-SERASA                 SECTION.                     
       *================================================================*
       * LEITURA DOS REGISTROS EM SERASA                                *
       *----------------------------------------------------------------*
            MOVE '2230-LEITURA-SERASA'      TO    WS-CODIGO-AREA.        
                                                                         
            READ SERASA.                                                 
                                                                         
            IF  WS-FS-SER  NOT EQUAL '00'   AND WS-FS-SER  NOT EQUAL '10'
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-SER              
               MOVE WS-FS-SER               TO    WS-AREA-STAT           
	       MOVE 'ERRO AO LER ARQUIVO DE ENTRADA SERASA'              
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
                                                                         
            ELSE                                                         
               IF   WS-FS-SER = '10'                                     
                    MOVE   HIGH-VALUES      TO    WS-CPF-SER-X           
                    GO TO  2230-99-LEITURA-SERASA-EXIT                   
               END-IF                                                    
                                                                         
            END-IF.                                                      
                                                                         
            MOVE CPF-SER                    TO    WS-CPF-SER.            
            ADD  1                          TO    WS-QTDE-REG-SER.       
       *----------------------------------------------------------------*
        2230-99-LEITURA-SERASA-EXIT.        EXIT.                        
       *================================================================*
        3000-RELATORIO                      SECTION.
       *================================================================*
       * IMPRESSAO DO RELATORIO EM RELATO                               *
       *----------------------------------------------------------------*
       * VERIFICA SE A QUANTIDADE DE LINHAS EH MAIOR QUE 49. CASO SEJA  *
       * POSITIVO, EH EFETUADA A QUEBRA DE PAGINA.                      *
       *----------------------------------------------------------------*
            IF   WS-CT-LIN GREATER 49                                    
                 IF PAGINA GREATER ZEROS                                 
                    DISPLAY PAGINA                                       
                    MOVE  SPACES            TO    REG-ATR                
                    WRITE REG-ATR                                        
                 END-IF                                                  
                                                                         
                 MOVE ZEROS TO WS-CT-LIN                                 
                 PERFORM 3100-CABECALHO-PARTE1                           
                 PERFORM 3200-CABECALHO-PARTE2                           
            END-IF.

	    PERFORM 3300-IMPRIMA-DET.                                    
                                                                         
       *----------------------------------------------------------------*
        3000-99-RELATORIO-EXIT.             EXIT.                        
       *================================================================*
        3100-CABECALHO-PARTE1               SECTION.                     
       *================================================================*
       * CABECALHO "PADRAO" - MOVIMENTACAO DE DADOS E GRAVACAO          *
       *----------------------------------------------------------------*
            ADD   1                          TO    WS-CT-PAG.            
            MOVE  WS-CT-PAG                  TO    PAGINA.               
            MOVE  FUNCTION CURRENT-DATE(7:2) TO    DIA.                  
            MOVE  FUNCTION CURRENT-DATE(5:2) TO    MES.                  
            MOVE  FUNCTION CURRENT-DATE(1:4) TO    ANO.                  
            WRITE REG-ATR                    FROM  CAB-01.               
            WRITE REG-ATR                    FROM  CAB-02.               
            WRITE REG-ATR                    FROM  CAB-03.               
            MOVE  3                          TO    WS-CT-LIN.
       *----------------------------------------------------------------*
        3100-99-CABECALHO-PARTE1-EXIT.      EXIT.                        
       *================================================================*
        3200-CABECALHO-PARTE2               SECTION.                     
       *================================================================*
       * MONTAGEM DO CABECALHO USANDO CAMPOS CPF, NOME E VALOR TOTAL    *
       * DIVIDA                                                         *
       *----------------------------------------------------------------*
            MOVE  SPACES                    TO    REG-ATR.               
            WRITE REG-ATR.                                               
            WRITE REG-ATR                   FROM  CAB-04.                
            ADD   2                         TO    WS-CT-LIN.             
       *----------------------------------------------------------------*
        3200-99-CABECALHO-PARTE2-EXIT.      EXIT.                        
       *================================================================*
        3300-IMPRIMA-DET                    SECTION.                     
       *================================================================*
       * IMPRIMA A LINHA DE DETALHE (REGISTRO A SER GRAVADO EM RELATO)  *
       *----------------------------------------------------------------*
            MOVE  '3300-IMPRIMA-DET'        TO    WS-CODIGO-AREA.        
            IF    WS-FLAG   EQUAL ZEROS                                  
                  MOVE SPACES               TO    REG-ATR                
                  MOVE CPF-SER(1:9)         TO    CPF-NOVE               
                  MOVE CPF-SER(10:2)        TO    CPF-DOIS               
                  MOVE NOME-CLIENTE-SER     TO    NOME                   
                  MOVE TOTAL-DIVIDA-SER     TO    VALOR-TOT-D            
                                                                         
                  WRITE REG-ATR             FROM  DETALHE                
                  ADD   TOTAL-DIVIDA-SER    TO    WS-TOTAL-RELAT         
                  ADD   1                   TO    WS-QTDE-REG-REL        
                  MOVE  1                   TO    WS-FLAG                
            END-IF.                                                      
       *----------------------------------------------------------------*
       * VERIFICACAO DE FILE STATUS PARA GRAVACAO EM RELATO             *
       *----------------------------------------------------------------*
            IF WS-FS-REL                    NOT   EQUAL '00'
	       DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-REL              
               MOVE WS-FS-REL               TO    WS-AREA-STAT           
               MOVE 'NAO FOI POSSIVEL GRAVAR EM RELATO'                  
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
            END-IF.                                                      
                                                                         
            ADD   1                         TO    WS-CT-LIN.             
       *----------------------------------------------------------------*
        3300-99-IMPRIMA-DET-EXIT.           EXIT.                        
       *================================================================*
        5000-FIM                            SECTION.                     
       *================================================================*
            MOVE '5000-FIM'                 TO    WS-CODIGO-AREA.        
                                                                         
            IF WS-CT-LIN GREATER 47                                      
               MOVE SPACES TO REG-ATR
	       WRITE REG-ATR                                             
               MOVE ZEROS  TO WS-CT-LIN                                  
               PERFORM 3100-CABECALHO-PARTE1                             
               WRITE REG-ATR                                             
               ADD   1     TO WS-CT-LIN                                  
            END-IF.                                                      
                                                                         
            IF WS-QTDE-BATIMENTOS EQUAL ZEROS                            
               PERFORM 5400-IMPRIMA-ZERO-BAT                             
            END-IF.                                                      
                                                                         
            PERFORM 5100-IMPRIMA-TOT-RELAT.                              
                                                                         
       *----------------------------------------------------------------*
       * FECHAMENTO DOS ARQUIVOS E VERIFICACAO DE FILE STATUS           *
       *----------------------------------------------------------------*
            CLOSE   ARQENT.                                              
            IF WS-FS-ENT                    NOT   EQUAL '00'
	       DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-ENT              
               MOVE WS-FS-ENT               TO    WS-AREA-STAT           
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA ARQENT'    
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
            END-IF.                                                      
       *----------------------------------------------------------------*
            CLOSE   ARQSAI.                                              
            IF WS-FS-SAI                    NOT   EQUAL '00'             
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-SAI              
               MOVE WS-FS-SAI             TO      WS-AREA-STAT           
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE SAIDA ARQSAI'      
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
            END-IF.                                                      
       *----------------------------------------------------------------*
	    CLOSE RELATO.                                                
            IF WS-FS-REL                    NOT   EQUAL '00'             
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FS-REL              
               MOVE WS-FS-REL               TO    WS-AREA-STAT           
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO RELATORIO'            
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
            END-IF.                                                      
                                                                         
            PERFORM 5500-RESUMO.                                         
                                                                         
            DISPLAY '                                                  '.
            DISPLAY '**************************************************'.
            DISPLAY '         FIM DO PROGRAMA - THE END                '.
            DISPLAY '**************************************************'.
       *----------------------------------------------------------------*
        5000-99-FIM-EXIT.                   EXIT.
       *================================================================*
        5100-IMPRIMA-TOT-RELAT              SECTION.                     
       *================================================================*
            MOVE  SPACES                    TO   REG-ATR.                
            WRITE REG-ATR.                                               
            MOVE  WS-TOTAL-RELAT            TO   VALOR-TOT-F.            
            WRITE REG-ATR                   FROM TOT-01.                 
            MOVE  WS-QTDE-REG-REL           TO   TOT-IMP.                
            WRITE REG-ATR                   FROM TOT-02.                 
       *----------------------------------------------------------------*
        5100-99-IMPRIMA-TOT-RELAT-EXIT.     EXIT.                        
       *================================================================*
        5200-IMPRIMA-ARQENT-VAZIO           SECTION.                     
       *================================================================*
            PERFORM 3100-CABECALHO-PARTE1.                               
            MOVE    SPACES                  TO   REG-ATR.                
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR                 FROM DETALHE-ARQENT-VAZIO.
       *----------------------------------------------------------------*
        5200-IMPRIMA-ARQENT-VAZIO-EXIT.     EXIT.                        
       *================================================================*
        5300-IMPRIMA-SERASA-VAZIO           SECTION.                     
       *================================================================*
            PERFORM 3100-CABECALHO-PARTE1.                               
            MOVE    SPACES                  TO   REG-ATR.                
            WRITE   REG-ATR.                                             
            WRITE   REG-ATR                 FROM DETALHE-SERASA-VAZIO.   
       *----------------------------------------------------------------*
        5300-IMPRIMA-SERASA-VAZIO-EXIT.     EXIT.                        
       *================================================================*
        5400-IMPRIMA-ZERO-BAT               SECTION.                     
       *================================================================*
            WRITE   REG-ATR                 FROM DETALHE-ZERO-BATIMENTOS.
       *----------------------------------------------------------------*
        5400-IMPRIMA-ZERO-BAT-EXIT.         EXIT.                        
       *================================================================*
	5500-RESUMO                         SECTION.                     
       *================================================================*
       * RESULTADO DISPONIVEL NA SAIDA SYSOUT                           *
       *----------------------------------------------------------------*
            DISPLAY '**************************************************'.
            DISPLAY '       RESUMO/BALANCO GERAL DO PROGRAMA           '.
            DISPLAY '**************************************************'.
            DISPLAY '**************************************************'.
            DISPLAY '**** REGISTROS ARQENT   =   ' WS-QTDE-REG-ENT      .
            DISPLAY '**** REGISTROS SERASA   =   ' WS-QTDE-REG-SER      .
            DISPLAY '**** REGISTROS ARQSAI   =   ' WS-QTDE-REG-SAI      .
            DISPLAY '**** GRAVADOS EM RELATO =   ' WS-QTDE-REG-REL      .
            DISPLAY '**** BATIMENTOS         =   ' WS-QTDE-BATIMENTOS   .
            DISPLAY '**************************************************'.
            DISPLAY '**************************************************'.
       *----------------------------------------------------------------*
        5500-RESUMO-EXIT.                   EXIT.                        
       *================================================================*                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
