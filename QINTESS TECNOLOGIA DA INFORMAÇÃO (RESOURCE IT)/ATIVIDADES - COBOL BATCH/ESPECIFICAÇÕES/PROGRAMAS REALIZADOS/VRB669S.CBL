       IDENTIFICATION                      DIVISION.                    
      *================================================================*
       PROGRAM-ID.                         VRB669S.                     
       AUTHOR.                             HUGO SAMPAIO.                
       INSTALLATION.                       QINTESS PAULISTA.            
      *================================================================*
      *----------------------------------------------------------------*
      * OBJETIVO: ATUALIZAR O STATUS DAS CONTAS NO VSAM CONTAS, DE     *
      * ACORDO COM O STATUS INFORMADO NO ARQUIVO SEQUENCIAL ARQSTS.    *
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
                                                                        
           SELECT ARQSTS         ASSIGN TO ARQSTS                       
           FILE STATUS           IS        WS-FS-STS.                   
      *----------------------------------------------------------------*
           SELECT CONTAS         ASSIGN TO CONTAS                       
           ORGANIZATION          IS        INDEXED                      
           ACCESS MODE           IS        RANDOM                       
           RECORD KEY            IS        CONTA-VSAM                   
           FILE STATUS           IS        WS-FS-VSAM.                  
      *================================================================*
       DATA                                DIVISION.                    
      *================================================================*
       FILE                                SECTION.                     
      *----------------------------------------------------------------*
       FD  ARQSTS                                                       
           LABEL       RECORD   STANDARD                                
           RECORDING   MODE     F                                       
           RECORD      CONTAINS 60 CHARACTERS                           
           DATA RECORD IS       REG-STS.                                
                                                                        
       01  REG-STS.                                                     
           05 STS-CONTA                    PIC 9(10).                   
           05 STS-CPF                      PIC 9(11).                   
           05 STS-NOME                     PIC X(30).                   
           05 STS-STATUS                   PIC X(01).                   
           05 FILLER                       PIC X(08).                   
      *----------------------------------------------------------------*
       FD  CONTAS                                                       
           RECORD      CONTAINS 60 CHARACTERS                           
           DATA RECORD IS       REG-VSAM.                               
                                                                        
       01  REG-VSAM.                                                    
           05 CONTA-VSAM                   PIC 9(10).                   
           05 CPF-VSAM                     PIC 9(11).                   
           05 NOME-CLIENTE-VSAM            PIC X(30).                   
           05 STATUS-CONTA-VSAM            PIC X(01).                   
           05 FILLER                       PIC X(08).          
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.                     
      *----------------------------------------------------------------*
      *                   DECLARACAO DE VARIAVEIS                      *
      *----------------------------------------------------------------*
      *                   VARIAVEIS DE FILE-STATUS                     *
      *----------------------------------------------------------------*
       77  WS-FS-STS                       PIC X(02)     VALUE '00'.    
       77  WS-FS-VSAM                      PIC X(02)     VALUE '00'.    
      *----------------------------------------------------------------*
      *                         CONTADORES                             *
      *----------------------------------------------------------------*
       77  WS-QTDE-REG-STS                 PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-REG-VSAM                PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-TOT-ATU                 PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-TOT-NF                  PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-REG-F                   PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-BLOQUEADOS              PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-INATIVOS                PIC 9(03)     VALUE ZEROS.   
       77  WS-QTDE-ATIVAS                  PIC 9(03)     VALUE ZEROS.   
      *----------------------------------------------------------------*
      *                       AREA DE ABEND                            *
      *----------------------------------------------------------------*
       77  WS-ABENDA                       PIC X(08)   VALUE 'ABENDA31'.
       01  WS-AREA.                                                     
           05 WS-AREA-PGM                  PIC X(08)   VALUE 'VRB669S'. 
           05 WS-AREA-STAT                 PIC X(03)   VALUE SPACES.    
           05 WS-AREA-MSG                  PIC X(50)   VALUE SPACES.    
                                                                        
       01  WS-CODIGO-AREA                  PIC X(30)   VALUE SPACES.    
      *================================================================*
       PROCEDURE                           DIVISION.                    
      *================================================================*
       0000-PRINCIPAL                      SECTION.                     
      *================================================================*
           PERFORM 1000-INICIO             THRU  1000-99-INICIO-EXIT.   
           PERFORM 2000-PROCESSA           THRU  2000-99-PROCESSA-EXIT  
                                           UNTIL WS-FS-STS   EQUAL '10'.
           PERFORM 5000-WRITE-PGM          THRU  5000-99-WRITE-PGM-EXIT.
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
           MOVE  '1000-INICIO'             TO         WS-CODIGO-AREA.   
                                                                        
           OPEN  INPUT                     ARQSTS.                      
           IF    WS-FS-STS                 NOT EQUAL  '00'              
                 DISPLAY '*----------------------------------------*'   
                 DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'   
                 DISPLAY '*----------------------------------------*'   
                 DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA    
                 DISPLAY 'STATUS:  '                  WS-FS-STS         
                 MOVE WS-FS-STS            TO         WS-AREA-STAT      
                 MOVE 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQSTS'   
                                           TO         WS-AREA-MSG       
                 CALL WS-ABENDA            USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           OPEN  I-O                       CONTAS.                      
           IF    WS-FS-VSAM                NOT EQUAL  '00' AND '97'     
                 DISPLAY '*----------------------------------------*'   
                 DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'   
                 DISPLAY '*----------------------------------------*'   
                 DISPLAY 'ERRO NO: '                  WS-CODIGO-AREA    
                 DISPLAY 'STATUS:  '                  WS-FS-VSAM        
                 MOVE  WS-FS-VSAM          TO         WS-AREA-STAT      
                 MOVE  'ERRO NA ABERTURA DO ARQUIVO DE SAIDA ARQSAI'    
                                           TO         WS-AREA-MSG       
                 CALL  WS-ABENDA           USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           PERFORM 2220-LEITURA-ARQSTS.                                 
                                                                        
           IF WS-QTDE-REG-STS              EQUAL '10'                   
              DISPLAY '************************************************'
              DISPLAY '******* ARQUIVO DE ENTRADA ARQSTS VAZIO ********'
              DISPLAY '************************************************'
              GO TO   1000-99-INICIO-EXIT                               
           END-IF.                                                      
      *----------------------------------------------------------------*
       1000-99-INICIO-EXIT.                EXIT.                        
      *================================================================*
       2000-PROCESSA                       SECTION.                     
      *================================================================*
      * EFETUA-SE UMA LEITURA DE AMBOS OS ARQUIVOS DE ENTRADA ARQSTS   *
      * (SEQUENCIAL) E O ARQUIVO DE VSAM CONTAS. APOS SER EFETUADA COM *
      * SUCESSO, EH VERIFICADO SE A CONTA EXISTE POR MEIO DO CAMPO     *
      * CHAVE CONTA (VSAM). CASO O RESULTADO SEJA POSITIVO, O CAMPO    *
      * STATUS-CONTA DO VSAM EH ATUALIZADO.                            *
      *----------------------------------------------------------------*
           MOVE    STS-CONTA                  TO    CONTA-VSAM.         
           PERFORM 2230-LEITURA-CONTAS.                                 
                                                                        
           MOVE    '1000-INICIO'              TO    WS-CODIGO-AREA.     
                                                                        
           IF  WS-FS-VSAM           EQUAL '00'                          
              PERFORM 2240-CHECA-ST-CONTA                               
              IF   STS-STATUS       EQUAL 'A'                           
                   PERFORM 2250-ALTERA-VSAM                             
              ELSE                                                      
                   PERFORM 2260-NAO-ATIVOS                              
              END-IF                                                    
           END-IF.                                                      
                                                                        
           PERFORM 2220-LEITURA-ARQSTS.                                 
      *----------------------------------------------------------------*
       2000-99-PROCESSA-EXIT.              EXIT.                        
      *================================================================*
       2220-LEITURA-ARQSTS                 SECTION.                     
      *================================================================*
      * LEITURA DOS REGISTROS EM ARQSTS                                *
      *----------------------------------------------------------------*
           MOVE '2220-LEITURA-ARQSTS'      TO  WS-CODIGO-AREA.          
                                                                        
           READ ARQSTS.                                                 
                                                                        
           IF  WS-FS-STS  NOT EQUAL '00'                                
           AND WS-FS-STS  NOT EQUAL '10'                                
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                       WS-CODIGO-AREA 
               DISPLAY 'STATUS:  '                       WS-FS-STS      
               MOVE    WS-FS-STS               TO        WS-AREA-STAT   
               MOVE    'ERRO AO LER ARQUIVO DE ENTRADA ARQSTS'          
                                               TO        WS-AREA-MSG    
               CALL WS-ABENDA                  USING     WS-AREA        
           END-IF.                                                      
                                                                        
           IF  WS-FS-STS  EQUAL '00'                                    
               ADD  1                       TO           WS-QTDE-REG-STS
           END-IF.                                                      
      *----------------------------------------------------------------*
       2220-99-LEITURA-ARQSTS-EXIT.        EXIT.                        
      *================================================================*
       2230-LEITURA-CONTAS                 SECTION.                     
      *================================================================*
      * LEITURA DOS REGISTROS NO ARQUIVO VSAM CONTAS                   *
      *----------------------------------------------------------------*
           MOVE '2230-LEITURA-CONTAS'      TO           WS-CODIGO-AREA. 
                                                                        
           READ CONTAS.                                                 
                                                                        
           IF  WS-FS-VSAM NOT EQUAL '00'                                
           AND WS-FS-VSAM NOT EQUAL '23'                                
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NA LEITURA RANDOMICA:    ' WS-CODIGO-AREA  
               DISPLAY 'VRB669S: FILE STATUS:         ' WS-FS-VSAM      
               MOVE    WS-FS-VSAM              TO       WS-AREA-STAT    
               MOVE    'ERRO AO LER ARQUIVO VSAM CONTAS '               
                                               TO       WS-AREA-MSG     
               CALL    WS-ABENDA               USING    WS-AREA         
           END-IF.                                                      
                                                                        
           IF  WS-FS-VSAM    EQUAL '00'                                 
               ADD      1                      TO       WS-QTDE-REG-F   
           ELSE                                                         
               IF WS-FS-VSAM EQUAL '23'                                 
                  ADD   1                      TO       WS-QTDE-TOT-NF  
                  DISPLAY 'CONTA ' CONTA-VSAM ' NAO ENCONTRADA.'        
               END-IF                                                   
           END-IF.                                                      
                                                                        
           ADD  1                       TO    WS-QTDE-REG-VSAM.         
                                                                        
      *----------------------------------------------------------------*
       2230-99-LEITURA-CONTAS-EXIT.        EXIT.                        
      *================================================================*
       2240-CHECA-ST-CONTA                 SECTION.                     
      *================================================================*
      * VERIFICA O STATUS-CONTA NO ARQUIVO VSAM                        *
      *----------------------------------------------------------------*
           MOVE '2240-CHECA-STATUS'        TO    WS-CODIGO-AREA.        
                                                                        
           IF  STATUS-CONTA-VSAM           EQUAL 'A'                    
               ADD 1           TO          WS-QTDE-ATIVAS               
           ELSE                                                         
               IF STATUS-CONTA-VSAM        EQUAL 'B'                    
                  ADD 1        TO          WS-QTDE-BLOQUEADOS           
               ELSE                                                     
                  IF STATUS-CONTA-VSAM     EQUAL 'C'                    
                     ADD 1     TO          WS-QTDE-INATIVOS             
                  END-IF                                                
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2240-99-CHECA-ST-CONTA-EXIT.        EXIT.                        
      *================================================================*
       2250-ALTERA-VSAM                    SECTION.                     
      *================================================================*
           REWRITE   REG-VSAM  FROM        REG-STS.                     
           PERFORM   2270-CHECA-ALT-VSAM.                               
           ADD       1         TO          WS-QTDE-TOT-ATU.             
      *----------------------------------------------------------------*
       2250-ALTERA-VSAM-EXIT.              EXIT.                        
      *================================================================*
       2260-NAO-ATIVOS                     SECTION.                     
      *================================================================*
           IF STS-STATUS       EQUAL 'I'                                
              PERFORM 2250-ALTERA-VSAM                                  
           ELSE                                                         
              IF STS-STATUS    EQUAL 'B'                                
                 PERFORM 2250-ALTERA-VSAM                               
              END-IF                                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2260-NAO-ATIVOS-EXIT.               EXIT.                        
      *================================================================*
       2270-CHECA-ALT-VSAM                 SECTION.                     
      *================================================================*
           MOVE '2270-CHECA-ALT-VSAM'      TO           WS-CODIGO-AREA. 
                                                                        
           IF  WS-FS-VSAM NOT EQUAL '00'                                
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NA LEITURA RANDOMICA:    ' WS-CODIGO-AREA  
               DISPLAY 'VRB669S: FILE STATUS:         ' WS-FS-VSAM      
               MOVE    WS-FS-VSAM              TO       WS-AREA-STAT    
               MOVE    'ERRO AO REESCREVER O ARQUIVO CONTAS'            
                                               TO       WS-AREA-MSG     
               CALL    WS-ABENDA               USING    WS-AREA         
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2270-CHECA-ALT-VSAM-EXIT.           EXIT.                        
      *================================================================*
       5000-WRITE-PGM                      SECTION.                     
      *================================================================*
           MOVE '5000-WRITE'                 TO    WS-CODIGO-AREA.      
      *----------------------------------------------------------------*
      * FECHAMENTO DOS ARQUIVOS E VERIFICACAO DE FILE STATUS           *
      *----------------------------------------------------------------*
           CLOSE   ARQSTS.                                              
           IF WS-FS-STS                    NOT   EQUAL '00'             
              DISPLAY '*----------------------------------------*'      
              DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'      
              DISPLAY '*----------------------------------------*'      
              DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
              DISPLAY 'STATUS:  '                WS-FS-STS              
              MOVE WS-FS-STS               TO    WS-AREA-STAT           
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO SEQUENCIAL ARQSTS'    
                                           TO    WS-AREA-MSG            
              CALL WS-ABENDA               USING WS-AREA                
           END-IF.                                                      
      *----------------------------------------------------------------*
           CLOSE   CONTAS.                                              
                                                                        
           IF WS-FS-VSAM                   NOT   EQUAL '00'             
              DISPLAY '*----------------------------------------*'      
              DISPLAY '*     VRB669S - PROGRAMA CANCELADO:      *'      
              DISPLAY '*----------------------------------------*'      
              DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
              DISPLAY 'STATUS:  '                WS-FS-VSAM             
              MOVE WS-FS-VSAM            TO      WS-AREA-STAT           
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE SAIDA ARQSAI'      
                                           TO    WS-AREA-MSG            
              CALL WS-ABENDA               USING WS-AREA                
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '         FIM DO PROGRAMA - THE END                '.
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
           PERFORM 5500-RESUMO.                                         
                                                                        
      *----------------------------------------------------------------*
       5000-99-WRITE-PGM-EXIT.             EXIT.                        
      *================================================================*
      *================================================================*
       5500-RESUMO                         SECTION.                     
      *================================================================*
      * RESULTADO DISPONIVEL NA SAIDA SYSOUT                           *
      *----------------------------------------------------------------*
           DISPLAY '**************************************************'.
           DISPLAY '       RESUMO/BALANCO GERAL DO PROGRAMA           '.
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
           DISPLAY '* LIDOS EM ARQSTS       =    ' WS-QTDE-REG-STS     .
           DISPLAY '* LIDOS EM CONTAS       =    ' WS-QTDE-REG-VSAM    .
           DISPLAY '* REGISTROS ENCONTRADOS =    ' WS-QTDE-REG-F       .
           DISPLAY '* REGISTROS NAO-ACHADOS =    ' WS-QTDE-TOT-NF      .
           DISPLAY '* REGISTROS ATUALIZADOS =    ' WS-QTDE-TOT-ATU     .
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
      *----------------------------------------------------------------*
       5500-RESUMO-EXIT.                   EXIT.                        
      *================================================================*

      
