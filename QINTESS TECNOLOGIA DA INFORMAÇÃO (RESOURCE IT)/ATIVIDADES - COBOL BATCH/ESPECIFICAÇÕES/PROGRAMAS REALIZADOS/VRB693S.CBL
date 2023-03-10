       IDENTIFICATION                      DIVISION.                    
      *================================================================*
       PROGRAM-ID.                         VRB693S.                     
       AUTHOR.                             HUGO SAMPAIO.                
       INSTALLATION.                       QINTESS PAULISTA.            
      *================================================================*
      *----------------------------------------------------------------*
      * OBJETIVO: EXPURGAR A BASE DE CONTAS DE ACORDO COM A DATA       *
      * INFORMADA NO PARM. PARA ISSO, ESTE PROGRAMA IRA RECEPCIONAR    *
      * VIA LINKAGE A DATA DE EXPURGO DO VSAM CONTAS E, A PARTIR DE    *
      * ENTAO, DELETAR DO VSAM CONTAS TODOS OS REGISTROS CUJA DATA DE  *
      * ABERTURA FOR ANTERIOR A DATA INFORMADA NA LINKAGE, INCLUSIVE A *
      * PROPRIA DATA INFORMADA.                                        *
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
                                                                        
           SELECT CONTAS         ASSIGN TO CONTAS                       
           ORGANIZATION          IS        INDEXED                      
           ACCESS MODE           IS        SEQUENTIAL                   
           RECORD KEY            IS        CHAVE-CONTA                  
           FILE STATUS           IS        WS-FS-CON.                   
      *================================================================*
       DATA                                DIVISION.                    
      *================================================================*
       FILE                                SECTION.                     
      *----------------------------------------------------------------*
       FD  CONTAS                                                       
           RECORD      CONTAINS 70 CHARACTERS                           
           DATA RECORD IS       REG-CON.                                
                                                                        
       01  REG-CON.                                                     
           05 CHAVE-CONTA                  PIC 9(10).                   
           05 CONTA-CPF                    PIC 9(11).                   
           05 CONTA-CLIENTE                PIC X(30).                   
           05 CONTA-STATUS                 PIC X(01).                   
           05 CONTA-DATA.                                               
              10 CONTA-ANO                 PIC 9999.                    
              10 CONTA-MES                 PIC 99.                      
              10 CONTA-DIA                 PIC 99.                      
           05 FILLER                       PIC X(10).                   
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.                     
      *----------------------------------------------------------------*
      *                   DECLARACAO DE VARIAVEIS                      *
      *----------------------------------------------------------------*
      *                   VARIAVEIS DE FILE-STATUS                     *
      *----------------------------------------------------------------*
       77  WS-FS-CON                       PIC X(02)   VALUE '00'.      
      *----------------------------------------------------------------*
      *                         CONTADORES                             *
      *----------------------------------------------------------------*
       77  WS-CT-LIDOS                     PIC 9(03)   VALUE ZEROS.     
       77  WS-CT-DELETADOS                 PIC 9(03)   VALUE ZEROS.     
       77  WS-DATA                         PIC 9(08)   VALUE ZEROS.     
      *----------------------------------------------------------------*
      *                       AREA DE ABEND                            *
      *----------------------------------------------------------------*
       77  WS-ABENDA                       PIC X(08)   VALUE 'ABENDA31'.
       01  WS-AREA.                                                     
           05 WS-AREA-PGM                  PIC X(08)   VALUE 'VRB693S'. 
           05 WS-AREA-STAT                 PIC X(03)   VALUE SPACES.    
           05 WS-AREA-MSG                  PIC X(50)   VALUE SPACES.    
                                                                        
       01  WS-CODIGO-AREA                  PIC X(30)   VALUE SPACES.    
                                                                        
      *----------------------------------------------------------------*
       LINKAGE                             SECTION.                     
      *----------------------------------------------------------------*
       01  P-PARM.                                                      
           03 FILLER                       PIC S9(04)  COMP.            
           03 P-PARAMETRO.                                              
              10 P-ANO                     PIC 9999.                    
              10 P-MES                     PIC 99.                      
              10 P-DIA                     PIC 99.                      
      *================================================================*
       PROCEDURE                           DIVISION    USING P-PARM.    
      *================================================================*
       0000-PRINCIPAL                      SECTION.                     
      *================================================================*
           PERFORM 1000-INICIO             THRU  1000-99-INICIO-EXIT.   
           PERFORM 2000-PROCESSA           THRU  2000-99-PROCESSA-EXIT  
                                           UNTIL WS-FS-CON EQUAL '10'.  
           PERFORM 5000-FINALIZA           THRU  5000-99-FINALIZA-EXIT. 
           STOP RUN.                                                    
      *----------------------------------------------------------------*
       0000-99-PRINCIPAL-EXIT.             EXIT.                        
      *================================================================*
       1000-INICIO                         SECTION.                     
      *================================================================*
           DISPLAY '**************************************************'.
           DISPLAY '           INICIANDO O PROGRAMA VRB693S           '.
           DISPLAY '**************************************************'.
           DISPLAY '                                                  '.
      *----------------------------------------------------------------*
      * ABERTURA DO VSAM CONTAS E VERIFICACAO DE FILE STATUS           *
      *----------------------------------------------------------------*
           MOVE  '1000-INICIO'             TO         WS-CODIGO-AREA.   
                                                                        
           OPEN I-O                        CONTAS.                      
           IF  WS-FS-CON                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB680S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CON         
               DISPLAY 'ERRO NA ABERTURA DO ARQUIVO VSAM CONTAS '       
               MOVE  WS-FS-CON              TO        WS-AREA-STAT      
               MOVE  'ERRO NA ABERTURA DO ARQUIVO VSAM CONTAS   '       
                                            TO        WS-AREA-MSG       
               CALL  WS-ABENDA              USING     WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           PERFORM 1100-LEITURA-CONTAS.                                 
                                                                        
           IF  WS-FS-CON                    EQUAL '10'                  
               DISPLAY '***********************************************'
               DISPLAY '******* ARQUIVO DE VSAM  CONTAS VAZIO *********'
               DISPLAY '***********************************************'
               GO TO   1000-99-INICIO-EXIT                              
           END-IF.                                                      
      *----------------------------------------------------------------*
       1000-99-INICIO-EXIT.                EXIT.                        
      *================================================================*
       1100-LEITURA-CONTAS                 SECTION.                     
      *================================================================*
      * LEITURA DOS REGISTROS NO ARQUIVO VSAM CONTAS                   *
      *----------------------------------------------------------------*
           MOVE '1100-LEITURA-CONTAS'      TO         WS-CODIGO-AREA.   
                                                                        
           READ CONTAS.                                                 
                                                                        
           IF  WS-FS-CON  NOT EQUAL '00'                                
           AND WS-FS-CON  NOT EQUAL '10'                                
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB680S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CON         
               DISPLAY 'ERRO AO LER ARQUIVO DE VSAM CONTAS '            
               MOVE    WS-FS-CON           TO         WS-AREA-STAT      
               MOVE    'ERRO AO LER ARQUIVO DE VSAM CONTAS '            
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
                                                                        
           IF  WS-FS-CON                   NOT EQUAL     '10'           
               ADD  1                      TO            WS-CT-LIDOS    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1100-99-LEITURA-CONTAS-EXIT.        EXIT.                        
      *================================================================*
       2000-PROCESSA                       SECTION.                     
      *================================================================*
           MOVE    '2000-PROCESSA'         TO         WS-CODIGO-AREA.   
                                                                        
      *    IF CONTA-ANO < P-ANO                                         
      *    OR (CONTA-ANO = P-ANO AND CONTA-MES < P-MES)                 
      *    OR (CONTA-ANO = P-ANO AND CONTA-MES = P-MES AND              
      *        CONTA-DIA < P-DIA)                                       
      *    OR (CONTA-ANO = P-ANO AND CONTA-MES = P-MES AND              
      *        CONTA-DIA = P-DIA)                                       
                                                                        
           MOVE CONTA-DATA                 TO         WS-DATA.          
                                                                        
           IF P-PARAMETRO                  >          WS-DATA           
              DELETE CONTAS                                             
              PERFORM 2100-TESTA-DELETE                                 
              ADD  1                       TO         WS-CT-DELETADOS   
           END-IF.                                                      
                                                                        
           PERFORM 1100-LEITURA-CONTAS.                                 
                                                                        
      *----------------------------------------------------------------*
       2000-99-PROCESSA-EXIT.              EXIT.                        
      *================================================================*
       2100-TESTA-DELETE                   SECTION.                     
      *================================================================*
      * VERIFICA SE O COMANDO DELETE FOI EXECUTADO COM SUCESSO.        *
      *----------------------------------------------------------------*
           MOVE '2100-TESTA-DELETE'        TO         WS-CODIGO-AREA.   
                                                                        
           IF  WS-FS-CON                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB680S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CON         
               DISPLAY 'ERRO AO DELETAR REGISTRO DO VSAM CONTAS'        
               MOVE    WS-FS-CON           TO         WS-AREA-STAT      
               MOVE    'ERRO AO DELETAR REGISTRO DO VSAM CONTAS'        
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2100-99-TESTA-DELETE-EXIT.          EXIT.                        
      *================================================================*
       5000-FINALIZA                       SECTION.                     
      *================================================================*
           MOVE '5000-FINALIZA'            TO         WS-CODIGO-AREA.   
      *----------------------------------------------------------------*
      * FECHAMENTO DO  ARQUIVO  CONTAS E VERIFICACAO DE FILE STATUS.   *
      *----------------------------------------------------------------*
           CLOSE   CONTAS.                                              
                                                                        
           IF  WS-FS-CON                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB680S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CON         
               DISPLAY 'ERRO NO FECHAMENTO DO ARQUIVO VSAM CONTAS'      
               MOVE WS-FS-CON              TO         WS-AREA-STAT      
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO VSAM CONTAS'         
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
                                                                        
           PERFORM 5500-RESUMO.                                         
                                                                        
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '         FIM DO PROGRAMA - THE END                '.
           DISPLAY '**************************************************'.
                                                                        
      *----------------------------------------------------------------*
       5000-99-FINALIZA-EXIT.              EXIT.                        
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
           DISPLAY '* DATA-PARAMETRO       =  '  P-ANO '/' P-MES '/'    
                                                           P-DIA       .
           DISPLAY '* LIDOS EM CONTAS      =  '  WS-CT-LIDOS           .
           DISPLAY '* REGISTROS DELETADOS  =  '  WS-CT-DELETADOS       .
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
      *----------------------------------------------------------------*
       5500-RESUMO-EXIT.                   EXIT.                        
      *================================================================*
      
      
