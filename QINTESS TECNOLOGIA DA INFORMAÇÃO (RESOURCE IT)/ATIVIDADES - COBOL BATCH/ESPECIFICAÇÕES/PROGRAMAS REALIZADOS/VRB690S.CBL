       IDENTIFICATION                      DIVISION.                    
      *================================================================*
       PROGRAM-ID.                         VRB690S.                     
       AUTHOR.                             HUGO SAMPAIO.                
       INSTALLATION.                       QINTESS PAULISTA.            
      *================================================================*
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
           SELECT ARQCAR         ASSIGN TO ARQCAR                       
           ORGANIZATION          IS        SEQUENTIAL                   
           ACCESS MODE           IS        SEQUENTIAL                   
           FILE STATUS           IS        WS-FS-CAR.                   
      *----------------------------------------------------------------*
           SELECT DETRAN         ASSIGN TO DETRAN                       
           ORGANIZATION          IS        INDEXED                      
           ACCESS MODE           IS        DYNAMIC                      
           RECORD KEY            IS        CHAVE-DETRAN                 
           FILE STATUS           IS        WS-FS-DET.                   
      *----------------------------------------------------------------*
           SELECT RELATO         ASSIGN TO RELATO                       
           FILE STATUS           IS        WS-FS-REL.                   
      *================================================================*
       DATA                                DIVISION.                    
      *================================================================*
       FILE                                SECTION.                     
      *----------------------------------------------------------------*
       FD  ARQCAR                                                       
                                                                       
           RECORD      CONTAINS  30 CHARACTERS                          
           DATA RECORD IS        ARQ-CAR.                               
                                                                        
       01  ARQ-CAR.                                                     
           05 ARQ-PLACA                    PIC X(07).                   
           05 ARQ-MODELO                   PIC X(10).                   
           05 ARQ-ANOFAB                   PIC 9(04).                   
           05 FILLER                       PIC X(09).                   
      *----------------------------------------------------------------*
       FD  DETRAN                                                       
                                                                        
           RECORD      CONTAINS  70 CHARACTERS                          
           DATA RECORD IS        REG-DETRAN.                            
                                                                        
       01  REG-DETRAN.                                                  
           05 CHAVE-DETRAN.                                             
              10 DETRAN-PLACA              PIC X(07).                   
              10 DETRAN-CODMULTA           PIC 9(05).                   
           05 DETRAN-INFRACAO              PIC X(30).                   
           05 DETRAN-DATA                  PIC 9(08).                   
           05 DETRAN-VALOR                 PIC 9(15)V99.                
           05 FILLER                       PIC X(03).                   
      *----------------------------------------------------------------*
       FD  RELATO                          RECORDING MODE F.            
                                                                        
       01  REG-LINHA                       PIC X(80).                   
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.                     
      *----------------------------------------------------------------*
      *                   DECLARACAO DE VARIAVEIS                      *
      *----------------------------------------------------------------*
      *                   VARIAVEIS DE FILE-STATUS                     *
      *----------------------------------------------------------------*
       77  WS-FS-CAR                       PIC X(02)     VALUE '00'.    
       77  WS-FS-DET                       PIC X(02)     VALUE '00'.    
       77  WS-FS-REL                       PIC X(02)     VALUE '00'.    
      *----------------------------------------------------------------*
      *                         CONTADORES                             *
      *----------------------------------------------------------------*
       77  WS-CT-LIN                       PIC 9(03)     VALUE 99.      
       77  WS-CT-PAG                       PIC 9(03)     VALUE ZEROS.   
       77  WS-CT-VEICULOS                  PIC 9(03)     VALUE ZEROS.   
       77  WS-CT-MULTAS                    PIC 9(03)     VALUE ZEROS.   
       77  WS-CT-DMULTAS                   PIC 9(03)     VALUE ZEROS.   
                                                                        
       77  WS-LIDOS-ARQCAR                 PIC 9(10)     VALUE ZEROS.   
       77  WS-LIDOS-DETRAN                 PIC 9(10)     VALUE ZEROS.   
                                                                        
      *----------------------------------------------------------------*
      *                        VARIAVEIS                               *
      *----------------------------------------------------------------*
       77  WS-ARQCAR-PLACA                 PIC X(07).                   
       77  WS-DETRAN-PLACA                 PIC X(07).                   
       77  WS-VALOR-TOTAL                  PIC 9(10)V99.                
      *----------------------------------------------------------------*
      *                       AREA DE ABEND                            *
      *----------------------------------------------------------------*
       77  WS-ABENDA                       PIC X(08)   VALUE 'ABENDA31'.
       01  WS-AREA.                                                     
           05 WS-AREA-PGM                  PIC X(08)   VALUE 'VRB690S'. 
           05 WS-AREA-STAT                 PIC X(03)   VALUE SPACES.    
           05 WS-AREA-MSG                  PIC X(50)   VALUE SPACES.    
                                                                        
       01  WS-CODIGO-AREA                  PIC X(30)   VALUE SPACES.    
      *----------------------------------------------------------------*
      *                   LAYOUT  DO RELATORIO                         *
      *----------------------------------------------------------------*
      *                        CABECALHO                               *
      *----------------------------------------------------------------*
       01  CAB-01.                                                      
           05 FILLER                       PIC X(28)  VALUE SPACES.     
           05 FILLER                       PIC X(09)  VALUE 'BRADESCO '.
           05 FILLER                       PIC X(07)  VALUE 'CARTOES'.  
           05 FILLER                       PIC X(19)  VALUE SPACES.     
           05 FILLER                       PIC X(06)  VALUE 'DATA: '.   
           05 CAB-DIA                      PIC 99     VALUE  ZEROS.     
           05 FILLER                       PIC X(01)  VALUE '/'.        
           05 CAB-MES                      PIC 99     VALUE ZEROS.      
           05 FILLER                       PIC X(01)  VALUE '/'.        
           05 CAB-ANO                      PIC 9999   VALUE ZEROS.      
      *----------------------------------------------------------------*
       01  CAB-02.                                                      
           05 FILLER                       PIC X(08)  VALUE 'VRB690S '. 
           05 FILLER                       PIC X(15)  VALUE SPACES.     
           05 FILLER                       PIC X(09)  VALUE 'RELATORIO'.
           05 FILLER                       PIC X(09)  VALUE ' CONTA X '.
           05 FILLER                       PIC X(07)  VALUE 'CARTOES'.  
           05 FILLER                       PIC X(15)  VALUE SPACES.     
           05 FILLER                       PIC X(09)  VALUE 'PAGINA...'.
           05 FILLER                       PIC X(04)  VALUE '..: '.     
           05 CAB-PAGINA                   PIC 999    VALUE  ZEROS.     
      *----------------------------------------------------------------*
       01  CAB-03.                                                      
           05 FILLER                       PIC X(80)  VALUE ALL '-'.    
      *----------------------------------------------------------------*
       01  CAB-04.                                                      
           05 FILLER                       PIC X(07)  VALUE 'VEICULO'.  
           05 FILLER                       PIC X(04)  VALUE SPACES.     
           05 FILLER                       PIC X(05)  VALUE 'PLACA'.    
           05 FILLER                       PIC X(04)  VALUE SPACES.     
           05 FILLER                       PIC X(08)  VALUE 'INFRACAO'. 
           05 FILLER                       PIC X(09)  VALUE SPACES.     
           05 FILLER                       PIC X(08)  VALUE 'BANDEIRA'. 
           05 FILLER                       PIC X(07)  VALUE SPACES.     
           05 FILLER                       PIC X(05)  VALUE 'DATA '.    
           05 FILLER                       PIC X(08)  VALUE 'INFRACAO'. 
           05 FILLER                       PIC X(02)  VALUE SPACES.     
           05 FILLER                       PIC X(05)  VALUE 'VALOR'.    
      *----------------------------------------------------------------*
      *                 AREA DE DETALHE - LAYOUT                       *
      *----------------------------------------------------------------*
       01  DET-01.                                                      
           05 DET-MODELO                   PIC X(10)  VALUE ZEROS.      
           05 FILLER                       PIC X(01)  VALUE SPACES.     
           05 DET-PLACA                    PIC X(07)  VALUE SPACES.     
           05 FILLER                       PIC X(02)  VALUE SPACES.     
           05 DET-INFRACAO                 PIC X(30)  VALUE SPACES.     
           05 FILLER                       PIC X(02)  VALUE SPACES.     
           05 DET-DIA                      PIC 99.                      
           05 FILLER                       PIC X(01)  VALUE '/'.        
           05 DET-MES                      PIC 99.                      
           05 FILLER                       PIC X(01)  VALUE '/'.        
           05 DET-ANO                      PIC 9999.                    
           05 FILLER                       PIC X(05)  VALUE SPACES.     
           05 DET-VALOR                    PIC ZZ.ZZZ,99.               
      *----------------------------------------------------------------*
      *                AREA DE TOTALIZADORES - LAYOUT                  *
      *----------------------------------------------------------------*
       01  TOT-01.                                                      
           05 FILLER                       PIC X(06)  VALUE 'TOTAL '.   
           05 FILLER                       PIC X(09)  VALUE 'VEICULOS '.
           05 FILLER                       PIC X(09)  VALUE 'IMPRESSOS'.
           05 FILLER                       PIC X(02)  VALUE '.:'.       
           05 FILLER                       PIC X(10)  VALUE SPACES.     
           05 TOT-VEICULOS                 PIC Z.ZZZ.ZZZ.ZZ9.           
      *----------------------------------------------------------------*
       01  TOT-02.                                                      
           05 FILLER                       PIC X(06)  VALUE 'TOTAL '.   
           05 FILLER                       PIC X(09)  VALUE 'MULTAS...'.
           05 FILLER                       PIC X(09)  VALUE '.........'.
           05 FILLER                       PIC X(02)  VALUE '.:'.       
           05 FILLER                       PIC X(10)  VALUE SPACES.     
           05 TOT-MULTAS                   PIC Z.ZZZ.ZZZ.ZZ9.           
      *----------------------------------------------------------------*
       01  TOT-03.                                                      
           05 FILLER                       PIC X(06)  VALUE 'VALOR '.   
           05 FILLER                       PIC X(06)  VALUE 'TOTAL '.   
           05 FILLER                       PIC X(09)  VALUE 'MULTAS ..'.
           05 FILLER                       PIC X(05)  VALUE '....:'.    
           05 FILLER                       PIC X(10)  VALUE SPACES.     
           05 TOT-VALORTOTAL               PIC Z.ZZZ.ZZZ.ZZ9.           
      *================================================================*
       PROCEDURE                           DIVISION.                    
      *================================================================*
       0000-PRINCIPAL                      SECTION.                     
      *================================================================*
           PERFORM 1000-INICIO             THRU  1000-99-INICIO-EXIT.   
           PERFORM 2000-PROCESSA           THRU  2000-99-PROCESSA-EXIT  
                                           UNTIL WS-FS-CAR   EQUAL '10'.
           PERFORM 7000-FINALIZA           THRU  7000-99-FINALIZA-EXIT. 
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
      * ABERTURA DE ARQUIVOS DE ENTRADA E VERIFICACAO DE FILE STATUS   *
      *----------------------------------------------------------------*
           MOVE  '1000-INICIO'             TO         WS-CODIGO-AREA.   
                                                                        
           OPEN  INPUT                     ARQCAR.                      
           IF  WS-FS-CAR                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CAR         
               MOVE WS-FS-CAR              TO         WS-AREA-STAT      
               MOVE 'ERRO NA ABERTURA DO ARQUIVO SEQ. ARQCAR'           
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           OPEN  INPUT                     DETRAN.                      
           IF  WS-FS-DET                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-DET         
               MOVE  WS-FS-DET             TO         WS-AREA-STAT      
               MOVE  'ERRO NA ABERTURA DO ARQUIVO VSAM DETRAN '         
                                           TO         WS-AREA-MSG       
               CALL  WS-ABENDA             USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           OPEN  OUTPUT                    RELATO.                      
                                                                        
           IF  WS-FS-REL                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-REL         
               MOVE  WS-FS-REL             TO         WS-AREA-STAT      
               MOVE  'ERRO NA ABERTURA DO ARQUIVO RELATO'               
                                           TO         WS-AREA-MSG       
               CALL  WS-ABENDA             USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           PERFORM 1100-LEITURA-ARQCAR.                                 
                                                                        
           IF  WS-FS-CAR                  EQUAL '10'                    
               DISPLAY ' ******************************************** ' 
               DISPLAY ' **********  ARQUIVO CONTAS VAZIO  ********** ' 
               DISPLAY ' ******************************************** ' 
               MOVE  '10'                 TO          WS-FS-CAR         
               PERFORM 1900-CABECALHO                                   
               GO TO 1000-99-INICIO-EXIT                                
           END-IF.                                                      
                                                                        
      *    PERFORM 1200-START-DETRAN.                                   
                                                                        
      *    IF  WS-FS-DET                  EQUAL '10'                    
      *        DISPLAY ' ******************************************** ' 
      *        DISPLAY ' *******  ARQUIVO DETRAN VAZIO      ********* ' 
      *        DISPLAY ' ******************************************** ' 
      *        DISPLAY ' ******************************************** ' 
      *        MOVE  '10'                 TO          WS-FS-CAR         
      *        PERFORM 1900-CABECALHO                                   
      *        GO TO 1000-99-INICIO-EXIT                                
      *    END-IF.                                                      
      *                                                                 
      *    PERFORM 1300-READNEXT-DETRAN.                                
                                                                        
      *----------------------------------------------------------------*
       1000-99-INICIO-EXIT.                EXIT.                        
      *================================================================*
       1100-LEITURA-ARQCAR                 SECTION.                     
      *================================================================*
      * LEITURA DOS REGISTROS EM ARQCAR                                *
      *----------------------------------------------------------------*
           MOVE '1100-LEITURA-ARQCAR'      TO         WS-CODIGO-AREA.   
                                                                        
           READ ARQCAR.                                                 
                                                                        
           IF  WS-FS-CAR  NOT EQUAL '00'                                
           AND WS-FS-CAR  NOT EQUAL '10'                                
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CAR         
               MOVE    WS-FS-CAR               TO     WS-AREA-STAT      
               MOVE    'ERRO AO LER ARQUIVO VSAM CONTAS'                
                                               TO     WS-AREA-MSG       
               CALL WS-ABENDA                  USING  WS-AREA           
           END-IF.                                                      
                                                                        
           ADD  1                          TO         WS-LIDOS-ARQCAR.  
                                                                        
           MOVE ARQ-PLACA                  TO         WS-ARQCAR-PLACA.  
                                                                        
      *----------------------------------------------------------------*
       1100-99-LEITURA-ARQCAR-EXIT.        EXIT.                        
      *================================================================*
       1200-START-DETRAN                   SECTION.                     
      *================================================================*
           MOVE '1200-START-DETRAN'        TO         WS-CODIGO-AREA.   
                                                                        
           MOVE  ARQ-PLACA                 TO         DETRAN-PLACA.     
           MOVE  ZEROS                     TO         DETRAN-CODMULTA.  
TIRAR      DISPLAY 'DENTRO DO START: ' ARQ-PLACA                        
                                                                        
           START DETRAN                    KEY NOT LESS CHAVE-DETRAN.   
                                                                        
           IF  WS-FS-DET NOT EQUAL '00'    AND WS-FS-DET NOT EQUAL '23' 
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO: ' WS-CODIGO-AREA                          
               DISPLAY 'VRB690S: FILE STATUS:       ' WS-FS-DET         
               MOVE    WS-FS-DET               TO     WS-AREA-STAT      
               MOVE    'ERRO AO DAR START NO VSAM DETRAN  '             
                                               TO     WS-AREA-MSG       
               CALL    WS-ABENDA               USING  WS-AREA           
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1200-99-START-DETRAN-EXIT.          EXIT.                        
      *================================================================*
       1300-READNEXT-DETRAN                SECTION.                     
      *================================================================*
           MOVE '1300-READNEXT'            TO         WS-CODIGO-AREA.   
                                                                        
           READ DETRAN   NEXT.                                         
                                                                       
TIRAR      DISPLAY 'FS READ DETRAN: ' WS-FS-DET                        
           IF  WS-FS-DET NOT EQUAL '00'                                
           AND WS-FS-DET NOT EQUAL '10'                                
               DISPLAY '*----------------------------------------*'    
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'    
               DISPLAY '*----------------------------------------*'    
               DISPLAY 'ERRO:    '                    WS-CODIGO-AREA   
               DISPLAY 'VRB690S: FILE STATUS:       ' WS-FS-DET        
               MOVE    WS-FS-DET               TO     WS-AREA-STAT     
               MOVE    'ERRO AO LER ARQUIVO VSAM DETRAN  '             
                                               TO     WS-AREA-MSG      
               CALL    WS-ABENDA               USING  WS-AREA          
           END-IF.                                                     
                                                                       
           IF  WS-FS-DET EQUAL '10'                                    
               MOVE SPACES                 TO         WS-DETRAN-PLACA  
           ELSE                                                        
               MOVE DETRAN-PLACA             TO       WS-DETRAN-PLACA  
               ADD  1                      TO         WS-LIDOS-DETRAN   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1300-99-READNEXT-DETRAN-EXIT.       EXIT.                        
      *================================================================*
       1900-CABECALHO                      SECTION.                     
      *================================================================*
           ADD   1                         TO         WS-CT-PAG.        
           MOVE  WS-CT-PAG                 TO         CAB-PAGINA.       
                                                                        
           IF  WS-CT-PAG                   GREATER    1                 
               MOVE  SPACES                TO         REG-LINHA         
               WRITE                       REG-LINHA                    
               PERFORM 2500-STATUS-REGLINHA                             
           END-IF.                                                      
                                                                        
           MOVE FUNCTION CURRENT-DATE(7:2) TO         CAB-DIA.          
           MOVE FUNCTION CURRENT-DATE(5:2) TO         CAB-MES.          
           MOVE FUNCTION CURRENT-DATE(1:4) TO         CAB-ANO.    
                                                                        
           WRITE REG-LINHA                 FROM       CAB-01.           
           PERFORM 2500-STATUS-REGLINHA.                                
           WRITE REG-LINHA                 FROM       CAB-02.           
           PERFORM 2500-STATUS-REGLINHA.                                
           WRITE REG-LINHA                 FROM       CAB-03.           
           PERFORM 2500-STATUS-REGLINHA.                                
           MOVE  SPACES                    TO         REG-LINHA.        
           WRITE REG-LINHA.                                             
           PERFORM 2500-STATUS-REGLINHA.                                
                                                                        
           MOVE      4                     TO         WS-CT-LIN.        
                                                                        
           IF  WS-FS-CAR                   NOT  EQUAL '10'              
               WRITE REG-LINHA             FROM       CAB-04            
               PERFORM 2500-STATUS-REGLINHA                             
               MOVE  5                     TO         WS-CT-LIN         
           END-IF.                                                      
      *----------------------------------------------------------------*
       1900-CABECALHO-EXIT.                EXIT.                        
      *================================================================*
       2000-PROCESSA                       SECTION.                     
      *================================================================*
           PERFORM 1200-START-DETRAN.                                   
           PERFORM 1300-READNEXT-DETRAN.                                
                                                                        
           IF  WS-ARQCAR-PLACA             EQUAL      WS-DETRAN-PLACA   
                                                                        
               PERFORM UNTIL WS-FS-DET     EQUAL '10'                   
                  OR         WS-ARQCAR-PLACA  NOT EQUAL  WS-DETRAN-PLACA
TIRAR      DISPLAY 'UNTIL---------> ' WS-FS-DET                         
TIRAR      DISPLAY 'PLACA ARQCAR--> ' WS-ARQCAR-PLACA                   
TIRAR      DISPLAY 'PLACA ARQDETR-> ' WS-DETRAN-PLACA                   
                  ADD     1                   TO         WS-CT-MULTAS   
                  ADD     1                   TO         WS-CT-DMULTAS  
                  MOVE    ARQ-MODELO          TO         DET-MODELO     
                  MOVE    ARQ-PLACA           TO         DET-PLACA      
                  MOVE    DETRAN-INFRACAO     TO         DET-INFRACAO   
                  MOVE    DETRAN-DATA(1:2)    TO         DET-DIA        
                  MOVE    DETRAN-DATA(3:2)    TO         DET-MES        
                  MOVE    DETRAN-DATA(5:4)    TO         DET-ANO        
                  MOVE    DETRAN-VALOR        TO         DET-VALOR      
                  ADD     DETRAN-VALOR        TO         WS-VALOR-TOTAL 
                  PERFORM 2600-IMPRIMA-DETALHE                          
                  PERFORM 1300-READNEXT-DETRAN                          
TIRAR      DISPLAY 'PLACA ARQCAR-FIM- > ' WS-ARQCAR-PLACA               
TIRAR      DISPLAY 'PLACA ARQDETR-FIM-> ' WS-DETRAN-PLACA               
               END-PERFORM                                              
           END-IF.                                                      
                                                                        
           PERFORM 1100-LEITURA-ARQCAR.                                 
           MOVE    ZEROS                   TO         WS-CT-MULTAS.     
      *----------------------------------------------------------------*
       2000-99-PROCESSA-EXIT.              EXIT.                        
      *================================================================*
       2500-STATUS-REGLINHA                SECTION.                     
      *================================================================*
           MOVE '2500-STATUS-REGLINHA'     TO         WS-CODIGO-AREA.   
                                                                        
           IF  WS-FS-REL                   NOT EQUAL  '00'              
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-REL         
               MOVE WS-FS-REL              TO         WS-AREA-STAT      
               MOVE 'ERRO AO GRAVAR LINHA EM RELATO '                   
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2500-STATUS-REGLINHA-EXIT.          EXIT.                        
      *================================================================*
       2600-IMPRIMA-DETALHE                SECTION.                     
      *================================================================*
           IF  WS-CT-LIN                   GREATER    49                
               PERFORM 1900-CABECALHO                                   
           END-IF.                
           WRITE REG-LINHA                 FROM       DET-01.           
           PERFORM 2500-STATUS-REGLINHA.                                
           ADD    1                        TO         WS-CT-LIN.        
                                                                        
      *----------------------------------------------------------------*
       2600-IMPRIMA-DETALHE-EXIT.          EXIT.                        
      *================================================================*
       3000-IMPRIMA-TOT                    SECTION.                     
      *================================================================*
           IF  WS-CT-LIN                   GREATER    44                
               PERFORM 1900-CABECALHO                                   
           END-IF.                                                      
                                                                        
           MOVE      WS-CT-VEICULOS        TO         TOT-VEICULOS.     
           MOVE      WS-CT-DMULTAS         TO         TOT-MULTAS.       
           MOVE      WS-VALOR-TOTAL        TO         TOT-VALORTOTAL.   
                                                                        
           MOVE      SPACES                TO         REG-LINHA.        
           WRITE     REG-LINHA.                                         
           PERFORM   2500-STATUS-REGLINHA.                              
           WRITE     REG-LINHA             FROM       TOT-01.           
           PERFORM   2500-STATUS-REGLINHA.                              
           WRITE     REG-LINHA             FROM       TOT-02.           
           PERFORM   2500-STATUS-REGLINHA.                              
           WRITE     REG-LINHA             FROM       TOT-03.           
           PERFORM   2500-STATUS-REGLINHA.                              
                                                                        
      *----------------------------------------------------------------*
       3000-99-IMPRIMA-TOT-EXIT.           EXIT.                        
      *================================================================*
       7000-FINALIZA                       SECTION.                     
      *================================================================*
           MOVE '7000-FINALIZA'            TO         WS-CODIGO-AREA.   
                                                                        
           MOVE      SPACES                TO         REG-LINHA.        
           WRITE     REG-LINHA.                                         
           ADD       1                     TO         WS-CT-LIN.        
                                                                        
           PERFORM   3000-IMPRIMA-TOT.                                  
                                                                        
      *----------------------------------------------------------------*
      * FECHAMENTO DOS ARQUIVOS E VERIFICACAO DE FILE STATUS           *
      *----------------------------------------------------------------*
           CLOSE     ARQCAR.                                            
           IF  WS-FS-CAR                   NOT   EQUAL '00'             
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-CAR         
               MOVE WS-FS-CAR              TO         WS-AREA-STAT      
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO ARQCAR'              
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           CLOSE    DETRAN.                                             
                                                                        
           IF  WS-FS-DET                   NOT   EQUAL '00'             
               DISPLAY '*----------------------------------------*'     
               DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'     
               DISPLAY '*----------------------------------------*'     
               DISPLAY 'ERRO NO: '                    WS-CODIGO-AREA    
               DISPLAY 'STATUS:  '                    WS-FS-DET         
               MOVE WS-FS-DET              TO         WS-AREA-STAT      
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO VSAM DETRAN '        
                                           TO         WS-AREA-MSG       
               CALL WS-ABENDA              USING      WS-AREA           
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
           CLOSE    RELATO.                                             
                                                                        
           IF WS-FS-REL                    NOT   EQUAL '00'             
              DISPLAY '*----------------------------------------*'      
              DISPLAY '*     VRB690S - PROGRAMA CANCELADO:      *'      
              DISPLAY '*----------------------------------------*'      
              DISPLAY 'ERRO NO: '                     WS-CODIGO-AREA    
              DISPLAY 'STATUS:  '                     WS-FS-CAR         
              MOVE WS-FS-CAR               TO         WS-AREA-STAT      
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO RELATO '              
                                           TO         WS-AREA-MSG       
              CALL WS-ABENDA               USING      WS-AREA           
           END-IF.                                                      
      *----------------------------------------------------------------*
           PERFORM 7500-RESUMO.                                         
                                                                        
           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '         FIM DO PROGRAMA - THE END                '.
           DISPLAY '**************************************************'.
                                                                        
      *----------------------------------------------------------------*
       7000-99-FINALIZA-EXIT.              EXIT.                        
      *================================================================*
      *================================================================*
       7500-RESUMO                         SECTION.                     
      *================================================================*
      * RESULTADO DISPONIVEL NA SAIDA SYSOUT                           *
      *----------------------------------------------------------------*
           DISPLAY '**************************************************'.
           DISPLAY '       RESUMO/BALANCO GERAL DO PROGRAMA           '.
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
           DISPLAY '* LIDOS EM ARQCAR                ' WS-LIDOS-ARQCAR .
           DISPLAY '* LIDOS EM DETRAN             =  ' WS-LIDOS-DETRAN .
           DISPLAY '* TOTAL DE VEICULOS IMPRESSOS =  ' WS-CT-VEICULOS  .
           DISPLAY '=================================================='.
           DISPLAY '* TOTAL DE MULTAS             =  ' WS-CT-DMULTAS   .
           DISPLAY '* VALOR TOTAL DE MULTAS       =  ' WS-VALOR-TOTAL  .
           DISPLAY '**************************************************'.
           DISPLAY '**************************************************'.
      *----------------------------------------------------------------*
       7500-RESUMO-EXIT.                   EXIT.                        
      *================================================================*                                      

      
