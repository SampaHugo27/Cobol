	IDENTIFICATION                      DIVISION.                    
       *----------------------------------------------------------------*
        PROGRAM-ID.            EX01.                                     
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
        SOURCE-COMPUTER.       IBM-PC.                                   
        OBJECT-COMPUTER.       IBM-PC.                                   
        SPECIAL-NAMES.         DECIMAL-POINT IS COMMA.                   
       *----------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
            SELECT ENTRADA  ASSIGN TO ENTRADA                            
            ORGANIZATION IS SEQUENTIAL.                                  
       *----------------------------------------------------------------*
            SELECT TRAB     ASSIGN TO TRAB.                              
       *----------------------------------------------------------------*
            SELECT RELAT    ASSIGN TO RELAT.                             
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        DATA                                DIVISION.                    
       *----------------------------------------------------------------*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD  ENTRADA                                                      
            LABEL RECORD ARE STANDARD.                                   
                                                                         
        01  REG-ENT.
	    05 COD-ENT                      PIC 9(04).                   
            05 SEXO-ENT                     PIC X(01).                   
            05 NOME-ENT                     PIC X(30).                   
       *----------------------------------------------------------------*
        SD  TRAB.                                                        
        01  REG-TRAB.                                                    
            05 COD-TRAB                     PIC 9(04).                   
            05 SEXO-TRAB                    PIC X(01).                   
            05 NOME-TRAB                    PIC X(30).                   
       *----------------------------------------------------------------*
        FD  RELAT                                                        
            LABEL RECORD IS  OMITTED.                                    
                                                                         
        01  REG-REL                         PIC X(80).                   
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.                     
       *----------------------------------------------------------------*
        77  FIM-ARQ                         PIC X(03) VALUE 'NAO'.
	77  WS-CTLIN                        PIC 9(02) VALUE 25.          
        77  WS-CTPAG                        PIC 9(02) VALUE ZEROS.       
       *----------------------------------------------------------------*
       *           LAYOUT  DE RELATORIO - AREA DE CABECALHOS            *
       *----------------------------------------------------------------*
        01 CAB-01.                                                       
            05 FILLER         PIC X(70) VALUE SPACES.                    
            05 FILLER         PIC X(05) VALUE 'PAG. '.                   
            05 VAR-PAG        PIC 99.                                    
            05 FILLER         PIC X(03) VALUE SPACES.                    
       *----------------------------------------------------------------*
        01 CAB-02.                                                       
            05 FILLER         PIC X(15) VALUE SPACES.                    
            05 FILLER         PIC X(25) VALUE 'RELATORIO DE USUARIOS DO'.
            05 FILLER         PIC X(15) VALUE ' SEXO MASCULINO'.         
            05 FILLER         PIC X(25) VALUE SPACES.                    
       *----------------------------------------------------------------*
        01 CAB-03.
            05 FILLER         PIC X(05) VALUE SPACES.                    
            05 FILLER         PIC X(06) VALUE 'CODIGO'.                  
            05 FILLER         PIC X(10) VALUE SPACES.                    
            05 FILLER         PIC X(04) VALUE 'SEXO'.                    
            05 FILLER         PIC X(05) VALUE SPACES.                    
            05 FILLER         PIC X(04) VALUE 'NOME'.                    
            05 FILLER         PIC X(46) VALUE SPACES.                    
       *----------------------------------------------------------------*
       *           LAYOUT  DE RELATORIO - AREA DE DETALHE               *
       *----------------------------------------------------------------*
        01 DETALHE.                                                      
            05 FILLER         PIC X(05) VALUE SPACES.                    
            05 COD-DET        PIC 9(04).                                 
            05 FILLER         PIC X(13) VALUE SPACES.                    
            05 SEXO-DET       PIC X(01).                                 
            05 FILLER         PIC X(02) VALUE SPACES.                    
            05 NOME-DET       PIC X(30).                                 
            05 FILLER         PIC X(25) VALUE SPACES.
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        PROCEDURE                           DIVISION.                    
       *----------------------------------------------------------------*
            SORT    TRAB                                                 
               ASCENDING KEY     COD-TRAB                                
               INPUT  PROCEDURE  1000-ROT-ENTRADA                        
               OUTPUT PROCEDURE  2000-ROT-SAIDA.                         
            STOP RUN.                                                    
       *----------------------------------------------------------------*
        1000-ROT-ENTRADA                    SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 1100-INICIO-ENTRADA.                                 
            PERFORM 1300-PRINCIPAL-ENTRADA UNTIL FIM-ARQ EQUAL 'SIM'.    
            PERFORM 1600-FIM-ENTRADA.                                    
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1100-INICIO-ENTRADA                 SECTION.
       *----------------------------------------------------------------*
            OPEN    INPUT   ENTRADA.                                     
            PERFORM 1200-LEITURA-ENTRADA.                                
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1200-LEITURA-ENTRADA                SECTION.                     
       *----------------------------------------------------------------*
            READ    ENTRADA                                              
                    AT END                                               
                    MOVE 'SIM' TO FIM-ARQ.                               
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1300-PRINCIPAL-ENTRADA              SECTION.                     
       *----------------------------------------------------------------*
            PERFORM  1400-SELECAO-ENTRADA.                               
            PERFORM  1200-LEITURA-ENTRADA.                               
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
		      1400-SELECAO-ENTRADA                SECTION.                     
       *----------------------------------------------------------------*
            IF SEXO-ENT = 'M'                                            
               PERFORM 1500-GRAVACAO-ENTRADA.                            
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1500-GRAVACAO-ENTRADA               SECTION.                     
       *----------------------------------------------------------------*
            MOVE COD-ENT   TO COD-TRAB.                                  
            MOVE SEXO-ENT  TO SEXO-TRAB.                                 
            MOVE NOME-ENT TO NOME-TRAB.                                  
            RELEASE REG-TRAB.                                            
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        1600-FIM-ENTRADA                    SECTION.                     
       *----------------------------------------------------------------*
            CLOSE ENTRADA.                                               
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2000-ROT-SAIDA                      SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 2100-INICIO-SAIDA.                                   
            PERFORM 2200-PRINCIPAL-SAIDA UNTIL FIM-ARQ EQUAL 'SIM'.      
            PERFORM 2900-FIM-SAIDA.                                      
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2100-INICIO-SAIDA                   SECTION.                     
       *----------------------------------------------------------------*
            MOVE 'NAO' TO FIM-ARQ.                                       
            OPEN OUTPUT RELAT.                                           
                                                                         
            PERFORM 2150-LEITURA-SAIDA.                                  
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2150-LEITURA-SAIDA                  SECTION.                     
       *----------------------------------------------------------------*
	    RETURN TRAB                                                  
                   AT END                                                
                   MOVE 'SIM' TO FIM-ARQ.                                
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2200-PRINCIPAL-SAIDA                SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 2300-IMPRIMA-SAIDA.                                  
            PERFORM 2150-LEITURA-SAIDA.                                  
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2300-IMPRIMA-SAIDA                  SECTION.                     
       *----------------------------------------------------------------*
            IF WS-CTLIN GREATER 24                                       
               PERFORM 2400-CABECALHO-SAIDA.                             
                                                                         
            PERFORM 2500-DETALHE-SAIDA.                                  
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2400-CABECALHO-SAIDA                SECTION.                     
       *----------------------------------------------------------------*
            ADD  1            TO            WS-CTPAG.                    
            MOVE WS-CTPAG     TO            VAR-PAG.                     
            MOVE SPACES       TO            REG-REL.                     
            WRITE REG-REL.                                               
            WRITE REG-REL.                                               
            WRITE REG-REL     FROM          CAB-01.                      
            WRITE REG-REL.                                               
            WRITE REG-REL.                                               
            WRITE REG-REL     FROM          CAB-02.                      
            WRITE REG-REL.                                               
            WRITE REG-REL.                                               
            WRITE REG-REL     FROM          CAB-03.                      
            WRITE REG-REL.                                               
            MOVE  ZEROES      TO            WS-CTLIN.                    
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2500-DETALHE-SAIDA                  SECTION.                     
       *----------------------------------------------------------------*
            MOVE  COD-TRAB    TO            COD-DET.                     
            MOVE  SEXO-TRAB   TO            SEXO-DET.                    
            MOVE  NOME-TRAB   TO            NOME-DET.                    
            WRITE REG-REL     FROM          DETALHE.                     
            ADD   1           TO            WS-CTLIN.                    
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        2900-FIM-SAIDA                      SECTION.                     
       *----------------------------------------------------------------*
            CLOSE RELAT.                                                 
       *----------------------------------------------------------------*                                                                                                                                                                                 
