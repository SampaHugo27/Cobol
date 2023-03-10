       *===============================================================*
        IDENTIFICATION                      DIVISION.                   
       *===============================================================*
        PROGRAM-ID.     VRB659S.                                        
        AUTHOR.         HUGO DA SILVA SAMPAIO.                    
        INSTALLATION.   QINTESS PAULISTA.                               
                                                                        
       *OBJETIVO.       DESENVOLVER UM PROGRAMA PARA LER O ARQUIVO      
       *                DE ENTRADA ARQENT E GRAVAR NA SAIDA APENAS      
       *                CLIENTES QUE FAZEM ANIVERSARIO NO MES CORRENTE. 
                                                                        
       *===============================================================*
        ENVIRONMENT                         DIVISION.                   
       *===============================================================*
                                                                        
       *---------------------------------------------------------------*
        CONFIGURATION                       SECTION.                    
       *---------------------------------------------------------------*
		                                                                      
        SOURCE-COMPUTER.                    IBM-PC.                     
        OBJECT-COMPUTER.                    IBM-PC.                     
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.     
                                                                        
       *---------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.                    
       *---------------------------------------------------------------*
                                                                        
        FILE-CONTROL.                                                   
                                                                        
            SELECT ARQENT  ASSIGN TO ARQENT                             
            ORGANIZATION   IS  SEQUENTIAL                               
            ACCESS MODE    IS  SEQUENTIAL                               
            FILE STATUS    IS  WK-FS-ARQENT.                            
                                                                        
            SELECT ARQSAI  ASSIGN TO ARQSAI                             
            ORGANIZATION   IS  SEQUENTIAL
	    ACCESS MODE    IS  SEQUENTIAL                               
            FILE STATUS    IS  WK-FS-ARQSAI.                            
                                                                        
       *---------------------------------------------------------------*
        DATA                                DIVISION.                   
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        FILE                                SECTION.                    
       *---------------------------------------------------------------*
                                                                        
        FD  ARQENT                                                      
            LABEL RECORD   ARE STANDARD                                 
            RECORD         CONTAINS 60 CHARACTERS                       
            DATA RECORD    IS REG-ENT                                   
            RECORDING MODE IS F.                                        
                                                                        
        01  REG-ENT.
	    05 ENT-CPF                      PIC 9(11) VALUE ZEROES.     
            05 ENT-NOME-CLIENTE             PIC X(30) VALUE SPACES.     
            05 ENT-CONTA                    PIC 9(10) VALUE ZEROES.     
            05 ENT-STATUS                   PIC X(01) VALUE SPACES.     
       *    (FORMATO AAAAMMDD)                                          
            05 ENT-NASCTO.                                              
                10 ENT-ANO                  PIC 9(04) VALUE ZEROES.     
                10 ENT-MES                  PIC 9(02) VALUE ZEROES.     
                10 ENT-DIA                  PIC 9(02) VALUE ZEROES.     
                                                                        
       *---------------------------------------------------------------*
                                                                        
        FD  ARQSAI                                                      
            LABEL RECORD   ARE STANDARD                                 
            RECORD         CONTAINS 60 CHARACTERS                       
            DATA RECORD    IS REG-SAI                                   
            RECORDING MODE IS F.
	01  REG-SAI.                                                     
            05 SAI-CPF                      PIC 9(11) VALUE ZEROES.      
            05 SAI-NOME-CLIENTE             PIC X(30) VALUE SPACES.      
            05 SAI-CONTA                    PIC 9(10) VALUE ZEROES.      
            05 SAI-STATUS                   PIC X(01) VALUE SPACES.      
            05 SAI-NASCTO                   PIC 9(08) VALUE ZEROES.      
                                                                         
       *---------------------------------------------------------------* 
        WORKING-STORAGE                     SECTION.                     
       *---------------------------------------------------------------* 
                                                                         
       *---------------------------------------------------------------* 
       *                VARIAVEIS DE FILE-STATUS                       * 
       *---------------------------------------------------------------* 
       *                                                               * 
        77  WK-FS-ARQENT                    PIC X(02) VALUE ZEROES.      
        77  WK-FS-ARQSAI                    PIC X(02) VALUE ZEROES.      
       *                                                               * 
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                AREA DE ABEND                                  *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-ABENDA                       PIC X(08) VALUE 'ABENDA31'. 
        01  WK-AREA.                                                    
            05 WK-AREA-PGM                  PIC X(08) VALUE 'VRB659S'.  
            05 WK-AREA-STAT                 PIC X(03) VALUE SPACES.     
            05 WK-AREA-MSG                  PIC X(50) VALUE SPACES.     
                                                                        
        01  WK-CODIGO-AREA                  PIC X(30) VALUE SPACES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                VARIAVEIS CONTADORES                           *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-QTD-READS                    PIC 9(10) VALUE ZEROES.     
        77  WK-QTD-WRITES                   PIC 9(10) VALUE ZEROES.     
        77  WK-QTD-ATIVAS                   PIC 9(10) VALUE ZEROES.     
        77  WK-QTD-NAO-ATIVAS               PIC 9(10) VALUE ZEROES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                VARIAVEIS GERAIS                               *
       *---------------------------------------------------------------*
       *                                                               *
        01  WK-DATA-ATUAL.                                              
            05 WK-ANO-ATUAL                 PIC 9(04) VALUE ZEROES.     
            05 WK-MES-ATUAL                 PIC 9(02) VALUE ZEROES.     
            05 WK-DIA-ATUAL                 PIC 9(02) VALUE ZEROES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        PROCEDURE                           DIVISION.                   
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        0000-PRINCIPAL                      SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            PERFORM 1000-INICIO        THRU  1000-99-FIM.               
            PERFORM 2000-PROCESSA      THRU  2000-99-FIM                
                                       UNTIL WK-FS-ARQENT EQUAL '10'.   
            PERFORM 3000-FINALIZA      THRU  3000-99-FIM.               
            STOP RUN.                                                   
       *                                                               *
       *---------------------------------------------------------------*
        0000-99-FIM.                        EXIT.
       *---------------------------------------------------------------* 
                                                                         
       *---------------------------------------------------------------* 
        1000-INICIO                         SECTION.                     
       *---------------------------------------------------------------* 
            DISPLAY '**************************************************'.
            DISPLAY '           INICIANDO O PROGRAMA...                '.
            DISPLAY '**************************************************'.
            DISPLAY '                                                  '.
       *---------------------------------------------------------------* 
       *                                                               * 
                                                                         
       *---------------------------------------------------------------* 
       * ABERTURA DE ARQUIVOS E VERIFICACA O DE FILE STATUS.           * 
                                                                         
            PERFORM 1100-ABERTURA-ARQUIVOS  THRU 1100-99-FIM.            
       *                                                               * 
       *---------------------------------------------------------------*
       * PRIMEIRA LEITURA E FILE STATUS PARA ARQUIVO VAZIO             *
                                                                        
            PERFORM 1200-FIRST-READ         THRU 1200-99-FIM.           
       *                                                               *
       *---------------------------------------------------------------*
       * MOVE M??S ATUAL TO WK-MES-ATUAL                                *
            ACCEPT WK-DATA-ATUAL       FROM       DATE YYYYMMDD.        
       *                                                                
       *---------------------------------------------------------------*
        1000-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        1100-ABERTURA-ARQUIVOS              SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            MOVE '1100-ABERTURA-ARQUIVOS' TO      WK-CODIGO-AREA.
	    OPEN INPUT  ARQENT.                                    
            IF WK-FS-ARQENT            NOT EQUAL  '00' AND '10'    
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA   
               DISPLAY 'STATUS:  '                WK-FS-ARQENT     
               MOVE WK-FS-ARQENT       TO         WK-AREA-STAT     
               MOVE 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQENT'
                                       TO         WK-AREA-MSG      
               CALL WK-ABENDA          USING      WK-AREA          
            END-IF.                                                
                                                                   
            OPEN OUTPUT ARQSAI.                                    
            IF WK-FS-ARQSAI            NOT EQUAL '00'              
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA   
               DISPLAY 'STATUS:  '                WK-FS-ARQSAI     
               MOVE WK-FS-ARQSAI       TO         WK-AREA-STAT     
               MOVE 'ERRO NA ABERTURA DO ARQUIVO DE SAIDA ARQSAI'  
                                       TO         WK-AREA-MSG      
               CALL WK-ABENDA          USING      WK-AREA
	    END-IF.                                                     
       *                                                               *
       *---------------------------------------------------------------*
        1100-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
                                                                        
                                                                        
       *---------------------------------------------------------------*
        1200-FIRST-READ                     SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            MOVE '1200-FIRST-READ'     TO         WK-CODIGO-AREA.       
                                                                        
            READ ARQENT.                                                
            IF WK-FS-ARQENT            NOT EQUAL  '00' AND '10'         
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA        
               DISPLAY 'STATUS:  '                WK-FS-ARQENT          
               MOVE WK-FS-ARQENT       TO         WK-AREA-STAT
	       MOVE 'ERRO NA LEITURA DO ARQUIVO DE ENTRADA ARQENT'      
                                       TO         WK-AREA-MSG           
               CALL WK-ABENDA          USING      WK-AREA               
            END-IF.                                                     
       *                                                               *
       *---------------------------------------------------------------*
        1200-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        2000-PROCESSA                       SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            ADD 1                      TO         WK-QTD-READS.         
                                                                        
            IF ENT-MES = WK-MES-ATUAL                                   
               MOVE REG-ENT            TO         REG-SAI               
               WRITE REG-SAI
		                                                                 
               MOVE '2000-PROCESSA'    TO         WK-CODIGO-AREA.  
                                                                   
               IF WK-FS-ARQSAI            NOT EQUAL  '00'          
                  DISPLAY 'ERRO NO: '                WK-CODIGO-AREA
                  DISPLAY 'STATUS:  '                WK-FS-ARQSAI  
                  MOVE WK-FS-ARQSAI       TO         WK-AREA-STAT  
                  MOVE 'ERRO NA ESCRITA DO ARQUIVO DE SAIDA ARQSAI'
                                          TO         WK-AREA-MSG   
                  CALL WK-ABENDA          USING      WK-AREA       
               END-IF.                                             
                                                                   
               ADD 1                   TO         WK-QTD-WRITES    
               ADD 1                   TO         WK-QTD-ATIVAS    
            ELSE                                                   
               ADD 1                   TO         WK-QTD-NAO-ATIVAS
            END-IF. 

	    READ ARQENT.                                                
                                                                        
            IF WK-FS-ARQENT            NOT EQUAL  '00' AND '10'         
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA        
               DISPLAY 'STATUS:  '                WK-FS-ARQENT          
               MOVE WK-FS-ARQENT       TO         WK-AREA-STAT          
               MOVE 'ERRO NA LEITURA DO ARQUIVO DE ENTRADA ARQENT'      
                                       TO         WK-AREA-MSG           
               CALL WK-ABENDA          USING      WK-AREA               
            END-IF.                                                     
       *                                                               *
       *---------------------------------------------------------------*
        2000-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
                                                                                                                      
       *---------------------------------------------------------------*
        3000-FINALIZA                       SECTION.
       *---------------------------------------------------------------*
       *                                                               *
            MOVE '3000-FINALIZA'       TO         WK-CODIGO-AREA.       
                                                                        
            CLOSE ARQENT.                                               
            IF WK-FS-ARQENT            NOT EQUAL  '00'                  
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA        
               DISPLAY 'STATUS:  '                WK-FS-ARQENT          
               MOVE WK-FS-ARQENT       TO         WK-AREA-STAT          
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA ARQENT'   
                                       TO         WK-AREA-MSG           
               CALL WK-ABENDA          USING      WK-AREA               
            END-IF.                                                     
                                                                        
            CLOSE ARQSAI.                                               
            IF WK-FS-ARQSAI            NOT EQUAL  '00'                  
               DISPLAY 'ERRO NO: '                WK-CODIGO-AREA        
               DISPLAY 'STATUS:  '                WK-FS-ARQSAI
       	       MOVE WK-FS-ARQSAI       TO         WK-AREA-STAT           
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE SAIDA ARQSAI'      
                                       TO         WK-AREA-MSG            
               CALL WK-ABENDA          USING      WK-AREA                
            END-IF.                                                      
                                                                         
            PERFORM 3100-DISPLAY-FIM        THRU 3100-99-FIM.            
       *                                                               * 
       *---------------------------------------------------------------* 
        3000-99-FIM.                        EXIT.                        
       *---------------------------------------------------------------* 
                                                                         
       *---------------------------------------------------------------* 
        3100-DISPLAY-FIM                    SECTION.                     
       *---------------------------------------------------------------* 
       *                                                               * 
            DISPLAY '**************************************************'.
            DISPLAY '*           TOTALIZADOR PROGRAMA VRB659S         *'.
            DISPLAY '*------------------------------------------------*'.
            DISPLAY '*  REGISTROS LIDOS ARQENT............: '            
                     WK-QTD-READS '*'                                    
            DISPLAY '*  REGISTROS GRAVADOS ARQSAI.........: '            
                     WK-QTD-WRITES '*'.                                  
            DISPLAY '*                                                *'.
            DISPLAY '*  QTDE DE ANIVERSARIANTES DO MES 'WK-MES-ATUAL'.: '
                     WK-QTD-ATIVAS '*'.                                  
            DISPLAY '*  QTDE DE CONTAS NAO ATIVAS.........: '            
                     WK-QTD-NAO-ATIVAS '*'.                              
            DISPLAY '**************************************************'.
       *                                                               * 
       *---------------------------------------------------------------* 
        3100-99-FIM.                        EXIT.                        
       *---------------------------------------------------------------*      
      
