       *===============================================================*
        IDENTIFICATION                      DIVISION.                   
       *===============================================================*
        PROGRAM-ID.     VRB661S.                                        
        AUTHOR.         HUGO DA SILVA SAMPAIO.                      
        INSTALLATION.   QINTESS PAULISTA.                               
                                                                        
       *OBJETIVO.       DESENVOLVER UM PROGRAMA PARA LER O ARQUIVO      
       *                DE ENTRADA ARQENT E CALCULA MEDIA DE NOTAS      
       *                E PORCENTAGEM DE ALUNOS NOMA 10.                
                                                                        
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
                                                                        
       *---------------------------------------------------------------*
        DATA                                DIVISION.                   
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
        FILE                                SECTION.                    
       *---------------------------------------------------------------*
                                                                        
        FD  ARQENT                                                      
            LABEL RECORD   ARE STANDARD                                 
            RECORD         CONTAINS 14 CHARACTERS                       
            DATA RECORD    IS REG-ALU                                   
            RECORDING MODE IS F.                                        
                                                                        
        01  REG-ALU.                                                    
            05 REG-COD-ALU                  PIC 9(10).                  
            05 REG-NOTA-ALU                 PIC 9(02)V99.               
                                                                        
       *---------------------------------------------------------------*
        WORKING-STORAGE                     SECTION. 
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                VARIAVEIS DE FILE-STATUS                       *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-FS-ARQENT                    PIC X(02) VALUE ZEROES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                AREA DE ABEND                                  *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-ABENDA                       PIC X(08) VALUE 'ABENDA31'. 
        01  WK-AREA.                                                    
            05 WK-AREA-PGM                  PIC X(08) VALUE 'VRB661S'.  
            05 WK-AREA-STAT                 PIC X(03) VALUE SPACES.
	    05 WK-AREA-MSG                  PIC X(50) VALUE SPACES.     
                                                                        
        01  WK-CODIGO-AREA                  PIC X(30) VALUE SPACES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                VARIAVEIS CONTADORES                           *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-QTD-ALUNOS                   PIC 9(10) VALUE ZEROES.     
        77  WK-QTD-ALUNOS-M                 PIC ZZZZZZZZZ9.             
        77  WK-QTD-NOTA-MAX                 PIC 9(10) VALUE ZEROES.     
       *                                                               *
       *---------------------------------------------------------------*
                                                                        
       *---------------------------------------------------------------*
       *                VARIAVEIS GERAIS                               *
       *---------------------------------------------------------------*
       *                                                               *
        77  WK-TOTAL-NOTAS                  PIC 9(08)V99 VALUE ZEROES.  
        77  WK-TOTAL-NOTAS-M                PIC ZZZ.ZZ9,99.             
        77  WK-MEDIA-NOTAS                  PIC 9(02)V99 VALUE ZEROES.  
        77  WK-MEDIA-NOTAS-M                PIC Z9,99.                  
        77  WK-POR-NOTA-MAX                 PIC 9(03) VALUE ZEROES.     
        77  WK-POR-NOTA-MAX-M               PIC ZZ9.                    
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
                                                                        
            PERFORM 1200-READ               THRU 1200-99-FIM.           
       *                                                               *
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
       *                                                               *
       *---------------------------------------------------------------*
        1100-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
		                                                                      
       *---------------------------------------------------------------*
        1200-READ                           SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            MOVE '1200-READ'     TO         WK-CODIGO-AREA.             
                                                                        
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
            ADD 1                      TO         WK-QTD-ALUNOS.        
                                                                        
            IF REG-NOTA-ALU = 10                                        
               ADD 1                   TO         WK-QTD-NOTA-MAX       
            END-IF.                                                     
                                                                        
            COMPUTE WK-TOTAL-NOTAS = WK-TOTAL-NOTAS + REG-NOTA-ALU.     
                                                                        
            PERFORM 1200-READ               THRU 1200-99-FIM.   
       *                                                               *
       *---------------------------------------------------------------*
        2000-99-FIM.                        EXIT.                       
       *---------------------------------------------------------------*
                                                                        
                                                                        
       *---------------------------------------------------------------*
        3000-FINALIZA                       SECTION.                    
       *---------------------------------------------------------------*
       *                                                               *
            COMPUTE WK-MEDIA-NOTAS = WK-TOTAL-NOTAS / WK-QTD-ALUNOS.    
            COMPUTE WK-POR-NOTA-MAX = WK-QTD-NOTA-MAX * 100             
            COMPUTE WK-POR-NOTA-MAX = WK-POR-NOTA-MAX / WK-QTD-ALUNOS   
                                                                        
            MOVE WK-TOTAL-NOTAS        TO         WK-TOTAL-NOTAS-M.     
            MOVE WK-MEDIA-NOTAS        TO         WK-MEDIA-NOTAS-M.     
            MOVE WK-POR-NOTA-MAX       TO         WK-POR-NOTA-MAX-M.    
            MOVE WK-QTD-ALUNOS         TO         WK-QTD-ALUNOS-M.
		                                                                      
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
            DISPLAY '*           TOTALIZADOR PROGRAMA VRB661S         *'.
            DISPLAY '*------------------------------------------------*'.
            DISPLAY '*  QTD ALUNOS........................: '            
                     WK-QTD-ALUNOS-M '*'                                 
            DISPLAY '*  SOMATORIA DAS NOTAS...............: '            
                     WK-TOTAL-NOTAS-M '*'.                               
            DISPLAY '*                                                *'.
            DISPLAY '*  MEDIA DA ESCOLA...................:      '       
                     WK-MEDIA-NOTAS-M '*'.                               
            DISPLAY '*  % DE ALUNOS NOTA 10...............:       '      
                     WK-POR-NOTA-MAX-M '%*'.                             
            DISPLAY '**************************************************'.
       *                                                               * 
       *---------------------------------------------------------------* 
        3100-99-FIM.                        EXIT.                        
       *---------------------------------------------------------------*     
      
      
