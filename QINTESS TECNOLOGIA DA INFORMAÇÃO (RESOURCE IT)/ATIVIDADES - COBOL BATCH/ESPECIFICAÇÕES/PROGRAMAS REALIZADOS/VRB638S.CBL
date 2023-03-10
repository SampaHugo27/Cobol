       *-----------------------------------------------------------------
        IDENTIFICATION                     DIVISION.                     
        PROGRAM-ID.  VRB638S.                                            
        AUTHOR.      HUGO DA SILVA SAMPAIO.                                            
       *-----------------------------------------------------------------
        ENVIRONMENT                        DIVISION.                     
       *-----------------------------------------------------------------
        CONFIGURATION                      SECTION.                      
       *SOURCE                                                           
       *OBJECT                                                           
        SPECIAL-NAMES.                     DECIMAL-POINT IS COMMA.       
       *-----------------------------------------------------------------
        INPUT-OUTPUT                       SECTION.                      
       *-----------------------------------------------------------------
        FILE-CONTROL.                                                    
       *                                                                 
              SELECT ARQENT ASSIGN TO ARQENT                             
	      FILE STATUS IS WK-FS-ARQENT.                               
       *                                                                 
              SELECT ARQSAI ASSIGN TO ARQSAI                             
              FILE STATUS IS WK-FS-ARQSAI.                               
       *                                                                 
              SELECT RELATO ASSIGN TO RELATO                             
              FILE STATUS IS WK-FS-RELATO.                               
       *-----------------------------------------------------------------
        DATA                               DIVISION.                     
       *-----------------------------------------------------------------
        FILE                               SECTION.                      
       *-----------------------------------------------------------------
        FD ARQENT                  RECORDING MODE F.                     
        01 ENTRADA.                                                      
           05 CPF-E                        PIC X(11).                    
           05 NOME-CLIENTE-E               PIC X(30).                    
           05 VALOR-GASTO                  PIC 9(15)V99.                 
           05 FILLER                       PIC X(02).   
       *                                                                 
        FD ARQSAI                  RECORDING MODE F.                     
        01 SAIDA.                                                        
           05 CPF-S                        PIC X(11).                    
           05 NOME-CLIENTE-S               PIC X(30).                    
           05 VALOR-TOTAL                  PIC 9(15)V99.                 
           05 FILLER                       PIC X(02).                    
       *                                                                 
        FD RELATO                  RECORDING MODE F.                     
        01 REL-LINHA.                                                    
           05 LINHA                    PIC X(80).                        
       *-----------------------------------------------------------------
        WORKING-STORAGE                    SECTION.                      
       *-----------------------------------------------------------------
        77 WK-FS-ARQENT                    PIC X(02)     VALUE SPACES.   
        77 WK-FS-ARQSAI                    PIC X(02)     VALUE SPACES.   
        77 WK-FS-RELATO                    PIC X(02)     VALUE SPACES.   
       *                                                                
	77 WK-TOT-SUM                      PIC 9(15)V99  VALUE 0.        
        77 WK-TOT-REG                      PIC 9(10)     VALUE 0.        
        77 WK-VALOR-TOTAL                  PIC 9(15)V99  VALUE 0.        
       *                                                                 
        77 WK-QUEBRA-ANT                   PIC 9(11)     VALUE 0.        
        77 WK-QUEBRA-ATU                   PIC 9(11)     VALUE 0.        
       *                                                                 
        77 WK-CONT-LINHA                   PIC 9(02)     VALUE 99.       
        77 WK-CONT-PAG                     PIC 9(02)     VALUE 0.        
       *                                                                 
        77 WK-CONT-LEITURA                 PIC 9(04)     VALUE 0.        
        77 WK-CONT-GRAVADO                 PIC 9(04)     VALUE 0.        
        77 WK-CONT-QUEBRA                  PIC 9(04)     VALUE 0.        
       *                                                                 
       ******************************************************************
       ************************* AREA - ABEND  **************************
       ******************************************************************
        77 WK-ABENDA                       PIC X(08)  VALUE 'ABENDA31'. 
	01 WK-AREA.                                                      
           05 WK-AREA-PGM                  PIC X(08)  VALUE 'VRB638S'.   
           05 WK-AREA-STAT                 PIC X(03)  VALUE SPACES.      
           05 WK-AREA-MSG                  PIC X(50)  VALUE SPACES.      
                                                                         
        01 WK-AREA-COD                     PIC X(30)  VALUE SPACES.      
       ******************************************************************
       *********************** AREA - CABECALHO *************************
       ******************************************************************
       *-----------------------------------------------------------------
        01 CAB-01.                                                       
           05 FILLER                       PIC X(29)  VALUE SPACES.      
           05 FILLER                       PIC X(16)                     
                                           VALUE 'BRADESCO CARTOES'.   
           05 FILLER                       PIC X(19)  VALUE SPACES.      
           05 FILLER                       PIC X(06)  VALUE 'DATA: '.    
           05 CAB1-DIA                     PIC 9(02)  VALUE ZEROS.       
           05 FILLER                       PIC X(01)  VALUE '/'.  
	   05 CAB1-MES                     PIC 9(02)  VALUE ZEROS.       
           05 FILLER                       PIC X(01)  VALUE '/'.         
           05 CAB1-ANO                     PIC 9(04)  VALUE ZEROS.       
       *-----------------------------------------------------------------
        01 CAB-02.                                                       
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(08)  VALUE 'VRB638S'.   
           05 FILLER                       PIC X(12)  VALUE SPACES.      
           05 FILLER                       PIC X(31)                     
                                     VALUE 'RELATORIO TOTALIZADOR DE GASTOS'.
           05 FILLER                       PIC X(17)  VALUE SPACES.      
           05 FILLER                       PIC X(08)  VALUE 'PAGINA: '.  
           05 CAB2-PAG                     PIC 9(03)  VALUE ZEROS.       
       *-----------------------------------------------------------------
        01 CAB-03.                                                       
           05 FILLER                       PIC X(80)  VALUE ALL '-'.     
       *-----------------------------------------------------------------
        01 CAB-04.
	   05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(03)  VALUE 'CPF'.       
           05 FILLER                       PIC X(15)  VALUE SPACES.      
           05 FILLER                       PIC X(04)  VALUE 'NOME'.      
           05 FILLER                       PIC X(41)  VALUE SPACES.      
           05 FILLER                       PIC X(11)                     
                                           VALUE 'VALOR TOTAL'.          
       ******************************************************************
       *********************** AREA - DETALHE ***************************
       ******************************************************************
       *-----------------------------------------------------------------
        01 DETALHE.                                                      
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 DET-CPF9                     PIC 9(09)  VALUE ZEROS.       
           05 FILLER                       PIC X(01)  VALUE '-'.         
           05 DET-CPF2                     PIC 9(02)  VALUE ZEROS.       
           05 FILLER                       PIC X(06)  VALUE SPACES.      
           05 DET-NOME                     PIC X(30)  VALUE SPACES.
	   05 FILLER                       PIC X(04)  VALUE SPACES.      
           05 DET-VALOR                    PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZZ,99    
                                           VALUE ZEROS.                  
       *-----------------------------------------------------------------
       ******************************************************************
       *********************** AREA - TOTALIZADOR ***********************
       ******************************************************************
       *-----------------------------------------------------------------
        01 TOTCLI.                                                       
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(31)                     
                                     VALUE 'VALOR TOTAL GASTO PELO CLIENTE '.
           05 TOTCLICP9                    PIC 9(09)    VALUE ZEROS.     
           05 FILLER                       PIC X(01)    VALUE '-'.       
           05 TOTCLICP2                    PIC 9(02)    VALUE ZEROS.     
           05 FILLER                       PIC X(09)                     
                                     VALUE '....:    '.                  
           05 TOTCLIVLR                    PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99 
		                                         VALUE ZEROS.                  
       *-----------------------------------------------------------------
        01 TOT1.                                                         
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(27)                     
                                     VALUE 'TOTAL REGISTROS IMPRESSOS: '.
           05 FILLER                       PIC X(09)    VALUE SPACES.    
           05 TOT1-VALOR-REG               PIC Z.ZZZ.ZZZ.ZZ9             
                                                        VALUE ZEROS.     
       *-----------------------------------------------------------------
        01 TOT2.                                                         
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(27)                     
                                     VALUE 'VALOR TOTAL SUMARIZADO...: '.
           05 TOT2-VALOR-SUM               PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99    
                                           VALUE ZEROS.                  
       *-----------------------------------------------------------------
       ******************************************************************
       ******************** FIM DA WORKING-STORAGE **********************
       ******************************************************************
       *-----------------------------------------------------------------
        PROCEDURE                          DIVISION.                     
       *-----------------------------------------------------------------
        0000-PRINCIPAL                     SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            DISPLAY '**********************************************'     
            DISPLAY '****************INICIANDO PGM.****************'     
            DISPLAY '**********************************************'     
            DISPLAY '                                              '     
       *                                                                 
            PERFORM 0100-INICIO.                                         
            PERFORM 0200-PROCESSA UNTIL WK-FS-ARQENT EQUAL '10'          
            PERFORM 0900-TERMINO.                                        
       *                                                                 
            STOP RUN.
       *                                                                 
        0000-PRINCIPAL-FIM.     EXIT.                                    
       *-----------------------------------------------------------------
        0100-INICIO                        SECTION.                      
       *-----------------------------------------------------------------
            MOVE '0100-INICIO'      TO    WK-AREA-COD                    
                                                                         
            OPEN INPUT    ARQENT.                                        
            IF WK-FS-ARQENT   NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-ARQENT                   
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ABRIR ARQUIVO ENTRADA'                      
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
            OPEN OUTPUT   ARQSAI.  
		          IF WK-FS-ARQSAI   NOT EQUAL '00'                        
               DISPLAY 'ERRO NO: '        WK-AREA-COD               
               DISPLAY 'STATUS:  '        WK-FS-ARQSAI              
               MOVE WK-FS-ARQSAI    TO    WK-AREA-STAT              
               MOVE 'ERRO AO ABRIR ARQUIVO SAIDA'                   
                                    TO    WK-AREA-MSG               
               CALL WK-ABENDA       USING WK-AREA                   
            END-IF.                                                 
       *                                                            
            OPEN OUTPUT   RELATO.                                   
            IF WK-FS-RELATO   NOT EQUAL '00'                        
               DISPLAY 'ERRO NO: '        WK-AREA-COD               
               DISPLAY 'STATUS:  '        WK-FS-RELATO              
               MOVE WK-FS-RELATO    TO    WK-AREA-STAT              
               MOVE 'ERRO AO ABRIR RELATORIO'                       
                                    TO    WK-AREA-MSG               
               CALL WK-ABENDA       USING WK-AREA                   
            END-IF.                                                 
       *                                                             
            PERFORM 0225-LEITURA                                     
                                                                     
            IF  WK-FS-ARQENT      EQUAL '10'                         
               DISPLAY 'ARQUIVO ENTRADA VAZIO'                       
               PERFORM 0150-CABECALHO                                
               MOVE ' ARQUIVO DE ENTRADA VAZIO' TO REL-LINHA         
               WRITE REL-LINHA                                       
               PERFORM 0300-FS-GRAVADET                              
               GO TO 0100-INICIO-FIM                                 
            END-IF.                                                  
                                                                     
            MOVE  CPF-E             TO    WK-QUEBRA-ATU.             
            MOVE  CPF-E             TO    WK-QUEBRA-ANT.             
       *                                                             
            MOVE  CPF-E             TO    CPF-S.                     
            MOVE  NOME-CLIENTE-E    TO    NOME-CLIENTE-S.            
       *
		          MOVE  CPF-E(1:9)        TO    DET-CPF9                       
            MOVE  CPF-E(10:2)       TO    DET-CPF2                       
            MOVE  NOME-CLIENTE-E    TO    DET-NOME.                      
       *                                                                 
        0100-INICIO-FIM.        EXIT.                                    
       *-----------------------------------------------------------------
        0150-CABECALHO                     SECTION.                      
       *-----------------------------------------------------------------
            ADD   1                 TO    WK-CONT-PAG                    
            MOVE  WK-CONT-PAG       TO    CAB2-PAG                       
       *                                                                 
            IF WK-CONT-PAG > 1                                           
               MOVE SPACES TO REL-LINHA                                  
               WRITE REL-LINHA                                           
               PERFORM 0300-FS-GRAVADET                                  
            END-IF.                                                      
       *                                                                 
            MOVE FUNCTION CURRENT-DATE(1:4) TO CAB1-ANO                  
		          MOVE FUNCTION CURRENT-DATE(5:2) TO CAB1-MES 
            MOVE FUNCTION CURRENT-DATE(7:2) TO CAB1-DIA.
       *                                                
            WRITE REL-LINHA         FROM  CAB-01        
            PERFORM 0300-FS-GRAVADET                    
            WRITE REL-LINHA         FROM  CAB-02        
            PERFORM 0300-FS-GRAVADET                    
            WRITE REL-LINHA         FROM  CAB-03        
            PERFORM 0300-FS-GRAVADET                    
            MOVE  SPACES            TO    REL-LINHA     
            WRITE REL-LINHA                             
            PERFORM 0300-FS-GRAVADET                    
            IF WK-FS-ARQENT       EQUAL '10'            
            OR WK-QUEBRA-ATU  NOT EQUAL  WK-QUEBRA-ANT  
               NEXT SENTENCE                            
            ELSE                                        
               WRITE REL-LINHA      FROM  CAB-04        
               PERFORM 0300-FS-GRAVADET                 
	    END-IF.                                                      
            MOVE  5                 TO    WK-CONT-LINHA.                 
       *                                                                 
        0150-CABECALHO-FIM.     EXIT.                                    
       *-----------------------------------------------------------------
        0200-PROCESSA                      SECTION.                      
       *-----------------------------------------------------------------
            IF WK-QUEBRA-ATU  NOT EQUAL  WK-QUEBRA-ANT                   
               PERFORM 0250-GRAVACAO                                     
               PERFORM 0400-TOTCLI                                       
                                                                         
               MOVE CPF-E(1:9)      TO    DET-CPF9                       
               MOVE CPF-E(10:2)     TO    DET-CPF2                       
               MOVE NOME-CLIENTE-E  TO    DET-NOME                       
       *                                                                 
               MOVE  CPF-E          TO    CPF-S                          
               MOVE  NOME-CLIENTE-E TO    NOME-CLIENTE-S                 
               MOVE  0              TO    VALOR-TOTAL
		             MOVE  WK-QUEBRA-ATU  TO    WK-QUEBRA-ANT                  
       *                                                                 
               ADD  1               TO    WK-CONT-QUEBRA                 
            END-IF.                                                      
       *                                                                 
            PERFORM 0350-GRAVA-DETALHE                                   
            ADD   VALOR-GASTO       TO    VALOR-TOTAL                    
            ADD   VALOR-GASTO       TO    WK-TOT-SUM                     
       *                                                                 
            PERFORM 0225-LEITURA.                                        
       *                                                                 
        0200-PROCESSA-FIM.      EXIT.                                    
       *-----------------------------------------------------------------
        0225-LEITURA                       SECTION.                      
       *-----------------------------------------------------------------
            MOVE '0225-LEITURA'     TO    WK-AREA-COD                    
            READ          ARQENT.                                        
            IF  WK-FS-ARQENT  NOT EQUAL '00'              
            AND WK-FS-ARQENT  NOT EQUAL '10'              
               DISPLAY 'ERRO NO: '        WK-AREA-COD     
               DISPLAY 'STATUS:  '        WK-FS-ARQENT    
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT    
               MOVE 'ERRO AO LER ARQUIVO ENTRADA'         
                                    TO    WK-AREA-MSG     
               CALL WK-ABENDA       USING WK-AREA         
            ELSE                                          
               IF WK-FS-ARQENT    EQUAL '10'              
                  GO TO 0225-LEITURA-FIM                  
               END-IF                                     
            END-IF.                                       
       *                                                  
            ADD   1                 TO    WK-CONT-LEITURA.
            MOVE  CPF-E             TO    WK-QUEBRA-ATU.  
       *                                                  
        0225-LEITURA-FIM.       EXIT. 
       *-----------------------------------------------------------------
        0250-GRAVACAO                      SECTION.                      
       *-----------------------------------------------------------------
            MOVE '0250-GRAVACAO'    TO    WK-AREA-COD                    
            MOVE  VALOR-TOTAL       TO    WK-VALOR-TOTAL                 
            WRITE SAIDA                                                  
                                                                         
            IF  WK-FS-ARQSAI  NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-ARQSAI                   
               MOVE  WK-FS-ARQSAI   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO GRAVAR NO ARQ. SAIDA'                       
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
            ADD  1                  TO    WK-CONT-GRAVADO.

        0250-GRAVACAO-FIM.      EXIT.                                    
       *-----------------------------------------------------------------
        0300-FS-GRAVADET                   SECTION.                      
       *-----------------------------------------------------------------
            MOVE '0300-FS-GRAVACAO' TO    WK-AREA-COD                    
            IF WK-FS-RELATO   NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-RELATO                   
               MOVE  WK-FS-RELATO   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO GRAVAR NO ARQ. RELATO'                      
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
        0300-FS-GRAVADET-FIM.   EXIT.                                    
       *-----------------------------------------------------------------
        0350-GRAVA-DETALHE                 SECTION.                      
       *-----------------------------------------------------------------  
	    IF  WK-CONT-LINHA > 49                                       
               PERFORM 0150-CABECALHO                                    
            END-IF.                                                      
       *                                                                 
            MOVE  VALOR-GASTO       TO    DET-VALOR                      
            WRITE REL-LINHA         FROM  DETALHE                        
            PERFORM 0300-FS-GRAVADET                                     
            ADD    1                TO    WK-TOT-REG.                    
            ADD    1                TO    WK-CONT-LINHA.                 
                                                                         
        0350-GRAVA-DETALHE-FIM. EXIT.                                    
       *-----------------------------------------------------------------
        0375-TOTALIZADOR                   SECTION.                      
       *-----------------------------------------------------------------
                                                                         
            IF WK-CONT-LINHA > 47                                        
               PERFORM 0150-CABECALHO                                    
            END-IF.
		                                                                       
            MOVE  WK-TOT-REG        TO    TOT1-VALOR-REG                 
            MOVE  WK-TOT-SUM        TO    TOT2-VALOR-SUM                 
                                                                         
            MOVE SPACES             TO    REL-LINHA                      
            WRITE REL-LINHA                                              
            PERFORM 0300-FS-GRAVADET                                     
            WRITE REL-LINHA         FROM  TOT1                           
            PERFORM 0300-FS-GRAVADET                                     
            WRITE REL-LINHA         FROM  TOT2                           
            PERFORM 0300-FS-GRAVADET.                                    
                                                                         
        0375-TOTALIZADOR-FIM.   EXIT.                                    
       *-----------------------------------------------------------------
        0400-TOTCLI                        SECTION.                      
       *-----------------------------------------------------------------
            MOVE DET-CPF9           TO    TOTCLICP9                      
            MOVE DET-CPF2           TO    TOTCLICP2                      
	    MOVE WK-VALOR-TOTAL     TO    TOTCLIVLR  
            MOVE SPACES             TO    REL-LINHA  
                                                     
            IF WK-CONT-LINHA > 45                    
               PERFORM 0150-CABECALHO                
            END-IF.                                  
                                                     
            WRITE REL-LINHA                          
            PERFORM 0300-FS-GRAVADET                 
            WRITE REL-LINHA         FROM    TOTCLI   
            PERFORM 0300-FS-GRAVADET                 
            IF WK-FS-ARQENT   NOT EQUAL '10'         
               WRITE REL-LINHA                       
               PERFORM 0300-FS-GRAVADET              
               WRITE REL-LINHA                       
               PERFORM 0300-FS-GRAVADET              
               WRITE REL-LINHA      FROM    CAB-04   
               PERFORM 0300-FS-GRAVADET 
	       ADD     5            TO    WK-CONT-LINHA                  
            END-IF.                                                      
                                                                         
        0400-TOTCLI-EXIT.       EXIT.                                    
       *-----------------------------------------------------------------
        0900-TERMINO                       SECTION.                      
       *-----------------------------------------------------------------
            MOVE '0900-TERMINO'     TO    WK-AREA-COD                    
            IF  WK-CONT-LEITURA    >=   1                                
               PERFORM 0250-GRAVACAO                                     
               ADD  1               TO    WK-CONT-QUEBRA                 
               PERFORM 0400-TOTCLI                                       
               ADD  2               TO    WK-CONT-LINHA                  
            END-IF.                                                      
                                                                         
            PERFORM 0375-TOTALIZADOR.                                    
                                                                         
            CLOSE         ARQENT.
	    IF WK-FS-ARQENT   NOT EQUAL '00'           
               DISPLAY 'ERRO NO: '        WK-AREA-COD  
               DISPLAY 'STATUS:  '        WK-FS-ARQENT 
               MOVE  WK-FS-ARQENT   TO    WK-AREA-STAT 
               MOVE 'ERRO AO FECHAR ARQUIVO ENTRADA'   
                                    TO    WK-AREA-MSG  
               CALL  WK-ABENDA      USING WK-AREA      
            END-IF.                                    
       *                                               
            CLOSE         ARQSAI.                      
            IF WK-FS-ARQSAI   NOT EQUAL '00'           
               DISPLAY 'ERRO NO: '        WK-AREA-COD  
               DISPLAY 'STATUS:  '        WK-FS-ARQSAI 
               MOVE  WK-FS-ARQSAI   TO    WK-AREA-STAT 
               MOVE 'ERRO AO FECHAR ARQUIVO SAIDA'     
                                    TO    WK-AREA-MSG  
               CALL  WK-ABENDA      USING WK-AREA      
            END-IF.  
       *                                                            
            CLOSE         RELATO.                                   
            IF WK-FS-RELATO   NOT EQUAL '00'                        
               DISPLAY 'ERRO NO: '        WK-AREA-COD               
               DISPLAY 'STATUS:  '        WK-FS-RELATO              
               MOVE  WK-FS-RELATO   TO    WK-AREA-STAT              
               MOVE 'ERRO AO FECHAR RELATORIO'                      
                                    TO    WK-AREA-MSG               
               CALL  WK-ABENDA      USING WK-AREA                   
            END-IF.                                                 
       *                                                            
            DISPLAY 'QTD DE LEITURAS: ' WK-CONT-LEITURA '          '
            DISPLAY 'QTD DE GRAVADOS: ' WK-CONT-GRAVADO '          '
            DISPLAY 'QTD DE QUEBRAS : ' WK-CONT-QUEBRA  '          '
            DISPLAY 'QTD DE PAGINAS : ' WK-CONT-PAG     '          '
            DISPLAY 'QTD DE REGISTRO: ' WK-TOT-REG      '          '
            DISPLAY 'VLR DA SOMA    : ' WK-TOT-SUM      '          '
            DISPLAY '                                              '                                                                                                                                                         
	    DISPLAY '**********************************************'     
            DISPLAY '**********PGM EXECUTADO COM SUCESSO.**********'     
            DISPLAY '**********************************************'.    
                                                                         
        0900-TERMINO-FIM.       EXIT.                                    
       *-----------------------------------------------------------------    
      
      
