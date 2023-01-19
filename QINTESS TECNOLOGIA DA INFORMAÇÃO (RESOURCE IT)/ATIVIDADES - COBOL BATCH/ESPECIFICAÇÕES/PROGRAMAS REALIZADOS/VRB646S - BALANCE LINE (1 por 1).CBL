      	*=================================================================
        IDENTIFICATION                     DIVISION.                     
       *=================================================================
        PROGRAM-ID.  VRB646S.                                            
        AUTHOR.      HUGO SAMPAIO/QINTESS.                                             
       *=================================================================
       *-----------------------------------------------------------------
       * OBJETIVO: IDENTIFICAR CLIENTES QUE TEM RESTRICOES NO SERASA     
       * USANDO BALANCE-LINE                                             
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
       *                                                                 
       * INPUT  - ARQENT                                                 
       *          SERASA                                                 
       * OUTPUT - ARQSAI                                                 
       *          RELATO                                                 
       *                                                                 
       *-----------------------------------------------------------------
       *=================================================================
        ENVIRONMENT                        DIVISION.                     
       *=================================================================
        CONFIGURATION                      SECTION.                      
       *-----------------------------------------------------------------
       *SOURCE-COMPUTER                                                  
       *OBJECT-COMPUTER                                                  
        SPECIAL-NAMES.                     DECIMAL-POINT IS COMMA.       
       *=================================================================
        INPUT-OUTPUT                       SECTION.                      
       *-----------------------------------------------------------------
        FILE-CONTROL.                                                    
                                                                         
              SELECT ARQENT ASSIGN TO ARQENT                             
              FILE STATUS IS WK-FS-ARQENT.                               
                                                                         
              SELECT SERASA ASSIGN TO SERASA                             
              FILE STATUS IS WK-FS-SERASA.
		                                                                       
              SELECT ARQSAI ASSIGN TO ARQSAI                             
              FILE STATUS IS WK-FS-ARQSAI.                               
                                                                         
              SELECT RELATO ASSIGN TO RELATO                             
              FILE STATUS IS WK-FS-RELATO.                               
       *                                                                 
       *-----------------------------------------------------------------
       *=================================================================
        DATA                               DIVISION.                     
       *=================================================================
       *-----------------------------------------------------------------
        FILE                               SECTION.                      
       *-----------------------------------------------------------------
        FD ARQENT                          RECORDING MODE F.             
                                                                         
        01 ARQ-ENT.                                                      
           03 CPF-ENT                      PIC 9(11).                    
	   03 NOME-CLIENTE-ENT             PIC X(30).       
           03 VALOR-GASTO-ENT              PIC 9(15)V99.    
           03 FILLER                       PIC 9(02).       
                                                            
        FD SERASA                          RECORDING MODE F.
                                                            
        01 ARQ-SER.                                         
           03 CPF-SER                      PIC 9(11).       
           03 NOME-CLIENTE-SER             PIC X(30).       
           03 TOTAL-DIVIDA-SER             PIC 9(15)V99.    
           03 FILLER                       PIC 9(02).       
                                                            
        FD ARQSAI                          RECORDING MODE F.
                                                            
        01 ARQ-SAI.                                         
           03 CPF-SAI                      PIC 9(11).       
           03 NOME-CLIENTE-SAI             PIC X(30).       
           03 TOTAL-DIVIDA-SAI             PIC 9(15)V99. 
	   03 FILLER                       PIC 9(02).                    
                                                                         
        FD RELATO                          RECORDING MODE F.             
                                                                         
        01 REL-LINHA.                                                    
           05 LINHA                        PIC X(80).                    
       *-----------------------------------------------------------------
        WORKING-STORAGE                    SECTION.                      
       *-----------------------------------------------------------------
                                                                         
        77 WK-FS-ARQENT                    PIC X(02)  VALUE SPACES.      
        77 WK-FS-SERASA                    PIC X(02)  VALUE SPACES.      
        77 WK-FS-ARQSAI                    PIC X(02)  VALUE SPACES.      
        77 WK-FS-RELATO                    PIC X(02)  VALUE SPACES.      
                                                                         
        77 WK-DATA                         PIC 9(08)  VALUE ZEROES.      
        77 WK-CPF-ENT                      PIC X(11)  VALUE ZEROES.      
        77 WK-CPF-SER                      PIC X(11)  VALUE ZEROES.
		                                                                       
        77 WK-CONT-LINHA                   PIC 9(02)  VALUE 99.          
        77 WK-CONT-PAG                     PIC 9(02)  VALUE ZEROS.       
        77 WK-TOT-REG                      PIC 9(04)  VALUE ZEROS.       
        77 WK-TOT-VLR                      PIC 9(15)V99 VALUE ZEROS.     
                                                                         
        77 WK-CONT-CPF                     PIC 9(04)  VALUE ZEROS.       
        77 WK-CONT-LER-ARQENT              PIC 9(04)  VALUE ZEROS.       
        77 WK-CONT-LER-SERASA              PIC 9(04)  VALUE ZEROS.       
        77 WK-CONT-GRAVADO                 PIC 9(04)  VALUE ZEROS.       
        77 WK-CONT-QUEBRA                  PIC 9(04)  VALUE ZEROS.       
                                                                         
       *-----------------------------------------------------------------
       ******************************************************************
       ************************* AREA - ABEND  **************************
       ******************************************************************
       *-----------------------------------------------------------------
        77 WK-ABENDA                       PIC X(08)  VALUE 'ABENDA31'.
	01 WK-AREA.                                                      
           05 WK-AREA-PGM                  PIC X(08)  VALUE 'VRB646S'.   
           05 WK-AREA-STAT                 PIC X(03)  VALUE SPACES.      
           05 WK-AREA-MSG                  PIC X(50)  VALUE SPACES.      
                                                                         
        01 WK-AREA-COD                     PIC X(30)  VALUE SPACES.      
       *-----------------------------------------------------------------
       ******************************************************************
       *********************** AREA - CABECALHO *************************
       ******************************************************************
       *-----------------------------------------------------------------
        01 CAB-01.                                                       
           05 FILLER                       PIC X(29)  VALUE SPACES.      
           05 FILLER                       PIC X(16)  VALUE                    
                                           'BRADESCO CARTOES'.   
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
           05 FILLER                       PIC X(08)  VALUE 'VRB646S'.   
           05 FILLER                       PIC X(08)  VALUE SPACES.      
           05 FILLER                       PIC X(42)  VALUE                   
                          'RELATORIO CLIENTES COM RESTRICOES - SERASA'. 
           05 FILLER                       PIC X(10)  VALUE SPACES.      
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
           05 FILLER                       PIC X(34)  VALUE SPACES.      
           05 FILLER                       PIC X(18)                     
                                           VALUE 'VALOR TOTAL DIVIDA'.   
       *-----------------------------------------------------------------
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
           05 DET-VALOR                    PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99    
                                           VALUE ZEROS.                  
       *-----------------------------------------------------------------
       ******************************************************************
       *********************** AREA - TOTALIZADOR ***********************
       ******************************************************************
       *-----------------------------------------------------------------
        01 TOTDIV.                                                       
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(18)                     
                                           VALUE 'VALOR TOTAL DIVIDA'.   
           05 FILLER                       PIC X(30)                     
                                  VALUE '.............................:'.
           05 FILLER                       PIC X(04) VALUE SPACES.       
           05 TOT-VLRDIV                   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
		                                         VALUE ZEROS.                  
       *-----------------------------------------------------------------
        01 TOTREG.                                                       
           05 FILLER                       PIC X(01)  VALUE SPACES.      
           05 FILLER                       PIC X(26)                     
                                      VALUE 'TOTAL REGISTROS IMPRESSOS:'.
           05 FILLER                       PIC X(10)  VALUE SPACES.      
           05 TOT-REG                      PIC Z.ZZZ.ZZZ.ZZ9             
                                           VALUE ZEROES.                 
       *-----------------------------------------------------------------
       ******************************************************************
       ******************** FIM DA WORKING-STORAGE **********************
       ******************************************************************
       *-----------------------------------------------------------------
       *=================================================================
        PROCEDURE                          DIVISION.                     
       *=================================================================
        0000-PRINCIPAL                     SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            DISPLAY '**************************************************' 
            DISPLAY '******************INICIANDO PGM.******************' 
            DISPLAY '**************************************************' 
            DISPLAY '                                                  ' 
       *                                                                 
            PERFORM 1000-INICIO.                                         
            PERFORM 2000-PROCESSA UNTIL                                  
                    WK-FS-ARQENT EQUAL '10' AND WK-FS-SERASA EQUAL '10'  
            PERFORM 9000-TERMINO.                                        
       *                                                                 
            STOP RUN.                                                    
       *                                                                 
        0000-PRINCIPAL-EXIT.           EXIT.                             
       *-----------------------------------------------------------------
        1000-INICIO                          SECTION.                    
       *-----------------------------------------------------------------
	    MOVE '1000-INICIO'      TO    WK-AREA-COD 
                                                      
            OPEN INPUT    ARQENT.                     
            IF WK-FS-ARQENT   NOT EQUAL '00'          
               DISPLAY 'ERRO NO: '        WK-AREA-COD 
               DISPLAY 'STATUS:  '        WK-FS-ARQENT
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT
               MOVE 'ERRO AO ABRIR ARQENT'            
                                    TO    WK-AREA-MSG 
               CALL WK-ABENDA       USING WK-AREA     
            END-IF.                                   
       *                                              
            OPEN INPUT    SERASA.                     
            IF WK-FS-SERASA   NOT EQUAL '00'          
               DISPLAY 'ERRO NO: '        WK-AREA-COD 
               DISPLAY 'STATUS:  '        WK-FS-SERASA
               MOVE WK-FS-SERASA    TO    WK-AREA-STAT
               MOVE 'ERRO AO ABRIR SERASA'
		                                  TO    WK-AREA-MSG 
               CALL WK-ABENDA       USING WK-AREA     
            END-IF.                                   
       *                                              
            OPEN OUTPUT   ARQSAI.                     
            IF WK-FS-ARQSAI   NOT EQUAL '00'          
               DISPLAY 'ERRO NO: '        WK-AREA-COD 
               DISPLAY 'STATUS:  '        WK-FS-ARQSAI
               MOVE WK-FS-ARQSAI    TO    WK-AREA-STAT
               MOVE 'ERRO AO ABRIR ARQSAI'            
                                    TO    WK-AREA-MSG 
               CALL WK-ABENDA       USING WK-AREA     
            END-IF.                                   
       *                                              
            OPEN OUTPUT   RELATO.                     
            IF WK-FS-RELATO   NOT EQUAL '00'          
               DISPLAY 'ERRO NO: '        WK-AREA-COD 
               DISPLAY 'STATUS:  '        WK-FS-RELATO
	       MOVE WK-FS-RELATO    TO    WK-AREA-STAT                  
               MOVE 'ERRO AO ABRIR RELATO'                              
                                    TO    WK-AREA-MSG                   
               CALL WK-ABENDA       USING WK-AREA                       
            END-IF.                                                     
       *                                                                
            PERFORM 1250-LER-ARQENT                                     
                                                                        
            IF WK-FS-ARQENT EQUAL '10'                                  
               DISPLAY 'ARQUIVO ENTRADA VAZIO'                          
               PERFORM 1750-CABECALHO                                   
               MOVE ' ARQENT VAZIO, BATIMENTO NAO POSSIVEL' TO REL-LINHA
               WRITE REL-LINHA                                          
               PERFORM 1900-FS-GRAVADET                                 
               GO TO 1000-INICIO-FIM                                    
            END-IF.                                                     
                                                                        
            PERFORM 1500-LER-SERASA
		                                                                       
            IF WK-FS-SERASA EQUAL '10'                                   
               DISPLAY 'ARQUIVO SERASA VAZIO'                            
               PERFORM 1750-CABECALHO                                    
               MOVE ' SERASA VAZIO, BATIMENTO NAO POSSIVEL' TO REL-LINHA 
               WRITE REL-LINHA                                           
               PERFORM 1900-FS-GRAVADET                                  
               GO TO 1000-INICIO-FIM                                     
            END-IF.                                                      
       *                                                                 
        1000-INICIO-FIM.            EXIT.                                
       *-----------------------------------------------------------------
        1250-LER-ARQENT                    SECTION.                      
       *-----------------------------------------------------------------
            MOVE '1250-ARQENT'      TO    WK-AREA-COD                    
            READ          ARQENT                                         
                                                                         
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
                  MOVE HIGH-VALUES TO WK-CPF-ENT             
                  GO TO 1250-LER-ARQENT-FIM                  
               END-IF                                        
            END-IF.                                          
       *                                                     
            MOVE CPF-ENT            TO    WK-CPF-ENT         
            ADD   1                 TO    WK-CONT-LER-ARQENT.
       *                                                     
        1250-LER-ARQENT-FIM.        EXIT.
       *-----------------------------------------------------------------
        1500-LER-SERASA                    SECTION.                      
       *-----------------------------------------------------------------
            MOVE '1500-SERASA'      TO    WK-AREA-COD                    
            READ          SERASA                                         
                                                                         
            IF  WK-FS-SERASA  NOT EQUAL '00'                             
            AND WK-FS-SERASA  NOT EQUAL '10'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-SERASA                   
               MOVE WK-FS-SERASA    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO LER ARQUIVO ENTRADA'                        
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            ELSE                                                         
               IF WK-FS-SERASA    EQUAL '10'                             
                  MOVE HIGH-VALUES TO WK-CPF-SER                         
                  GO TO 1500-LER-SERASA-FIM
	       END-IF                                                    
            END-IF.                                                      
       *                                                                 
            MOVE CPF-SER             TO    WK-CPF-SER                     
            ADD   1                  TO    WK-CONT-LER-SERASA.            
       *                                                                 
        1500-LER-SERASA-FIM.         EXIT.                                
       *-----------------------------------------------------------------
        1750-CABECALHO                     SECTION.                      
       *-----------------------------------------------------------------
            ADD   1                  TO    WK-CONT-PAG                    
            MOVE  WK-CONT-PAG        TO    CAB2-PAG                       
       *                                                                 
            IF WK-CONT-PAG > 1                                           
               MOVE SPACES TO REL-LINHA                                  
               WRITE REL-LINHA                                           
               PERFORM 1900-FS-GRAVADET                                  
               WRITE REL-LINHA
	       PERFORM 1900-FS-GRAVADET                 
            END-IF.                                     
       *                                                
            MOVE FUNCTION CURRENT-DATE(1:4) TO CAB1-ANO 
            MOVE FUNCTION CURRENT-DATE(5:2) TO CAB1-MES 
            MOVE FUNCTION CURRENT-DATE(7:2) TO CAB1-DIA.
       *                                                
            WRITE REL-LINHA         FROM  CAB-01        
            PERFORM 1900-FS-GRAVADET                    
            WRITE REL-LINHA         FROM  CAB-02        
            PERFORM 1900-FS-GRAVADET                    
            WRITE REL-LINHA         FROM  CAB-03        
            PERFORM 1900-FS-GRAVADET                    
            MOVE  SPACES            TO    REL-LINHA     
            WRITE REL-LINHA                             
            PERFORM 1900-FS-GRAVADET                    
            IF WK-FS-ARQENT         EQUAL   '10'            
            OR WK-FS-SERASA         EQUAL   '10'
	    OR WK-CPF-ENT     NOT EQUAL   WK-CPF-SER                      
               NEXT SENTENCE                                             
            ELSE                                                         
               WRITE REL-LINHA      FROM  CAB-04                         
               PERFORM 1900-FS-GRAVADET                                  
            END-IF.                                                      
            MOVE  5                 TO    WK-CONT-LINHA.                 
       *                                                                 
        1700-CABECALHO-FIM.         EXIT.                                
       *-----------------------------------------------------------------
        1900-FS-GRAVADET                   SECTION.                      
       *-----------------------------------------------------------------
            MOVE '1900-FS-GRAVACAO' TO    WK-AREA-COD                    
            IF WK-FS-RELATO   NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-RELATO                   
               MOVE  WK-FS-RELATO   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO GRAVAR NO RELATO'
		                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
        1900-FS-GRAVADET-FIM.      EXIT.                                 
       *-----------------------------------------------------------------
        2000-PROCESSA                        SECTION.                    
       *-----------------------------------------------------------------
            IF WK-CPF-ENT          EQUAL   WK-CPF-SER                     
               ADD 1 TO WK-CONT-CPF                                      
               PERFORM 2250-GRAVACAO                                     
               PERFORM 2750-GRAVADET                                     
               PERFORM 1250-LER-ARQENT                                   
               PERFORM 1500-LER-SERASA                                   
            ELSE                                                         
               IF  WK-CPF-ENT     >      WK-CPF-SER                      
                   PERFORM 1500-LER-SERASA                               
               ELSE 
		   IF WK-CPF-ENT     <      WK-CPF-SER                   
                      PERFORM 1250-LER-ARQENT                            
                   END-IF                                                
               END-IF                                                    
            END-IF.                                                      
                                                                         
        2000-PROCESSA-FIM.          EXIT.                                 
       *-----------------------------------------------------------------
        2250-GRAVACAO                      SECTION.                      
       *-----------------------------------------------------------------
            MOVE '2250-GRAVACAO'    TO    WK-AREA-COD                    
            MOVE  CPF-ENT           TO    CPF-SAI                        
            MOVE  NOME-CLIENTE-ENT  TO    NOME-CLIENTE-SAI               
            MOVE  TOTAL-DIVIDA-SER  TO    TOTAL-DIVIDA-SAI               
            WRITE ARQ-SAI                                                
                                                                         
            IF  WK-FS-ARQSAI  NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD
	       DISPLAY 'STATUS:  '        WK-FS-ARQSAI                   
               MOVE  WK-FS-ARQSAI   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO GRAVAR NO ARQ. SAIDA'                       
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
            ADD  1                  TO    WK-CONT-GRAVADO.               
                                                                         
        2250-GRAVACAO-FIM.          EXIT.                                 
       *-----------------------------------------------------------------
        2750-GRAVADET                      SECTION.                      
       *-----------------------------------------------------------------
            IF  WK-CONT-LINHA > 49                                       
               PERFORM 1750-CABECALHO                                    
            END-IF.                                                      
       *                                                                 
            MOVE CPF-ENT(1:9)       TO     DET-CPF9
            MOVE CPF-ENT(10:2)      TO     DET-CPF2                       
            MOVE NOME-CLIENTE-ENT   TO     DET-NOME                       
            MOVE TOTAL-DIVIDA-SER   TO     DET-VALOR                      
            ADD  TOTAL-DIVIDA-SER   TO     WK-TOT-VLR                     
                                                                         
            WRITE REL-LINHA         FROM   DETALHE                        
            PERFORM 1900-FS-GRAVADET                                     
            ADD    1                TO      WK-TOT-REG.                    
            ADD    1                TO      WK-CONT-LINHA.                 
                                                                         
        2750-GRAVADET-FIM.          EXIT.                                 
       *-----------------------------------------------------------------
        3000-TOTALIZADOR                    SECTION.                       
       *-----------------------------------------------------------------
            MOVE '3000-TOTALIZADOR'  TO     WK-AREA-COD                    
            IF WK-CONT-LINHA > 47                                        
               PERFORM 1750-CABECALHO                                    
            END-IF.
		                                                                       
            MOVE  WK-TOT-VLR        TO      TOT-VLRDIV                     
            MOVE  WK-TOT-REG        TO      TOT-REG                        
                                                                         
            MOVE SPACES             TO      REL-LINHA                      
            WRITE REL-LINHA                                              
            PERFORM 1900-FS-GRAVADET                                     
            WRITE REL-LINHA         FROM    TOTDIV                         
            PERFORM 1900-FS-GRAVADET                                     
            WRITE REL-LINHA         FROM    TOTREG                         
            PERFORM 1900-FS-GRAVADET.                                    
                                                                         
        3000-TOTALIZADOR-FIM.      EXIT.                                 
       *-----------------------------------------------------------------
        9000-TERMINO                        SECTION.                    
       *-----------------------------------------------------------------
            MOVE '9000-TERMINO'     TO    WK-AREA-COD                    
            WRITE REL-LINHA
	    ADD 1                   TO    WK-CONT-LINHA     
                                                            
            PERFORM 3000-TOTALIZADOR.                       
                                                            
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
            CLOSE         SERASA.                           
            IF WK-FS-SERASA   NOT EQUAL '00'                
               DISPLAY 'ERRO NO: '        WK-AREA-COD       
               DISPLAY 'STATUS:  '        WK-FS-SERASA
	       MOVE  WK-FS-SERASA   TO    WK-AREA-STAT
               MOVE 'ERRO AO FECHAR RELATORIO'        
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
                                                                         
            DISPLAY 'QTD LIDA EM ARQENT : ' WK-CONT-LER-ARQENT     '   ' 
            DISPLAY 'QTD LIDA EM SERASA : ' WK-CONT-LER-SERASA     '   ' 
            DISPLAY 'QTD DE CPF EM AMBOS: ' WK-CONT-CPF            '   ' 
            DISPLAY 'QTD NO ARQSAI      : ' WK-CONT-GRAVADO        '   ' 
            DISPLAY 'QTD NO RELATO      : ' WK-TOT-REG             '   ' 
            DISPLAY 'QTD DE PAGINAS     : ' WK-CONT-PAG            '   ' 
            DISPLAY '                                                  ' 
            DISPLAY '**************************************************' 
            DISPLAY '************PGM EXECUTADO COM SUCESSO.************' 
            DISPLAY '**************************************************'.
		                                                                       
        9000-TERMINO-FIM.          EXIT.                                 
       *-----------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                                                                                      
                           
		                                   
