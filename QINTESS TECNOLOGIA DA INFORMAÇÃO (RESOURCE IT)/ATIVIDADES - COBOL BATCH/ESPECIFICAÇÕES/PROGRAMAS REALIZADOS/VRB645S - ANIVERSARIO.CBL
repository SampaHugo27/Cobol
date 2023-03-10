        IDENTIFICATION                      DIVISION.                    
       *================================================================*
        PROGRAM-ID.                         EXHB.                        
        AUTHOR.                             HUGO SAMPAIO.                
        INSTALLATION.                       QINTESS.                     
       *----------------------------------------------------------------*
       * OBJETIVO DO PROGRAMA: O PROGRAMA LE OS REGISTROS DO ARQUIVO DE *
       * ENTRADA ARQENT  E IDENTIFICAR NESSE ARQUIVO A QUANTIDADE DE    *
       * CLIENTES QUE FAZEM ANIVERSARIO EM CADA DIA DO ANO.             *
       *----------------------------------------------------------------*
       *================================================================*
        ENVIRONMENT                         DIVISION.                    
       *================================================================*
        CONFIGURATION                       SECTION.                     
       *----------------------------------------------------------------*
       *SOURCE-COMPUTER.                                                 
       *OBJECT-COMPUTER.                                                 
		                                                                       
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.      
       *----------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.                     
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
            SELECT ARQENT  ASSIGN TO ARQENT                              
            ORGANIZATION   IS  SEQUENTIAL                                
            ACCESS MODE    IS  SEQUENTIAL                                
            FILE STATUS    IS  WS-FSARQENT.                              
       *----------------------------------------------------------------*
       *================================================================*
        DATA                                DIVISION.                    
       *================================================================*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD  ARQENT         RECORDING MODE F.
	01  REG-ENT.                                                     
            05  CPF            PIC 9(11).                                
            05  NOME-CLI       PIC X(30).                                
            05  DATA-NASCTO.                                             
                10   ANO       PIC 9999.                                 
                10   MES       PIC 99.                                   
                10   DIA       PIC 99.                                   
            05  VALOR-GASTO    PIC 9(15)V99.                             
            05  FILLER         PIC X(04).                                
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.                     
       *----------------------------------------------------------------*
       *              DECLARACAO DA TABELA BIDIMENSIONAL                *
       *----------------------------------------------------------------*
        01 WS-TABELA.                                                    
           03 WS-TB-MES        OCCURS 12 TIMES.                          
              05 WS-TB-DIA     OCCURS 31 TIMES.                          
                 07 WS-TAB-QTDE             PIC 99        VALUE ZEROS.
       *----------------------------------------------------------------*
       *                       AREA DE ABEND                            *
       *----------------------------------------------------------------*
        77  WS-ABENDA                       PIC X(08)   VALUE 'ABENDA31'.
        01  WS-AREA.                                                     
            05 WS-AREA-PGM                  PIC X(08)   VALUE 'VRB645S'. 
            05 WS-AREA-STAT                 PIC X(03)   VALUE SPACES.    
            05 WS-AREA-MSG                  PIC X(50)   VALUE SPACES.    
                                                                         
        01  WS-CODIGO-AREA                  PIC X(30)   VALUE SPACES.    
       *----------------------------------------------------------------*
        77  WS-CONTA-REC                    PIC 999     VALUE ZEROS.     
        77  WS-FSARQENT                     PIC X(02)   VALUE '00'.      
       *----------------------------------------------------------------*
       *                   DECLARACAO DOS INDICES                       *
       *----------------------------------------------------------------*
        77  WS-IND                          PIC 99      VALUE ZEROS.     
        77  WS-IND2                         PIC 99      VALUE ZEROS.
	77  WS-IND3                         PIC 99      VALUE ZEROS.     
       *----------------------------------------------------------------*
       *                   DECLARACAO DAS FLAGS                         *
       *----------------------------------------------------------------*
        77  WS-FLAG1                        PIC 9       VALUE ZEROS.     
        77  WS-FLAG2                        PIC 9       VALUE ZEROS.     
       *================================================================*
        PROCEDURE                           DIVISION.                    
       *================================================================*
            PERFORM 1000-INICIO    THRU     1000-99-INICIO-EXIT.         
            PERFORM 3000-PROCESSA  THRU     3000-99-PROCESSA-EXIT        
                                            UNTIL WS-FSARQENT EQUAL '10'.
            PERFORM 9000-FINALIZA  THRU     9000-99-FINALIZA-EXIT.       
            STOP RUN.                                                    
       *================================================================*
        1000-INICIO                         SECTION.                     
       *================================================================*
       * ABERTURA DO ARQUIVO DE ENTRADA E VERIFICACAO DE FILE STATUS    *                                                                                              
       *----------------------------------------------------------------*
            MOVE '1000-INICIO'         TO         WS-CODIGO-AREA.        
                                                                         
            OPEN INPUT   ARQENT.                                         
            IF  WS-FSARQENT            NOT EQUAL '00'                    
                DISPLAY 'ERRO NO: '                WS-CODIGO-AREA        
                DISPLAY 'STATUS:  '                WS-FSARQENT           
                MOVE WS-FSARQENT        TO         WS-AREA-STAT          
                MOVE 'ERRO NA ABERTURA DO ARQUIVO DE ENTRADA ARQENT'     
                                        TO         WS-AREA-MSG           
                CALL WS-ABENDA          USING      WS-AREA               
            END-IF.                                                      
                                                                         
            PERFORM 3100-LEITURA.                                        
       *----------------------------------------------------------------*
       * VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                    *
       *----------------------------------------------------------------*
            IF WS-CONTA-REC             GREATER 0
	       ADD  1                   TO      WS-CONTA-REC             
            ELSE                                                         
               DISPLAY '************************************************'
               DISPLAY '********** ARQUIVO DE ENTRADA VAZIO ************'
               DISPLAY '************************************************'
               STOP  RUN                                                 
            END-IF.                                                      
                                                                         
            DISPLAY '**************************************************'.
            DISPLAY '*  TABELA DE VOLUME DE ANIVERSARIANTES POR DIA   *'.
            DISPLAY '-------------------------------------------------*'.
       *================================================================*
        1000-99-INICIO-EXIT.                EXIT.                        
       *================================================================*
        3000-PROCESSA                       SECTION.                     
       *================================================================*
            ADD 1 TO WS-TAB-QTDE(MES, DIA).                              
            PERFORM 3100-LEITURA.
                                                                       
       *================================================================*
        3000-99-PROCESSA-EXIT.              EXIT.                        
       *================================================================*
        3100-LEITURA                        SECTION.                     
       *================================================================*
            MOVE '3100-LEITURA'             TO    WS-CODIGO-AREA.        
                                                                         
            READ ARQENT.                                                 
                                                                         
            IF  WS-FSARQENT  NOT EQUAL '00'                              
            AND WS-FSARQENT  NOT EQUAL '10'                              
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FSARQENT            
               MOVE WS-FSARQENT             TO    WS-AREA-STAT           
               MOVE 'ERRO AO LER ARQUIVO ENTRADA'                        
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA
		          ELSE                                                         
               IF   WS-FSARQENT = '10'                                   
                    GO TO  3100-99-LEITURA-EXIT                          
               END-IF                                                    
            END-IF.                                                      
                                                                         
            ADD   1                         TO    WS-CONTA-REC.          
       *================================================================*
        3100-99-LEITURA-EXIT.               EXIT.                        
       *================================================================* 
        9000-FINALIZA                       SECTION.                     
       *================================================================*
       * FECHAMENTO DO ARQUIVO DE ENTRADA E VERIFICACAO DE FILE STATUS  *
       *----------------------------------------------------------------*
            MOVE '9000-FINALIZA'            TO    WS-CODIGO-AREA.        
                                                                         
            PERFORM 9100-DESCARREGA-TAB1.
	    DISPLAY '**************************************************'.
                                                                         
            CLOSE   ARQENT.                                              
            IF WS-FSARQENT                  NOT   EQUAL '00'             
               DISPLAY 'ERRO NO: '                WS-CODIGO-AREA         
               DISPLAY 'STATUS:  '                WS-FSARQENT            
               MOVE WS-FSARQENT             TO    WS-AREA-STAT           
               MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE ENTRADA ARQENT'    
                                            TO    WS-AREA-MSG            
               CALL WS-ABENDA               USING WS-AREA                
            END-IF.                                                      
                                                                         
       *================================================================*
        9000-99-FINALIZA-EXIT.              EXIT.                        
       *================================================================*
        9100-DESCARREGA-TAB1                SECTION.                     
       *================================================================*
            PERFORM VARYING WS-IND          FROM 1 BY 1 
                    UNTIL   WS-IND          GREATER 12                   
                                                                         
                    MOVE    ZEROS           TO  WS-FLAG1                 
                                                                         
                    PERFORM 9200-DESCARREGA-TAB2                         
                                                                         
            END-PERFORM.                                                 
       *================================================================*
        9100-99-DESCARREGA-TAB1-EXIT.       EXIT.                        
       *================================================================*
        9200-DESCARREGA-TAB2                SECTION.                     
       *================================================================*
            PERFORM VARYING WS-IND2         FROM  1 BY 1                 
                    UNTIL   WS-IND2         GREATER 31                   
                                                                         
                    PERFORM VARYING WS-IND3  FROM 1 BY 1                 
		                          UNTIL   WS-IND3  GREATER 31                  
                            OR      WS-FLAG1 EQUAL 1                     
                                                                         
                            IF WS-TAB-QTDE(WS-IND, WS-IND3) GREATER ZEROS
                               MOVE 1        TO WS-FLAG1                 
                            END-IF                                       
                                                                         
                    END-PERFORM                                          
                                                                         
                    IF WS-IND2 EQUAL 1 AND WS-FLAG1 EQUAL 1              
                       DISPLAY '*                                       '
                       '         *'                                      
                       EVALUATE WS-IND                                   
                        WHEN 1  DISPLAY '* JANEIRO                      '
                        '                  *'                            
                        WHEN 2  DISPLAY '* FEVEREIRO                    '
                        '                  *'                            
                        WHEN 3  DISPLAY '* MARCO                        '
		        '                  *'                            
                        WHEN 4  DISPLAY '* ABRIL                        '
                        '                  *'                            
                        WHEN 5  DISPLAY '* MAIO                         '
                        '                  *'                            
                        WHEN 6  DISPLAY '* JUNHO                        '
                        '                  *'                            
                        WHEN 7  DISPLAY '* JULHO                        '
                        '                  *'                            
                        WHEN 8  DISPLAY '* AGOSTO                       '
                        '                  *'                            
                        WHEN 9  DISPLAY '* SETEMBRO                     '
                        '                  *'                            
                        WHEN 10 DISPLAY '* OUTUBRO                      '
                        '                  *'                            
                        WHEN 11 DISPLAY '* NOVEMBRO                     '
                        '                  *'                            
                        WHEN 12 DISPLAY '* DEZEMBRO                     ' 
		        '                  *'                            
                       END-EVALUATE                                      
                                                                         
                       DISPLAY '* DIA  QTDE ANIVERSARIANTES            ' 
                       '          *'                                     
                    END-IF                                               
                                                                         
                                                                         
                    IF WS-TAB-QTDE(WS-IND, WS-IND2) GREATER ZEROS        
                       AND WS-FLAG1 EQUAL 1                              
                                                                         
                       DISPLAY '*  ' WS-IND2 '  '                        
                       WS-TAB-QTDE(WS-IND, WS-IND2)                      
                       '                                        *'       
                    END-IF                                               
            END-PERFORM.                                                 
                                                                         
       *================================================================* 
        9200-99-DESCARREGA-TAB2-EXIT.       EXIT.                        
       *================================================================*                      
      
      
