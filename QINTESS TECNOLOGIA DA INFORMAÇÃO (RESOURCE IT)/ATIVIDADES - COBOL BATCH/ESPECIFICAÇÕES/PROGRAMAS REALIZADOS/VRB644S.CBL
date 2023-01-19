       *=================================================================
        IDENTIFICATION                     DIVISION.                     
       *=================================================================
        PROGRAM-ID. VRB644S.                                             
        AUTHOR.     HUGO SAMPAIO/QINTESS.                                             
       *=================================================================
       *                                                                 
       * INPUT  - ARQENT                                                 
       * OUTPUT - SYSOUT                                                 
       *                                                                 
       *=================================================================
        ENVIRONMENT                        DIVISION.                     
       *=================================================================
       *-----------------------------------------------------------------
        CONFIGURATION                      SECTION.                      
       *-----------------------------------------------------------------
       *SOURCE-COMPUTER                                                  
       *OBJECT-COMPUTER.
	SPECIAL-NAMES.                     DECIMAL-POINT IS COMMA.       
       *-----------------------------------------------------------------
        INPUT-OUTPUT                       SECTION.                      
       *-----------------------------------------------------------------
        FILE-CONTROL.                                                    
       *                                                                 
              SELECT ARQENT ASSIGN TO ARQENT                             
              FILE STATUS IS WK-FS-ARQENT.                               
       *                                                                 
       *=================================================================
        DATA                               DIVISION.                     
       *=================================================================
       *-----------------------------------------------------------------
        FILE                               SECTION.                      
       *-----------------------------------------------------------------
        FD ARQENT                          RECORDING MODE F.             
       *                                                                 
        01 ENTRADA.
	   03 CPF                          PIC 9(11).                    
           03 NOME-CLIENTE                 PIC X(30).                    
           03 DATA-NASCTO.                                               
              05 ANO                       PIC 9(04).                    
              05 MES                       PIC 9(02).                    
              05 DIA                       PIC 9(02).                    
           03 VALOR-GASTO                  PIC 9(15)V99.                 
           03 FILLER                       PIC 9(04).                    
       *                                                                 
       *-----------------------------------------------------------------
        WORKING-STORAGE                    SECTION.                      
       *-----------------------------------------------------------------
        77 WK-FS-ARQENT                    PIC X(02)  VALUE SPACES.      
        77 WK-IND                          PIC 9(02)  VALUE ZEROES.      
        77 WK-IND2                         PIC 9(02)  VALUE ZEROES.      
        77 WK-IND3                         PIC 9(02)  VALUE ZEROES.      
        77 WK-FLAG                         PIC X(01)  VALUE 'N'.         
       *
       ******************************************************************
       ************************* AREA - ABEND  **************************
       ******************************************************************
       *-----------------------------------------------------------------
        77 WK-ABENDA                       PIC X(08)  VALUE 'ABENDA31'.  
        01 WK-AREA.                                                      
           05 WK-AREA-PGM                  PIC X(08)  VALUE 'VRB644S'.   
           05 WK-AREA-STAT                 PIC X(03)  VALUE SPACES.      
           05 WK-AREA-MSG                  PIC X(50)  VALUE SPACES.      
                                                                         
        01 WK-AREA-COD                     PIC X(30)  VALUE SPACES.      
       *-----------------------------------------------------------------
       ******************************************************************
       ************************* AREA - OCCURS **************************
       ******************************************************************
       *-----------------------------------------------------------------
        01 WK-TAB-MESDIA.                                                
           03 WK-TAB-MES                   OCCURS 12 TIMES.
	      05 WK-TAB-DIA                OCCURS 31 TIMES.              
                 07 WK-TAB-QTD             PIC 9(08).                    
       *-----------------------------------------------------------------
       *=================================================================
        PROCEDURE                          DIVISION.                     
       *=================================================================
       *-----------------------------------------------------------------
        0000-PRINCIPAL                     SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            DISPLAY '**************************************************'.
            DISPLAY '******************INICIANDO PGM.******************'.
            DISPLAY '**************************************************'.
            DISPLAY '                                                  '.
                                                                         
            PERFORM 1000-INICIO.                                         
            PERFORM 2000-PROCESSA UNTIL WK-FS-ARQENT EQUAL '10'.         
            PERFORM 9000-TERMINO. 
	                                                                        
            STOP RUN.                                                    
       *                                                                 
       *-----------------------------------------------------------------
        0000-PRINCIPAL-FIM.            EXIT.                             
       *=================================================================
        1000-INICIO                        SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            MOVE '1000-INICIO'      TO    WK-AREA-COD                    
                                                                         
            OPEN INPUT    ARQENT.                                        
            IF WK-FS-ARQENT   NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-ARQENT                   
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ABRIR ARQUIVO ENTRADA'                      
                                    TO    WK-AREA-MSG                    
	       CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
                                                                         
            PERFORM 1500-LEITURA.                                        
                                                                         
            IF WK-FS-ARQENT EQUAL '10'                                   
               DISPLAY 'ARQUIVO DE ENTRADA VAZIO.'                       
               GO TO 1000-INICIO-FIM                                     
            END-IF.                                                      
       *                                                                 
       *-----------------------------------------------------------------
        1000-INICIO-FIM.               EXIT.                             
       *=================================================================
        1500-LEITURA                       SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            MOVE '1500-LEITURA'     TO    WK-AREA-COD                    
            READ ARQENT.
		                                                                       
            IF  WK-FS-ARQENT  NOT EQUAL '00'                             
            AND WK-FS-ARQENT  NOT EQUAL '10'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-ARQENT                   
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO LER ARQUIVO ENTRADA'                        
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
       *-----------------------------------------------------------------
        1500-LEITURA-FIM.              EXIT.                             
       *=================================================================
        2000-PROCESSA                      SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            ADD 1 TO WK-TAB-QTD(MES,DIA)
	    PERFORM 1500-LEITURA.                                        
       *                                                                 
       *-----------------------------------------------------------------
        2000-PROCESSA-FIM.             EXIT.                             
       *=================================================================
        3000-DESCARREGA-TAB                SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            DISPLAY ' ***********************************************  '.
            DISPLAY ' * TABELA DE VOLUME DE ANIVERSARIANTES POR DIA *  '.
            DISPLAY ' *---------------------------------------------*  '.
                                                                         
                                                                         
            PERFORM VARYING WK-IND         FROM 1 BY 1                   
            UNTIL    WK-IND                GREATER 12                    
                                                                         
               MOVE 'N'                    TO WK-FLAG
		                                                            
               PERFORM VARYING WK-IND2     FROM 1 BY 1        
               UNTIL WK-IND2               GREATER 31         
                                                              
                  PERFORM VARYING WK-IND3  FROM 1 BY 1        
                  UNTIL WK-IND3            GREATER 31         
                  OR    WK-FLAG            EQUAL 'Y'          
                                                              
                  IF WK-TAB-QTD(WK-IND,WK-IND3) GREATER THAN 0
                     MOVE 'Y' TO WK-FLAG                      
                  END-IF                                      
                                                              
                  END-PERFORM                                 
                                                              
               IF WK-IND2 EQUAL 1 AND WK-FLAG EQUAL 'Y'       
                  EVALUATE WK-IND                             
                  WHEN 1                                                
                     DISPLAY ' * JANEIRO                               '
                     '      *'                                          
                  WHEN 2                                                
                     DISPLAY ' * FEVEREIRO                             '
                     '      *'                                          
                  WHEN 3                                                
                     DISPLAY ' * MARCO                                 '
                     '      *'                                          
                  WHEN 4                                                
                     DISPLAY ' * ABRIL                                 '
                     '      *'                                          
                  WHEN 5                                                
                     DISPLAY ' * MAIO                                  '
                     '      *'                                          
                  WHEN 6                                                
                     DISPLAY ' * JUNHO                                 '
                     '      *'
		                WHEN 7                                                
                     DISPLAY ' * JULHO                                 '
                     '      *'                                          
                  WHEN 8                                                
                     DISPLAY ' * AGOSTO                                '
                     '      *'                                          
                  WHEN 9                                                
                     DISPLAY ' * SETEMBRO                              '
                     '      *'                                          
                  WHEN 10                                               
                     DISPLAY ' * OUTUBRO                               '
                     '      *'                                          
                  WHEN 11                                               
                     DISPLAY ' * NOVEMBRO                              '
                     '      *'                                          
                  WHEN 12                                               
                     DISPLAY ' * DEZEMBRO                              '
                     '      *'                                          
		  END-EVALUATE                                          
                  DISPLAY ' * DIA QTDE ANIVERSARIANTES                 '
                  '   *'                                                
               END-IF                                                   
                                                                        
               IF  WK-FLAG                      EQUAL 'Y'               
               AND WK-TAB-QTD(WK-IND,WK-IND2)   GREATER 0               
                  DISPLAY ' * ' WK-IND2 '  ' WK-TAB-QTD(WK-IND,WK-IND2) 
                  '                                * '                  
               END-IF                                                   
                                                                        
               IF WK-FLAG EQUAL 'Y' AND WK-IND2 EQUAL 31                
                  DISPLAY ' *                                          '
                  '   *'                                                
               END-IF                                                   
                                                                        
               END-PERFORM                                              

            END-PERFORM.                                                 
            DISPLAY ' *********************************************** '. 
       *                                                                 
       *-----------------------------------------------------------------
        3000-DESCARREGA-TAB-FIM.       EXIT.                             
       *=================================================================
        9000-TERMINO                       SECTION.                      
       *-----------------------------------------------------------------
       *                                                                 
            MOVE '9000-TERMINO'     TO    WK-AREA-COD                    
                                                                         
            PERFORM 3000-DESCARREGA-TAB                                  
                                                                         
            CLOSE         ARQENT.                                        
            IF WK-FS-ARQENT   NOT EQUAL '00'                             
               DISPLAY 'ERRO NO: '        WK-AREA-COD                    
               DISPLAY 'STATUS:  '        WK-FS-ARQENT                   
               MOVE  WK-FS-ARQENT   TO    WK-AREA-STAT 
	       MOVE 'ERRO AO FECHAR ARQUIVO ENTRADA'                     
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
            DISPLAY '                                                  '.
            DISPLAY '**************************************************'.
            DISPLAY '************PGM EXECUTADO COM SUCESSO.************'.
            DISPLAY '**************************************************'.
       *                                                                 
       *-----------------------------------------------------------------
        9000-TERMINO-FIM.              EXIT.                             
       *-----------------------------------------------------------------           
      
      
