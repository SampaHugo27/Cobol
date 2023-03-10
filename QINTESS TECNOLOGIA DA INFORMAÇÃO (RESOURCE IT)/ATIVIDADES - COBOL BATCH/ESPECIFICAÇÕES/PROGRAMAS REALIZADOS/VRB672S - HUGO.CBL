      *=================================================================
       IDENTIFICATION                     DIVISION.                     
      *=================================================================
       PROGRAM-ID.   VRB672S.                                           
       AUTHOR.       HUGO DA SILVA SAMPAIO.                                         
       INSTALLATION. QINTESS.                                           
      *=================================================================
      *-----------------------------------------------------------------
      * OBJETIVO: COMPARAR ARQUIVO ENTRADA (ARQSTS) COM ARQUIVO VSAM    
      * (CONTAS) PARA VERIFICAR CLIENTES QUE ESTAO COM A CONTA EM ESTADO
      * (A)TIVO (B)LOQUEADO OU (I)NATIVO. CASO A CONTA NAO EXISTA,      
      * ESCREVER ELA NO ARQ CONTAS. SE EXISTIR ATUALIZAR ARQ CONTAS.    
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *                                                                 
      * INPUT  - ARQSTS                                                 
      * VSAM   - CONTAS                                                 
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
                                                                        
             SELECT   ARQSTS      ASSIGN  TO  ARQSTS                    
                      FILE        STATUS  IS  WK-FS-ARQSTS.             
                                                                        
             SELECT   CONTAS      ASSIGN  TO  CONTAS                    
                      ORGANIZATION        IS  INDEXED                   
                      ACCESS      MODE    IS  RANDOM                    
                      RECORD      KEY     IS  VSAM-CONTA                
                      FILE        STATUS  IS  WK-FS-CONTAS.             
      *                                                                 
      *-----------------------------------------------------------------
      *=================================================================
       DATA                               DIVISION.                     
      *=================================================================
      *-----------------------------------------------------------------
       FILE                               SECTION.                      
      *-----------------------------------------------------------------
       FD ARQSTS                          RECORDING MODE F.             
                                                                        
       01 ARQ-STS.                                                      
          03 STS-CONTA                    PIC 9(10).                    
          03 STS-CPF                      PIC 9(11).                    
          03 STS-NOMECLI                  PIC X(30).                    
          03 STS-STATUS                   PIC X(01).                    
          03 FILLER                       PIC 9(08).                    
                                                                        
       FD CONTAS.                                                       
                                                                        
       01 ARQ-CONTA.                                                    
          03 VSAM-CONTA                   PIC X(10).                    
          03 VSAM-CPF                     PIC 9(11).                    
          03 VSAM-NOMECLI                 PIC X(30).                    
          03 VSAM-STATUS                  PIC X(01).                    
          03 FILLER                       PIC 9(08).                    
                                                                        
      *-----------------------------------------------------------------
       WORKING-STORAGE                      SECTION.                    
      *-----------------------------------------------------------------
                                                                        
       77 WK-FS-ARQSTS                    PIC X(02)  VALUE SPACES.      
       77 WK-FS-CONTAS                    PIC X(02)  VALUE SPACES.      
                                                                        
      *-----------------------------------------------------------------
      ******************************************************************
      ************************ AREA - DISPLAY **************************
      ******************************************************************
      *-----------------------------------------------------------------
                                                                        
       77 WK-CONT-LER-ARQSTS              PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-LER-CONTAS              PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-EXISTE                  PIC 9(04)  VALUE ZEROS. 
                                                                  
       77 WK-CONT-MODS                    PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-MUDAATIVOS              PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-MUDAINATIVOS            PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-MUDABLOCKS              PIC 9(04)  VALUE ZEROS. 
                                                                  
       77 WK-CONT-NOVO                    PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-NOVOATIVOS              PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-NOVOINATIVOS            PIC 9(04)  VALUE ZEROS. 
       77 WK-CONT-NOVOBLOCKS              PIC 9(04)  VALUE ZEROS. 
                                                                  
       01 WK-DATA.                                                
          03 WK-DATA-DD                   PIC 9(02)  VALUE ZEROS. 
          03 WK-DATA-MM                   PIC 9(02)  VALUE ZEROS. 
          03 WK-DATA-AAAA                 PIC 9(04)  VALUE ZEROS. 
                                                                  
       01 WK-HORA.                                                
          03 WK-HORA-HH                   PIC 9(02)  VALUE ZEROS.       
          03 WK-HORA-MM                   PIC 9(02)  VALUE ZEROS.       
          03 WK-HORA-SS                   PIC 9(02)  VALUE ZEROS.       
          03 WK-HORA-MS                   PIC 9(02)  VALUE ZEROS.       
                                                                        
      *-----------------------------------------------------------------
      ******************************************************************
      ************************* AREA - ABEND  **************************
      ******************************************************************
      *-----------------------------------------------------------------
                                                                        
       77 WK-ABENDA                       PIC X(08)  VALUE 'ABENDA31'.  
       01 WK-AREA.                                                      
          05 WK-AREA-PGM                  PIC X(08)  VALUE 'VRB672S'.   
          05 WK-AREA-STAT                 PIC X(03)  VALUE SPACES.      
          05 WK-AREA-MSG                  PIC X(50)  VALUE SPACES.      
                                                                        
       01 WK-AREA-COD                     PIC X(30)  VALUE SPACES.      
                                                                        
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
           DISPLAY '**************INICIANDO PGM - VRB672S.************' 
           DISPLAY '**************************************************' 
      *                                                                 
           PERFORM 1000-INICIO      THRU  1000-INICIO-FIM.              
           PERFORM 2000-PROCESSA    THRU  2000-PROCESSA-FIM             
             UNTIL WK-FS-ARQSTS     EQUAL '10'.                         
           PERFORM 9000-TERMINO     THRU  9000-TERMINO-FIM.             
      *                                                                 
           STOP RUN.                                                    
      *                                                                 
       0000-PRINCIPAL-FIM.          EXIT.                               
      *-----------------------------------------------------------------
       1000-INICIO                          SECTION.                    
      *-----------------------------------------------------------------
           MOVE '1000-INICIO'               TO  WK-AREA-COD             
                                                                        
           MOVE FUNCTION CURRENT-DATE(1:4)  TO  WK-DATA-AAAA            
           MOVE FUNCTION CURRENT-DATE(5:2)  TO  WK-DATA-MM              
           MOVE FUNCTION CURRENT-DATE(7:2)  TO  WK-DATA-DD              
                                                                        
           ACCEPT   WK-HORA         FROM    TIME.                       
                                                                        
           DISPLAY '  **********************************************  ' 
           DISPLAY '  ***********  DIA: ' WK-DATA-DD '/' WK-DATA-MM '/' 
                    WK-DATA-AAAA ' *****************  '                 
           DISPLAY '  *********** HORA: ' WK-HORA-HH ':' WK-HORA-MM ':' 
                    WK-HORA-SS '   *****************  '                 
           DISPLAY '  **********************************************  ' 
                                                                        
           OPEN INPUT ARQSTS.                                           
           IF   WK-FS-ARQSTS    NOT EQUAL '00'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - ARQSTS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-ARQSTS                  
              DISPLAY '  ********************************************* '
              MOVE WK-FS-ARQSTS     TO    WK-AREA-STAT                  
              MOVE 'ERRO AO ABRIR ARQSTS'                               
                                    TO    WK-AREA-MSG                   
              CALL WK-ABENDA        USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           OPEN I-O   CONTAS.                                           
           IF   WK-FS-CONTAS    NOT EQUAL '00'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - CONTAS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-CONTAS                  
              DISPLAY '  ********************************************* '
              MOVE WK-FS-CONTAS     TO    WK-AREA-STAT                  
              MOVE 'ERRO AO ABRIR CONTAS'                               
                                    TO    WK-AREA-MSG                   
              CALL WK-ABENDA        USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           PERFORM 1250-LER-ARQSTS  THRU  1250-LER-ARQSTS-FIM           
                                                                        
           IF   WK-FS-ARQSTS        EQUAL '10'                          
              DISPLAY '  ********************************************  '
              DISPLAY '  ************ ARQUIVO ENTRADA VAZIO *********  '
              DISPLAY '  ********************************************  '
              GO TO 1000-INICIO-FIM                                     
           END-IF.                                                      
                                                                        
       1000-INICIO-FIM.             EXIT.                               
      *-----------------------------------------------------------------
       1250-LER-ARQSTS                      SECTION.                    
      *-----------------------------------------------------------------
           MOVE '1250-ARQSTS'       TO    WK-AREA-COD                   
           READ      ARQSTS                                             
                                                                        
           IF  WK-FS-ARQSTS     NOT EQUAL '00'                          
           AND WK-FS-ARQSTS     NOT EQUAL '10'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - ARQSTS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-ARQSTS                  
              DISPLAY '  ********************************************* '
              MOVE WK-FS-ARQSTS     TO    WK-AREA-STAT                  
              MOVE 'ERRO AO LER ARQUIVO ENTRADA'                        
                                    TO    WK-AREA-MSG                   
              CALL WK-ABENDA        USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           IF  WK-FS-ARQSTS     NOT EQUAL '10'                          
              ADD   1               TO    WK-CONT-LER-ARQSTS            
           END-IF.                                                      
      *                                                                 
       1250-LER-ARQSTS-FIM.         EXIT.                               
      *-----------------------------------------------------------------
       1500-LER-CONTAS                    SECTION.                      
      *-----------------------------------------------------------------
           MOVE  '1500-CONTAS'      TO    WK-AREA-COD                   
           READ      CONTAS                                             
                                                                        
           IF  WK-FS-CONTAS     NOT EQUAL '00'                          
           AND WK-FS-CONTAS     NOT EQUAL '23'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - CONTAS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-CONTAS                  
              DISPLAY '  ********************************************* '
              MOVE WK-FS-CONTAS     TO    WK-AREA-STAT                  
              MOVE 'ERRO AO LER ARQUIVO CONTAS'                         
                                    TO    WK-AREA-MSG                   
              CALL WK-ABENDA        USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           IF    WK-FS-CONTAS       EQUAL '23'                          
                 ADD 1              TO    WK-CONT-NOVO                  
           ELSE                                                         
              IF WK-FS-CONTAS   NOT EQUAL '23'                          
                 ADD 1              TO    WK-CONT-EXISTE                
              END-IF                                                    
           END-IF.                                                      
           ADD   1                  TO    WK-CONT-LER-CONTAS.           
      *                                                                 
       1500-LER-CONTAS-FIM.         EXIT.                               
      *-----------------------------------------------------------------
       2000-PROCESSA                      SECTION.                      
      *-----------------------------------------------------------------
           MOVE  STS-CONTA                  TO    VSAM-CONTA            
           PERFORM 1500-LER-CONTAS          THRU  1500-LER-CONTAS-FIM   
                                                                        
           MOVE  '2000-PROCESSA'            TO    WK-AREA-COD           
                                                                        
           IF    WK-FS-CONTAS               EQUAL '23'                  
                 PERFORM 2125-ESCREVE-VSAM  THRU  2125-ESCREVE-VSAM-FIM 
                 PERFORM 2175-STATUS-ADD    THRU  2175-STATUS-ADD-FIM   
           ELSE                                                         
              IF WK-FS-CONTAS               EQUAL '00'                  
                 PERFORM 2075-CONTA-STATUS  THRU  2075-CONTA-STATUS-FIM 
              IF STS-STATUS                 EQUAL 'A'                   
                 PERFORM 2050-MUDA-SAIDA    THRU  2050-MUDA-SAIDA-FIM   
              ELSE                                                      
                 PERFORM 2025-NAOATIVOS     THRU  2025-NAOATIVOS-FIM    
              END-IF                                                    
           END-IF.                                                      
                                                                        
           PERFORM 1250-LER-ARQSTS          THRU  1250-LER-ARQSTS-FIM.  
                                                                        
       2000-PROCESSA-FIM.           EXIT.                               
      *-----------------------------------------------------------------
       2025-NAOATIVOS                     SECTION.                      
      *-----------------------------------------------------------------
           IF    STS-STATUS               EQUAL 'I'                     
                 PERFORM 2050-MUDA-SAIDA  THRU  2050-MUDA-SAIDA-FIM     
           ELSE                                                         
              IF STS-STATUS               EQUAL 'B'                     
                 PERFORM 2050-MUDA-SAIDA  THRU  2050-MUDA-SAIDA-FIM     
              END-IF                                                    
           END-IF.                                                      
      *                                                                 
       2025-NAOATIVOS-FIM.          EXIT.                               
      *-----------------------------------------------------------------
       2050-MUDA-SAIDA                    SECTION.                      
      *-----------------------------------------------------------------
                                                                        
           REWRITE     ARQ-CONTA    FROM  ARQ-STS                       
           PERFORM 2100-FS-WRITE    THRU  2100-FS-WRITE-FIM             
           ADD                 1    TO    WK-CONT-MODS.                 
      *                                                                 
       2050-MUDA-SAIDA-FIM.         EXIT.                               
      *-----------------------------------------------------------------
       2075-CONTA-STATUS                  SECTION.                      
      *-----------------------------------------------------------------
           EVALUATE VSAM-STATUS                                         
              WHEN 'A'                                                  
                   ADD   1          TO    WK-CONT-MUDAATIVOS            
              WHEN 'I'                                                  
                   ADD   1          TO    WK-CONT-MUDAINATIVOS          
              WHEN 'B'                                                  
                   ADD   1          TO    WK-CONT-MUDABLOCKS            
           END-EVALUATE.                                                
                                                                        
       2075-CONTA-STATUS-FIM.       EXIT.                               
      *-----------------------------------------------------------------
       2100-FS-WRITE                      SECTION.                      
      *-----------------------------------------------------------------
           MOVE '2100-FS-WRITE'     TO    WK-AREA-COD                   
                                                                        
           IF WK-FS-CONTAS      NOT EQUAL '00'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - CONTAS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-CONTAS                  
              DISPLAY '  ********************************************* '
              MOVE  WK-FS-CONTAS    TO    WK-AREA-STAT                  
              MOVE 'ERRO AO REESCREVER/ESCREVER O ARQUIVO CONTAS'       
                                    TO    WK-AREA-MSG                   
              CALL  WK-ABENDA       USING WK-AREA                       
           END-IF.                                                      
                                                                        
       2100-FS-WRITE-FIM.           EXIT.                               
      *-----------------------------------------------------------------
       2125-ESCREVE-VSAM                  SECTION.                      
      *-----------------------------------------------------------------
           MOVE '2125-ESCREVE-VSAM' TO    WK-AREA-COD                   
                                                                        
           WRITE   ARQ-CONTA        FROM  ARQ-STS                       
           PERFORM 2100-FS-WRITE.                                       
                                                                        
       2125-ESCREVE-VSAM-FIM.       EXIT.                               
      *-----------------------------------------------------------------
       2175-STATUS-ADD                    SECTION.                      
      *-----------------------------------------------------------------
           EVALUATE VSAM-STATUS                                         
              WHEN 'A'                                                  
                   ADD   1          TO    WK-CONT-NOVOATIVOS            
              WHEN 'I'                                                  
                   ADD   1          TO    WK-CONT-NOVOINATIVOS          
              WHEN 'B'                                                  
                   ADD   1          TO    WK-CONT-NOVOBLOCKS            
           END-EVALUATE.                                                
                                                                        
       2175-STATUS-ADD-FIM.         EXIT.                               
      *-----------------------------------------------------------------
       9000-TERMINO                       SECTION.                      
      *-----------------------------------------------------------------
           MOVE '9000-TERMINO'      TO    WK-AREA-COD                   
                                                                        
           CLOSE    ARQSTS.                                             
           IF WK-FS-ARQSTS      NOT EQUAL '00'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - ARQSTS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-ARQSTS                  
              DISPLAY '  ********************************************* '
              MOVE  WK-FS-ARQSTS    TO    WK-AREA-STAT                  
              MOVE 'ERRO AO FECHAR ARQUIVO ENTRADA'                     
                                    TO    WK-AREA-MSG                   
              CALL  WK-ABENDA       USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           CLOSE    CONTAS.                                             
           IF WK-FS-CONTAS      NOT EQUAL '00'                          
              DISPLAY '  ********************************************* '
              DISPLAY '  ** PROGRAMA VRB672S FOI CANCELADO - CONTAS ** '
              DISPLAY '  ** ERRO NO: '    WK-AREA-COD                   
              DISPLAY '  ** STATUS : '    WK-FS-CONTAS                  
              DISPLAY '  ********************************************* '
              MOVE  WK-FS-CONTAS    TO    WK-AREA-STAT                  
              MOVE 'ERRO AO FECHAR ARQUIVO CONTAS'                      
                                    TO    WK-AREA-MSG                   
              CALL  WK-ABENDA       USING WK-AREA                       
           END-IF.                                                      
      *                                                                 
           DISPLAY '  *                                           *   ' 
           DISPLAY '  *********************************************   ' 
           DISPLAY '  *QTD LIDA EM ARQSTS    :' WK-CONT-LER-ARQSTS  ' ' 
           DISPLAY '  *QTD LIDA EM CONTAS    :' WK-CONT-LER-CONTAS  ' ' 
           DISPLAY '  *QTD DE BATIMENTOS     :' WK-CONT-EXISTE      ' ' 
           DISPLAY '  *********************************************   ' 
           DISPLAY '  *QTD DE CPF MODIFICADOS:' WK-CONT-MODS        ' ' 
           DISPLAY '  *MUDANCAS P/ ATIVOS    :' WK-CONT-MUDAATIVOS  ' ' 
           DISPLAY '  *MUDANCAS P/ INATIVOS  :' WK-CONT-MUDAINATIVOS' ' 
           DISPLAY '  *MUDANCAS P/ BLOQUEADOS:' WK-CONT-MUDABLOCKS  ' ' 
           DISPLAY '  *********************************************   ' 
           DISPLAY '  *QTD DE CPF NOVOS      :' WK-CONT-NOVO        ' ' 
           DISPLAY '  *NOVOS VSAM, ATIVOS    :' WK-CONT-NOVOATIVOS  ' ' 
           DISPLAY '  *NOVOS VSAM, INATIVOS  :' WK-CONT-NOVOINATIVOS' ' 
           DISPLAY '  *NOVOS VSAM, BLOQUEADOS:' WK-CONT-NOVOBLOCKS  ' ' 
           DISPLAY '  *********************************************   ' 
           DISPLAY '**************************************************' 
           DISPLAY '*******PGM - VRB672S EXECUTADO COM SUCESSO.*******' 
           DISPLAY '**************************************************'.
                                                                        
       9000-TERMINO-FIM.            EXIT.                               
      *-----------------------------------------------------------------
      
      
      
      
