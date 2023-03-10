		                                                                       
       *=================================================================
        IDENTIFICATION                     DIVISION.                     
       *=================================================================
        PROGRAM-ID.  VRB663S.                                            
        AUTHOR.      HUGO DA SILVA SAMPAIO/QINTESS.                                            
       *=================================================================
       *-----------------------------------------------------------------
       * OBJETIVO: COMPARAR ARQUIVO ENTRADA (ARQ-ENT) COM ARQUIVO VSAM   
       * (CONTAS) PARA VERIFICAR CLIENTES QUE ESTAO COM A CONTA EM ESTADO
       *            (A)TIVO   (B)LOQUEADO   OU   (I)NATIVO               
       *-----------------------------------------------------------------
       *-----------------------------------------------------------------
       *                                                                 
       * INPUT  - ARQENT                                                 
       * VSAM   - CONTAS                                                 
       * OUTPUT - ARQSAI                                                 
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
                                                                         
              SELECT CONTAS ASSIGN TO CONTAS
	      ORGANIZATION IS INDEXED                                    
              ACCESS MODE IS RANDOM                                      
              RECORD KEY IS CONTA-VSAM                                   
              FILE STATUS IS WK-FS-CONTAS.                               
                                                                         
              SELECT ARQSAI ASSIGN TO ARQSAI                             
              FILE STATUS IS WK-FS-ARQSAI.                               
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
           03 CONTA-ENT                    PIC 9(10).       
           03 FILLER                       PIC 9(02).       
                                                            
        FD CONTAS.                                          
                                                            
        01 ARQ-CONTA.                                       
           03 CONTA-VSAM                   PIC X(10).       
           03 CPF-VSAM                     PIC 9(11).       
           03 NOME-CLIENTE-VSAM            PIC X(30).       
           03 STATUS-VSAM                  PIC X(01).       
           03 FILLER                       PIC 9(08).       
                                                            
        FD ARQSAI                          RECORDING MODE F.
                                                            
        01 ARQ-SAI.
	   03 CPF-SAI                      PIC 9(11).                    
           03 NOME-CLIENTE-SAI             PIC X(30).                    
           03 VALOR-GASTO-SAI              PIC 9(15)V99.                 
           03 CONTA-SAI                    PIC 9(10).                    
           03 STATUS-SAI                   PIC X(01).                    
           03 FILLER                       PIC 9(01).                    
       *-----------------------------------------------------------------
        WORKING-STORAGE                    SECTION.                      
       *-----------------------------------------------------------------
                                                                         
        77 WK-FS-ARQENT                    PIC X(02)     VALUE SPACES.   
        77 WK-FS-CONTAS                    PIC X(02)     VALUE SPACES.   
        77 WK-FS-ARQSAI                    PIC X(02)     VALUE SPACES.   
                                                                         
       *-----------------------------------------------------------------
       ******************************************************************
       ************************ AREA - DISPLAY **************************
       ******************************************************************
       *------------------------------------------------------------
                                                                    
        77 WK-CONT-ATIVOS                  PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-INATIVOS                PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-BLOQUEADOS              PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-IGNORADOS               PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-LER-ARQENT              PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-LER-CONTAS              PIC 9(04)    VALUE ZEROS.
        77 WK-CONT-SAIDA                   PIC 9(04)    VALUE ZEROS.
                                                                    
        01 WK-DATA.                                                 
           03 WK-DATA-DD                   PIC 9(02)    VALUE ZEROS.
           03 WK-DATA-MM                   PIC 9(02)    VALUE ZEROS.
           03 WK-DATA-AAAA                 PIC 9(04)    VALUE ZEROS.
                                                                    
        01 WK-HORA.                                                 
           03 WK-HORA-HH                   PIC 9(02)    VALUE ZEROS.
           03 WK-HORA-MM                   PIC 9(02)    VALUE ZEROS.
	   03 WK-HORA-SS                   PIC 9(02)    VALUE ZEROS.     
           03 WK-HORA-MS                   PIC 9(02)    VALUE ZEROS.     
                                                                         
       *-----------------------------------------------------------------
       ******************************************************************
       ************************* AREA - ABEND  **************************
       ******************************************************************
       *-----------------------------------------------------------------
                                                                         
        77 WK-ABENDA                       PIC X(08)  VALUE 'ABENDA31'.  
        01 WK-AREA.                                                      
           05 WK-AREA-PGM                  PIC X(08)  VALUE 'VRB663S'.   
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
            DISPLAY '******************INICIANDO PGM.******************' 
            DISPLAY '**************************************************' 
       *                                                                 
            PERFORM 1000-INICIO   THRU 1000-INICIO-FIM.                  
            PERFORM 2000-PROCESSA THRU 2000-PROCESSA-FIM     UNTIL       
                    WK-FS-ARQENT EQUAL '10'.                             
	    PERFORM 9000-TERMINO  THRU 9000-TERMINO-FIM.                 
       *                                                                 
            STOP RUN.                                                    
       *                                                                 
        0000-PRINCIPAL-FIM.           EXIT.                              
       *-----------------------------------------------------------------
        1000-INICIO                          SECTION.                    
       *-----------------------------------------------------------------
            MOVE '1000-INICIO'      TO    WK-AREA-COD                    
                                                                         
            MOVE FUNCTION CURRENT-DATE(1:4) TO WK-DATA-AAAA              
            MOVE FUNCTION CURRENT-DATE(5:2) TO WK-DATA-MM                
            MOVE FUNCTION CURRENT-DATE(7:2) TO WK-DATA-DD                
                                                                         
            ACCEPT WK-HORA FROM TIME.                                    
                                                                         
            DISPLAY '                                                  ' 
            DISPLAY '*************  DIA: ' WK-DATA-DD '/' WK-DATA-MM '/'
		                   WK-DATA-AAAA ' *******************'                 
            DISPLAY '************* HORA: ' WK-HORA-HH ':' WK-HORA-MM ':' 
                     WK-HORA-SS '   *******************'                 
            DISPLAY '                                                  ' 
                                                                         
            OPEN INPUT    ARQENT.                                        
            IF WK-FS-ARQENT   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQENT ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-ARQENT              
               DISPLAY '  ********************************************* '
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ABRIR ARQENT'                               
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *  
	    OPEN INPUT    CONTAS.                                        
            IF  WK-FS-CONTAS   NOT EQUAL '00'                            
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - CONTAS ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-CONTAS              
               DISPLAY '  ********************************************* '
               MOVE WK-FS-CONTAS    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ABRIR CONTAS'                               
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
            OPEN OUTPUT   ARQSAI.                                        
            IF WK-FS-ARQSAI   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQSAI ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD
		             DISPLAY '  ** STATUS : '        WK-FS-ARQSAI              
               DISPLAY '  ********************************************* '
               MOVE WK-FS-ARQSAI    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ABRIR ARQSAI'                               
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
            PERFORM 1250-LER-ARQENT                                      
                                                                         
            IF WK-FS-ARQENT EQUAL '10'                                   
               DISPLAY '  ********************************************  '
               DISPLAY '  ************ ARQUIVO ENTRADA VAZIO *********  '
               DISPLAY '  ********************************************  '
               GO TO 1000-INICIO-FIM                                     
            END-IF.                                                      
                                                                         
        1000-INICIO-FIM.            EXIT.                                
       *-----------------------------------------------------------------
        1250-LER-ARQENT                    SECTION.                      
       *-----------------------------------------------------------------
            MOVE '1250-ARQENT'      TO    WK-AREA-COD                    
            READ          ARQENT                                         
                                                                         
            IF  WK-FS-ARQENT  NOT EQUAL '00'                             
            AND WK-FS-ARQENT  NOT EQUAL '10'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQENT ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-ARQENT              
               DISPLAY '  ********************************************* '
               MOVE WK-FS-ARQENT    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO LER ARQUIVO ENTRADA'                        
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.
       *                                                                 
            IF WK-FS-ARQENT   NOT EQUAL '10'                             
               ADD   1                 TO    WK-CONT-LER-ARQENT          
            END-IF.                                                      
       *                                                                 
        1250-LER-ARQENT-FIM.        EXIT.                                
       *-----------------------------------------------------------------
        1500-LER-CONTAS                    SECTION.                      
       *-----------------------------------------------------------------
            MOVE '1500-CONTAS'      TO    WK-AREA-COD                    
            READ CONTAS                                                  
                                                                         
            IF  WK-FS-CONTAS  NOT EQUAL '00'                             
            AND WK-FS-CONTAS  NOT EQUAL '23'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - CONTAS ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-CONTAS
	       DISPLAY '  ********************************************* '
               MOVE WK-FS-CONTAS    TO    WK-AREA-STAT                   
               MOVE 'ERRO AO LER ARQUIVO CONTAS'                         
                                    TO    WK-AREA-MSG                    
               CALL WK-ABENDA       USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
            IF WK-FS-CONTAS         EQUAL '23'                           
               ADD 1                   TO    WK-CONT-IGNORADOS           
            ELSE                                                         
               IF  WK-FS-CONTAS NOT EQUAL '23'                           
                  ADD 1                TO    WK-CONT-LER-CONTAS          
               END-IF                                                    
            END-IF.                                                      
       *                                                                 
        1500-LER-CONTAS-FIM.        EXIT.                                
       *-----------------------------------------------------------------
        2000-PROCESSA                        SECTION.   
       *-----------------------------------------------------------------
            MOVE  CONTA-ENT             TO CONTA-VSAM                    
            PERFORM 1500-LER-CONTAS                                      
                                                                         
            MOVE '2000-PROCESSA'        TO WK-AREA-COD                   
                                                                         
            IF  WK-FS-CONTAS                EQUAL '00'                   
               IF  STATUS-VSAM              EQUAL 'A'                    
                  ADD 1 TO WK-CONT-ATIVOS                                
                  PERFORM 2050-ESCREVE-SAIDA                             
               ELSE                                                      
                  IF  STATUS-VSAM           EQUAL 'I'                    
                     ADD 1 TO WK-CONT-INATIVOS                           
                  ELSE                                                   
                     IF STATUS-VSAM         EQUAL 'B'                    
                        ADD 1 TO WK-CONT-BLOQUEADOS                      
                     END-IF                                              
                  END-IF
	       END-IF                                                    
            END-IF.                                                      
                                                                         
            PERFORM 1250-LER-ARQENT.                                     
                                                                         
        2000-PROCESSA-FIM.          EXIT.                                
       *-----------------------------------------------------------------
        2050-ESCREVE-SAIDA                   SECTION.                    
       *-----------------------------------------------------------------
            MOVE CPF-ENT            TO    CPF-SAI                        
            MOVE NOME-CLIENTE-ENT   TO    NOME-CLIENTE-SAI               
            MOVE VALOR-GASTO-ENT    TO    VALOR-GASTO-SAI                
            MOVE CONTA-ENT          TO    CONTA-SAI                      
            MOVE STATUS-VSAM        TO    STATUS-SAI                     
            WRITE ARQ-SAI                                                
            PERFORM 2100-FS-WRITE                                        
            ADD 1 TO WK-CONT-SAIDA.
       *-----------------------------------------------------------------
	2050-ESCREVE-SAIDA-FIM.     EXIT.                                
       *-----------------------------------------------------------------
        2100-FS-WRITE                        SECTION.                    
       *-----------------------------------------------------------------
            MOVE '2100-FS-WRITE'    TO    WK-AREA-COD                    
                                                                         
            IF WK-FS-ARQSAI   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQSAI ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-ARQSAI              
               DISPLAY '  ********************************************* '
               MOVE  WK-FS-ARQSAI   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO ESCREVER EM ARQUIVO SAIDA'                  
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.  

	2100-FS-WRITE-FIM.          EXIT.                                
       *-----------------------------------------------------------------
        9000-TERMINO                         SECTION.                    
       *-----------------------------------------------------------------
            MOVE '9000-TERMINO'     TO    WK-AREA-COD                    
                                                                         
            CLOSE         ARQENT.                                        
            IF WK-FS-ARQENT   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQENT ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-ARQENT              
               DISPLAY '  ********************************************* '
               MOVE  WK-FS-ARQENT   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO FECHAR ARQUIVO ENTRADA'                     
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.
       *                                                	                                                                  
            CLOSE         CONTAS.                                        
            IF WK-FS-CONTAS   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - CONTAS ** '
               DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-CONTAS              
               DISPLAY '  ********************************************* '
               MOVE  WK-FS-CONTAS   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO FECHAR ARQUIVO CONTAS'                      
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
       *                                                                 
            CLOSE         ARQSAI.                                        
            IF WK-FS-ARQSAI   NOT EQUAL '00'                             
               DISPLAY '  ********************************************* '
               DISPLAY '  ** PROGRAMA VRB663S FOI CANCELADO - ARQSAI ** '
		             DISPLAY '  ** ERRO NO: '        WK-AREA-COD               
               DISPLAY '  ** STATUS : '        WK-FS-ARQSAI              
               DISPLAY '  ********************************************* '
               MOVE  WK-FS-ARQSAI   TO    WK-AREA-STAT                   
               MOVE 'ERRO AO FECHAR ARQUIVO SAIDA'                       
                                    TO    WK-AREA-MSG                    
               CALL  WK-ABENDA      USING WK-AREA                        
            END-IF.                                                      
                                                                         
            DISPLAY 'QTD LIDA EM ARQENT    : ' WK-CONT-LER-ARQENT  '   ' 
            DISPLAY 'QTD LIDA EM CONTAS    : ' WK-CONT-LER-CONTAS  '   ' 
            DISPLAY 'QTD DE CPF ATIVOS     : ' WK-CONT-ATIVOS      '   ' 
            DISPLAY 'QTD DE CPF INATIVOS   : ' WK-CONT-INATIVOS    '   ' 
            DISPLAY 'QTD DE CPF BLOQUEADOS : ' WK-CONT-BLOQUEADOS  '   ' 
            DISPLAY 'QTD DE CPF IGNORADOS  : ' WK-CONT-IGNORADOS   '   ' 
            DISPLAY 'QTD NO ARQSAI         : ' WK-CONT-SAIDA       '   ' 
            DISPLAY '                                                  ' 
            DISPLAY '**************************************************' 
	    DISPLAY '************PGM EXECUTADO COM SUCESSO.************' 
            DISPLAY '**************************************************'.
                                                                         
        9000-TERMINO-FIM.          EXIT.                                 
       *-----------------------------------------------------------------
      
                                                                                                                                                                                                                                                                                                                                                                                            
