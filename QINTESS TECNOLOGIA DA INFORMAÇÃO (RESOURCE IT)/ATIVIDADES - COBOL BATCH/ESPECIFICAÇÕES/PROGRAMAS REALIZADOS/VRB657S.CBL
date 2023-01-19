	IDENTIFICATION                      DIVISION.                    
       *===============================================================* 
        PROGRAM-ID.                         TESTE.                       
        AUTHOR.                             HUGO SAMPAIO.                
        INSTALLATION.                       QINTESS.                     
                                                                         
       *================================================================*
       * OBJETIVO: ESTE PROGRAMA UTILIZA DADOS DE UM ARQUIVO SEQUENCIAL *
       * E CRIA UM ARQUIVO INDEXADO.                                    *
       *================================================================*
        ENVIRONMENT                         DIVISION.                    
       *================================================================*
        CONFIGURATION                       SECTION.                     
       *================================================================*
        SOURCE-COMPUTER.                    IBM-PC.                      
        OBJECT-COMPUTER.                    IBM-PC.                      
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.      
       *----------------------------------------------------------------*
	INPUT-OUTPUT                        SECTION.                     
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
            SELECT CADENT  ASSIGN TO CADENT                              
            ORGANIZATION IS SEQUENTIAL.                                  
                                                                         
            SELECT CADIN   ASSIGN TO CADIN                               
            ORGANIZATION IS INDEXED                                      
            ACCESS MODE  IS SEQUENTIAL                                   
            RECORD KEY   IS NUM-IN                                       
            FILE STATUS     FS.                                          
                                                                         
       *================================================================*
        DATA                                DIVISION.                    
       *================================================================*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD  CADENT                          
	    LABEL RECORD IS STANDARD.                                    
                                                                         
        01  REG-ENT.                                                     
            05  COD-ENT                     PIC X(03).                   
            05  NOME-PROD                   PIC X(30).                   
                                                                         
        FD  CADIN                                                        
            LABEL RECORD IS STANDARD.                                    
                                                                         
        01  REG-IN.                                                      
            05  NUM-IN                      PIC X(03).                   
            05  NOME-IN                     PIC X(30).                   
       *----------------------------------------------------------------*
        WORKING-STORAGE                     SECTION.                     
       *----------------------------------------------------------------*
        77  FIM-ARQ                         PIC X(03).                   
        77  FS                              PIC X(02) VALUE SPACES.      
       *================================================================*
		      PROCEDURE                           DIVISION.                    
       *================================================================*
        0000-PRINCIPAL                      SECTION.                     
       *----------------------------------------------------------------*
            PERFORM 1000-INICIO.                                         
            PERFORM 2000-TRANSACAO  UNTIL   FIM-ARQ EQUAL 'YES'.         
            PERFORM 3000-FIM.                                            
            STOP RUN.                                                    
       *----------------------------------------------------------------*
        1000-INICIO                         SECTION.                     
       *----------------------------------------------------------------*
            OPEN INPUT CADENT EXTEND CADIN.                              
            PERFORM 1500-LEITURA.                                        
       *----------------------------------------------------------------*
        1500-LEITURA                        SECTION.                     
       *----------------------------------------------------------------*
            READ CADENT AT END MOVE 'YES' TO FIM-ARQ.                    
       *----------------------------------------------------------------*
	2000-TRANSACAO                      SECTION.                     
       *----------------------------------------------------------------*
            MOVE COD-ENT   TO NUM-IN.                                    
            MOVE NOME-PROD TO NOME-IN.                                   
            WRITE REG-IN.                                                
            PERFORM 1500-LEITURA.                                        
       *----------------------------------------------------------------*
        3000-FIM                            SECTION.                     
       *----------------------------------------------------------------*
            CLOSE CADENT  CADIN.                                         
       *----------------------------------------------------------------*                             
