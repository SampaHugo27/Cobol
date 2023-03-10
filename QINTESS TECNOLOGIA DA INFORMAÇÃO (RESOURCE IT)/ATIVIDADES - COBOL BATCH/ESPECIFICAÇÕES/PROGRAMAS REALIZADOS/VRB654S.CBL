       	IDENTIFICATION	                     DIVISION.                    
       *----------------------------------------------------------------*
        PROGRAM-ID.                         EM03EX01.                    
        AUTHOR.                             HUGO SAMPAIO.                
        INSTALLATION.                       QINTESS.                     
        DATE-WRITTEN.                                                    
        DATE-COMPILED.                                                   
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        ENVIRONMENT                         DIVISION.                    
       *----------------------------------------------------------------*
        CONFIGURATION                       SECTION.                     
       *----------------------------------------------------------------*
        SOURCE-COMPUTER.                    IBM-PC.                      
        OBJECT-COMPUTER.                    IBM-PC.                      
        SPECIAL-NAMES.                      DECIMAL-POINT IS COMMA.      
       *----------------------------------------------------------------*
        INPUT-OUTPUT                        SECTION.                     
       *----------------------------------------------------------------*
        FILE-CONTROL.                                                    
                                                                         
            SELECT CADENT ASSIGN TO CADENT                               
            ORGANIZATION  IS SEQUENTIAL.                                 
       *----------------------------------------------------------------*
            SELECT TRAB   ASSIGN TO TRAB.                                
       *----------------------------------------------------------------*
            SELECT CADSAI ASSIGN TO CADSAI                               
            ORGANIZATION  IS SEQUENTIAL.                                 
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        DATA                                DIVISION.                    
       *----------------------------------------------------------------*
        FILE                                SECTION.                     
       *----------------------------------------------------------------*
        FD CADENT                                                        
            LABEL RECORD ARE STANDARD.
		      01  REG-ENT.                                                     
            05 CODIGO-ENT               PIC 9(05).                       
            05 NOME-ENT                 PIC X(30).                       
            05 SEXO-ENT                 PIC X(01).                       
       *----------------------------------------------------------------*
        SD TRAB.                                                         
        01  REG-TRAB.                                                    
            05 CODIGO-TRAB              PIC 9(05).                       
            05 NOME-TRAB                PIC X(30).                       
            05 SEXO-TRAB                PIC X(01).                       
       *----------------------------------------------------------------*
        FD CADSAI                                                        
            LABEL RECORD ARE STANDARD.                                   
        01  REG-SAI.                                                     
            05 CODIGO-SAI               PIC 9(05).                       
            05 NOME-SAI                 PIC X(30).                       
            05 SEXO-SAI                 PIC X(01).                       
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*
        PROCEDURE                       DIVISION.                        
       *----------------------------------------------------------------*
            SORT  TRAB                                                   
                  ASCENDING KEY  CODIGO-TRAB                             
                  ASCENDING KEY  NOME-TRAB                               
                  USING          CADENT                                  
                  GIVING         CADSAI                                  
            STOP RUN.                                                    
       *----------------------------------------------------------------*
       *----------------------------------------------------------------*                                   

      
