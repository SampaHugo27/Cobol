       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID. VRB670S.                                             
                                                                        
       AUTHOR.     HUGO DA SILVA SAMPAIO.                                                   
                                                                        
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       CONFIGURATION SECTION.                                           
                                                                        
       SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.     
                                                                        
      ******************************************************************
                                                                        
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
      ******************************************************************
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       77 WS-DATAATUAL               PIC 9(06)         VALUE ZEROES.    
       77 WS-DATAFUTURA              PIC 9(06)         VALUE ZEROES.    
       77 WS-SOMADIAS                PIC 9(06)         VALUE 365.       
       77 WS-DIA                     PIC 9(02)         VALUE ZEROES.    
       77 WS-DIA4                    PIC 9(04)         VALUE ZEROES.    
       77 WS-MES                     PIC 9(02)         VALUE 00.        
       01 WS-DATA.                                                      
          03 WS-DATA-DD              PIC 9(02)         VALUE ZEROES.    
          03 WS-DATA-BARRA           PIC X(01)         VALUE '/'.       
          03 WS-DATA-MM              PIC 9(02)         VALUE ZEROES.    
          03 WS-DATA-BARRA1          PIC X(01)         VALUE '/'.       
          03 WS-DATA-SECVALOR        PIC 9(02)         VALUE 20.        
          03 WS-DATA-AA              PIC 9(02)         VALUE ZEROES.    
       01 WS-TESTE.                                                     
          03 WS-TESTE-01             PIC X(02)         VALUE 'A '.      
          03 WS-TESTE-02             PIC X(05)         VALUE 'DATA '.   
          03 WS-TESTE-03             PIC X(03)         VALUE 'E: '.     
          03 WS-TESTE-04.                                               
             05 WS-TESTE-05          PIC 9(04)         VALUE 1234.      
                                                                        
      *   03 WS-DATA-HIFEN           PIC X(03)         VALUE ' - '.     
      *   03 WS-DATA-SEC             PIC X(05)         VALUE 'SEC: '.   
      *   03 WS-DATA-SECVALOR        PIC 9(02)         VALUE 20.        
       77 WS-ANO                     PIC 9(02)         VALUE 00.        
       77 WS-VAR                     PIC 9(03)         VALUE ZEROES.    
      ******************************************************************
                                                                        
       PROCEDURE DIVISION.                                              
      *                                                                 
                                                                        
           ACCEPT  WS-DATAATUAL FROM DATE.                              
           DISPLAY 'DATA ATUAL : ' WS-DATAATUAL.                        
           DISPLAY '            +' WS-SOMADIAS.                         
           DISPLAY '             ------'                                
                                                                       
AND        MOVE WS-DATAATUAL (5:2)   TO WS-DATA-DD                
AND        MOVE WS-DATA-DD            TO WS-DIA4.                 
           MOVE WS-DATAATUAL (3:2)   TO WS-DATA-MM.               
           MOVE WS-DATAATUAL (1:2)   TO WS-DATA-AA.               
AND        ADD  WS-SOMADIAS          TO WS-DIA4.                  
                                                                  
           IF WS-DATA-MM = 2                                      
               COMPUTE WS-VAR        = 28 - WS-DATAATUAL          
           END-IF.                                                
                                                                  
           IF WS-DATA-MM = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12      
               COMPUTE WS-VAR        = 31 - WS-DATAATUAL          
           ELSE                                                   
               COMPUTE WS-VAR        = 30 - WS-DATAATUAL          
           END-IF.                                                
                                                                  
           COMPUTE WS-SOMADIAS       = WS-SOMADIAS - WS-VAR.      
      *    COMPUTE WS-SOMADIAS       = WS-SOMADIAS - WS-VAR + 1.  
           ADD 1 TO WS-DATA-MM.                                   
                                                                  
           IF WS-DATA-MM GREATER 12                                     
               ADD 1    TO WS-DATA-AA                                   
           END-IF.                                                      
                                                                        
           PERFORM                                                      
             UNTIL WS-DIA4 LESS   32                                    
                                                                        
AND           IF  WS-DIA4 GREATER 30 OR 31                              
                  ADD 1              TO WS-DATA-MM                      
              END-IF                                                    
                                                                        
                                                                        
              IF  WS-DATA-MM = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12        
AND              COMPUTE WS-DIA4     = WS-DIA4 - 31                     
              ELSE                                                      
AND              COMPUTE WS-DIA4     = WS-DIA4 - 30                     
              END-IF                                                    
           END-PERFORM.                                                 
                                                                        
                                                                       
           COMPUTE   WS-DATA-MM      = WS-DATA-MM - 1                  
                                                                       
      *    IF        WS-MES          = 2                               
      *      COMPUTE WS-DIA4         = WS-DIA4 - 28                    
      *    END-IF.                                                     
      *                                                                
      *    IF WS-MES GREATER 12                                        
      *       COMPUTE WS-MESNOVO     = WS-MES - 12                     
      *       ADD 1                  TO WS-ANO                         
      *    END-IF                                                      
      *    DISPLAY '+ ' WS-SOMADIAS.                                   
                                                                       
                                                                       
           PERFORM                                                     
             UNTIL WS-DATA-MM LESS 12                                  
                                                                       
             IF WS-DATA-MM = 2                                         
                COMPUTE WS-DIA4      = WS-DIA4 - 28                    
             END-IF                                                    
                                                                        
             IF WS-DATA-MM GREATER 12                                   
                ADD 1                TO WS-DATA-AA                      
                COMPUTE WS-DATA-MM   =  WS-DATA-MM - 12                 
                                                                        
      *          COMPUTE WS-MESNOVO  = WS-MES - 12                      
      *          MOVE 1              TO WS-MESN2                        
      *          MOVE WS-MESNOVO     TO WS-MESN2                        
      *          MOVE WS-MESNOVO     TO WS-MESN2                        
      *       ELSE                                                      
      *          MOVE WS-MES         TO WS-MESN2                        
             END-IF                                                     
           END-PERFORM.                                                 
                                                                        
           IF WS-DATA-AA GREATER 99                                     
              ADD 1                  TO WS-DATA-SECVALOR                
           END-IF.                                                      
                                                                        
           MOVE WS-DIA4              TO WS-DATA-DD.                     
           DISPLAY 'DATA FUTURA: ' WS-DATA.                             
                                                                        
           MOVE 4321                 TO WS-TESTE-05.                    
           DISPLAY WS-TESTE.                                            
                                                                        
           STOP RUN.                                                    

      
