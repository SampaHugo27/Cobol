JOB INPUT ARQENT START PROC-INICIO FINISH PROC-FINAL
*	                                                  
 WLIDOS      =  WLIDOS      +  1	                   
*	                                                  
 ARQSAI-REC            = ARQENT-REC	                
 IF ARQENT-STATUS      = 'A'	                       
    WATIVOS            = WATIVOS   + 1	             
    PUT ARQSAI	                                     
    WGRAVADOS          = WGRAVADOS + 1	             
 ELSE	                                              
    WINATIVOS          = WINATIVOS + 1	             
 END-IF	                                            
*	                                                  
*	                                                  
*	                                                  
 PROC-INICIO. PROC	                                            
    WLIDOS    = 0	                                             
    WATIVOS   = 0	                                             
    WINATIVOS = 0	                                             
    WGRAVADOS = 0	                                             
 END-PROC	                                                     
*	                                                             
 PROC-FINAL. PROC	                                             
*	                                                             
    DISPLAY '****************************************'	        
    DISPLAY '*     TOTALIZADOR PROGRAMA EASYV01     *'	        
    DISPLAY '*--------------------------------------*'	        
    DISPLAY '*                                      *'	        
    DISPLAY '*  REGISTROS LIDOS ARQENT ....: ' WLIDOS    '  *'	
    DISPLAY '*  REGISTROS GRAVADOS ARQSAI .: ' WGRAVADOS '  *'	
    DISPLAY '*                                      *'	        
    DISPLAY '*  QTDE DE CONTAS ATIVAS .....: ' WATIVOS   '  *'	

      
