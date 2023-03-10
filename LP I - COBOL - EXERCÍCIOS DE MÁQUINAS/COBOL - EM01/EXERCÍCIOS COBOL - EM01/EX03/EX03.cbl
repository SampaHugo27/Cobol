       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX03.
       AUTHOR. HUGO DA SILVA SAMPAIO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 10/08/2013.
       DATE-COMPILED. 
       SECURITY.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADALU ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADATU ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADALU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADALU.DAT".

       01 REG-ENT.
              02 NUMERO PIC 9(5).
              02 NOME  PIC X(20).
              02 NOTA1 PIC 9(2)V99.
              02 NOTA2 PIC 9(2)V99.
              02 NOTA3 PIC 9(2)V99.

       FD CADATU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADATU.DAT".

       01 REG-SAI.
              02 NUMERO-SAI PIC 9(5).
              02 MEDIA-SAI PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       77 MEDIA  PIC 9(2)V99.
       77 FIM-ARQ  PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAMA03.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADALU OUTPUT CADATU.
              PERFORM VERIFICA-FIM.
       CALCULA-MEDIA.
              MOVE 0 TO MEDIA.
              ADD NOTA1 NOTA2 NOTA3 TO MEDIA.
              DIVIDE 3 INTO MEDIA.   
       COPIA.
              MOVE NUMERO TO NUMERO-SAI.
              PERFORM CALCULA-MEDIA.
              MOVE MEDIA TO MEDIA-SAI.
              WRITE REG-SAI.
       VERIFICA-FIM.
              READ CADALU AT END MOVE 1 TO FIM-ARQ.
       TERMINO.
              CLOSE CADALU CADATU.
       PRINCIPAL.
              PERFORM COPIA.
              PERFORM VERIFICA-FIM.