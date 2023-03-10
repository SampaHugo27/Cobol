       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX05.
       AUTHOR. HUGO DA SILVA SAMPAIO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 11/08/2013.
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
       SELECT CADAPR ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

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
              02 FALTAS PIC 9(2).

       FD CADAPR
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADAPR.DAT".

       01 REG-SAI.
              02 NUMERO-SAI PIC 9(5).
              02 NOME-SAI  PIC X(20).
              02 MEDIA-SAI PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       77 MEDIA  PIC 9(2)V99.
       77 FIM-ARQ  PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADALU OUTPUT CADAPR.
              PERFORM VERIFICA-FIM.
       COPIA.
              MOVE NUMERO TO NUMERO-SAI.
              MOVE NOME   TO NOME-SAI.
              MOVE MEDIA  TO MEDIA-SAI.
              WRITE REG-SAI.
       SELECAO.
              PERFORM CALCULA-MEDIA.
              IF MEDIA NOT LESS THAN 7 AND FALTAS NOT GREATER THAN 18
                     PERFORM COPIA.
       TERMINO.
              CLOSE CADALU CADAPR.
       VERIFICA-FIM.
              READ CADALU AT END MOVE 1 TO FIM-ARQ.
       CALCULA-MEDIA.
              MOVE 0 TO MEDIA.
              ADD NOTA1 NOTA2 TO MEDIA.
              DIVIDE 2 INTO MEDIA.
       PRINCIPAL.
              PERFORM SELECAO.
              PERFORM VERIFICA-FIM.