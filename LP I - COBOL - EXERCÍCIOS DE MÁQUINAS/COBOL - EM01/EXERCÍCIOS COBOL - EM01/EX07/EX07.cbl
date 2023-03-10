       IDENTIFICATION DIVISION.
 
       PROGRAM-ID. EX07.
       AUTHOR. HUGO DA SILVA SAMPAIO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 12/08/2013.
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
             02 NOTA4 PIC 9(2)V99.
             02 SEXO  PIC X(1).

       FD CADATU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADATU.DAT".

       01 REG-SAI.
             02 NUMERO-SAI PIC 9(5).
             02 NOME-SAI  PIC X(20).
             02 MEDIA-SAI PIC 9(2)V99.
             02 SEXO-SAI  PIC X(1).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ  PIC 9(1) VALUE 0.
       77 MEDIA  PIC 9(2)V99.

       PROCEDURE DIVISION.

       PROGRAMA07.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
             OPEN INPUT CADALU OUTPUT CADATU.
       COPIA.
             PERFORM CALCULA-MEDIA.
             MOVE NUMERO TO NUMERO-SAI.
             MOVE NOME TO NOME-SAI.
             MOVE MEDIA TO MEDIA-SAI.
             MOVE SEXO TO SEXO-SAI.
             WRITE REG-SAI.
       CALCULA-MEDIA.
             MOVE 0 TO MEDIA.
             ADD NOTA1 NOTA2 NOTA3 NOTA4 TO MEDIA.
             DIVIDE 4 INTO MEDIA.
       SELECAO.
             IF SEXO EQUAL "F" OR "f"
                   PERFORM COPIA.
       TERMINO.
             CLOSE CADALU CADATU.
       VERIFICA-FIM.
             READ CADALU AT END MOVE 1 TO FIM-ARQ.
       PRINCIPAL.
             PERFORM SELECAO.
             PERFORM VERIFICA-FIM.