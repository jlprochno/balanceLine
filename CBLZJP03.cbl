       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZJP03.
      ******************************************************************
      * Author: JENYFFER LAURA PROCHNO PEREIRA
      * Date: 19012024
      * Purpose: ENTREGA DO EXERCICIO ACERCA DO BALANCE LINE
      * Tectonics: .CBL
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *******************************************************************
      *    DEFINICAO DOS ARQUIVOS                                       *
      *******************************************************************
       SELECT MATRIZ ASSIGN TO
       'C:\Cobol\02.TAREFAS\MATRIZ.txt'
       FILE STATUS IS AS-STATUS-E1.

       SELECT SOCIOS ASSIGN TO
       'C:\Cobol\02.TAREFAS\SOCIOS.txt'
       FILE STATUS IS AS-STATUS-E2.

       SELECT RELATO ASSIGN TO
       'C:\Cobol\02.TAREFAS\BALANCEJP.txt'
       FILE STATUS IS AS-STATUS-S.

       DATA DIVISION.
       FILE SECTION.

      *******************************************************************
      *    DEFINICAO DA MATRIZ                                       *
      *******************************************************************
       FD  MATRIZ
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  ARQ-MATRIZ                   PIC X(096).
       01  FILLER REDEFINES ARQ-MATRIZ.
           05 ARQ-M-CNPJ                PIC X(014).
           05 ARQ-M-SIT                 PIC X(006).
           05 ARQ-M-NOME                PIC X(059).
           05 ARQ-M-STATUS              PIC X(005).
           05 ARQ-M-DATA-ABERT          PIC X(010).
           05 ARQ-M-FIM                 PIC X(002).

      *******************************************************************
      *    DEFINICAO DOS SOCIOS                                         *
      *******************************************************************
       FD  SOCIOS
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  ARQ-SOCIOS                  PIC X(071).
       01  FILLER REDEFINES ARQ-SOCIOS.
           05 ARQ-S-CNPJ               PIC 9(014).
           05 ARQ-S-CNPJ-SOCIO         PIC 9(014).
           05 ARQ-S-NOME               PIC X(036).
           05 ARQ-S-STATUS             PIC X(005).
           05 ARQ-S-FIM                PIC X(002).

      *******************************************************************
      *    DEFINICAO DO RELATORIO                                          *
      *******************************************************************
       FD  RELATO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  ARQ-RELATO                 PIC X(062).

      *******************************************************************
      *    DEFINICAO DAS VARIAVEIS                                      *
      *******************************************************************
       WORKING-STORAGE SECTION.

       01  AS-STATUS-E1            PIC 9(002) VALUE ZEROS.
       01  AS-STATUS-E2            PIC 9(002) VALUE ZEROS.
       01  AS-STATUS-S             PIC 9(002) VALUE ZEROS.
       01  AS-FIM                  PIC X(001) VALUE 'N'.
       01  AS-DATA                 PIC X(008) VALUE ZEROS.
       01  AS-HORA                 PIC X(006) VALUE ZEROS.
       01  AS-ULTIMO-NOME          PIC X(036) VALUE SPACES.

      *******************************************************************
      *    DEFINICAO DO CABECALHO                                       *
      *******************************************************************
       01  WS-CABEC-REL1           PIC X(060) VALUE ALL '='.

       01  WS-CABEC-REL2.
           05 WS-CABEC-REL2-FL1    PIC X(001) VALUE SPACES.
           05 WS-CABEC-REL2-PGM    PIC X(008) VALUE 'CBLZJP03'.
           05 WS-CABEC-REL2-FL2    PIC X(015) VALUE SPACES.
           05 WS-CABEC-REL2-DES    PIC X(009) VALUE 'VOLVO S.A'.
           05 WS-CABEC-REL2-FL3    PIC X(016) VALUE SPACES.
           05 WS-CABEC-REL2-DT     PIC X(010) VALUE SPACES.
           05 WS-CABEC-REL2-FL4    PIC X(001) VALUE SPACES.

       01  WS-CABEC-REL3.
           05 WS-CABEC-REL3-FL1    PIC X(001) VALUE SPACES.
           05 WS-CABEC-REL3-HR     PIC X(008) VALUE SPACES.
           05 WS-CABEC-REL3-FL2    PIC X(014) VALUE SPACES.
           05 WS-CABEC-REL1-DES    PIC X(030) VALUE 'BALANCE LINE'.
           05 WS-CABEC-REL3-FL3    PIC X(007) VALUE SPACES.

      *******************************************************************
      *    DEFINICAO DA PRIMEIRA LINHA DO DETALHE                       *
      *******************************************************************
       01  LINDET01-REL.
           05 LINDET01-REL-CNPJ    PIC X(004) VALUE 'CNPJ'.
           05 LINDET01-REL-SPACE2  PIC X(013) VALUE SPACES.
           05 LINDET01-REL-NOME    PIC X(008) VALUE 'NOME'.
           05 LINDET01-REL-SPACE3  PIC X(034) VALUE SPACES.


      *******************************************************************
      *    DEFINICAO DA SEGUNDA LINHA DO DETALHE                        *
      *******************************************************************
       01  LINDET02-REL.
           05 LINDET02-REL-CNPJ-S  PIC X(010) VALUE 'CNPJ SOCIO'.
           05 LINDET02-REL-SPACE4  PIC X(011) VALUE SPACES.
           05 LINDET02-REL-NOME-S  PIC X(010) VALUE 'NOME SOCIO'.
           05 LINDET02-REL-SPACE5  PIC X(028) VALUE SPACES.

      *******************************************************************
      *    DEFINICAO DO CONTEUDO DA TABELA ORIUNDOS DA MATRIZ           *
      *******************************************************************
       01  LINDET03-REL.
           05 LINDET03-REL-CNPJ    PIC 9(014) VALUE ZEROS.
           05 LINDET03-REL-SPACE2  PIC X(003) VALUE SPACES.
           05 LINDET03-REL-NOME-M  PIC X(043) VALUE SPACES.

      *******************************************************************
      *    DEFINICAO DO CONTEUDO DA TABELA ORIUNDOS DA SOCIOS           *
      *******************************************************************
       01  LINDET04-REL.
           05 LINDET04-REL-CNPJ-S  PIC 9(014) VALUE ZEROS.
           05 LINDET04-REL-SPACE4  PIC X(007) VALUE SPACES.
           05 LINDET04-REL-NOME-S  PIC X(037) VALUE SPACES.

      *******************************************************************
      *    CRIACAO E DECLARACAO DOS PERFORMS                            *
      *******************************************************************

       PROCEDURE DIVISION.
       0000-PRINCIPAL              SECTION.

           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-PROCESSAR UNTIL AS-FIM = 'S'.
           PERFORM 2100-LER-MATRIZ.
           PERFORM 2200-LER-SOCIOS.
           PERFORM 2300-CONSOLIDAR-IGUAIS.
           PERFORM 2400-GERAR-RELATO.
           PERFORM 3000-FINALIZAR.

       0000-PRINCIPAL-FIM.
           EXIT.
      *******************************************************************
      *    INICIALIZACAO DO PROGRAMA                                    *
      *******************************************************************
       1000-INICIALIZAR             SECTION.

      *    DEFINICAO DE HORA E DATA ATUAL

           ACCEPT AS-DATA           FROM DATE YYYYMMDD.
           ACCEPT AS-HORA           FROM TIME.

           MOVE AS-DATA(1:4)        TO WS-CABEC-REL2-DT(7:4)
           MOVE AS-DATA(5:2)        TO WS-CABEC-REL2-DT(4:2)
           MOVE AS-DATA(7:2)        TO WS-CABEC-REL2-DT(1:2)
           MOVE '/'                 TO WS-CABEC-REL2-DT(3:1)
           MOVE '/'                 TO WS-CABEC-REL2-DT(6:1)

           MOVE AS-HORA(1:2)        TO WS-CABEC-REL3-HR(1:2)
           MOVE AS-HORA(3:2)        TO WS-CABEC-REL3-HR(4:2)
           MOVE AS-HORA(5:2)        TO WS-CABEC-REL3-HR(7:2)
           MOVE ':'                 TO WS-CABEC-REL3-HR(3:1)
           MOVE ':'                 TO WS-CABEC-REL3-HR(6:1)

           OPEN INPUT MATRIZ.
           IF AS-STATUS-E1 NOT EQUAL ZEROS
               DISPLAY 'ERRO NA ABERTURA' AS-STATUS-E1
           END-IF

           OPEN INPUT SOCIOS.
           IF AS-STATUS-E2 NOT EQUAL ZEROS
               DISPLAY 'ERRO NA ABERTURA DO SOCIOS' AS-STATUS-E2
           END-IF

           OPEN OUTPUT RELATO.
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ERRO NA ABERTURA DO RELATO' AS-STATUS-S
           END-IF

      *    INSERCAO DO CABECALHO NO RELATORIO

               MOVE WS-CABEC-REL1    TO ARQ-RELATO
               WRITE ARQ-RELATO
               MOVE WS-CABEC-REL2    TO ARQ-RELATO
               WRITE ARQ-RELATO
               MOVE WS-CABEC-REL3    TO ARQ-RELATO
               WRITE ARQ-RELATO
               MOVE WS-CABEC-REL1    TO ARQ-RELATO
               WRITE ARQ-RELATO
               MOVE LINDET01-REL     TO ARQ-RELATO
               WRITE ARQ-RELATO
               MOVE LINDET02-REL     TO ARQ-RELATO
               WRITE ARQ-RELATO

      *    IMPRESSAO CABECALHO NO LOG
           DISPLAY WS-CABEC-REL1
           DISPLAY WS-CABEC-REL2
           DISPLAY WS-CABEC-REL3
           DISPLAY WS-CABEC-REL1
           DISPLAY LINDET01-REL
           DISPLAY LINDET02-REL

           READ MATRIZ
           IF AS-STATUS-E1 NOT EQUAL ZEROS
               DISPLAY 'ARQUIVO MATRIZ VAZIO'
               MOVE 'S'             TO AS-FIM
           END-IF

           READ SOCIOS
           IF AS-STATUS-E2 NOT EQUAL ZEROS
               DISPLAY 'ARQUIVO SOCIOS VAZIO'
               MOVE 'S'             TO AS-FIM
           END-IF
           .
       1000-INICIALIZAR-FIM.
           EXIT.

      *******************************************************************
      *    LOGICA CENTRAL DO PROGRAMA - 2000-PROCESSAR                  *
      *******************************************************************
       2000-PROCESSAR                 SECTION.

           IF ARQ-M-CNPJ < ARQ-S-CNPJ
              PERFORM 2100-LER-MATRIZ
              ELSE
                  IF ARQ-M-CNPJ > ARQ-S-CNPJ
                     PERFORM 2200-LER-SOCIOS
                       ELSE
                           PERFORM 2300-CONSOLIDAR-IGUAIS
                   END-IF
           END-IF
           .
       2000-PROCESSAR-FIM.
           EXIT.

      *******************************************************************
      *    2100-LER-MATRIZ                                              *
      *******************************************************************
       2100-LER-MATRIZ                SECTION.

           READ MATRIZ
                AT END MOVE 'S'       TO AS-FIM
                NOT AT END
                   IF AS-STATUS-E1 NOT EQUAL ZEROS
                       DISPLAY 'ERRO NA LEITURA DA MATRIZ' AS-STATUS-E1
                       MOVE 'S'        TO AS-FIM
                       PERFORM 3000-FINALIZAR-FIM
                   END-IF
           .
       2100-LER-MATRIZ-FIM.
           EXIT.

      *******************************************************************
      *    2200-LER-SOCIOS                                            *
      *******************************************************************
       2200-LER-SOCIOS                SECTION.

           READ SOCIOS
                AT END MOVE 'S'      TO AS-FIM
                NOT AT END
                   IF AS-STATUS-E2 NOT EQUAL ZEROS
                       DISPLAY 'ERRO NA LEITURA DE SOCIOS' AS-STATUS-E2
                       MOVE 'S' TO AS-FIM
                       PERFORM 3000-FINALIZAR
                   END-IF
           .
       2200-LER-SOCIOS-FIM.
           EXIT.

      *******************************************************************
      *    2300-PROCESSAR-IGUAIS                                             *
      *******************************************************************
       2300-CONSOLIDAR-IGUAIS            SECTION.

           PERFORM 2400-GERAR-RELATO
           PERFORM 2200-LER-SOCIOS
           .
       2300-CONSOLIDAR-IGUAIS-FIM.
           EXIT.

      *******************************************************************
      *    2400-GERAR-RELATO                                            *
      *******************************************************************
       2400-GERAR-RELATO            SECTION.

           IF ARQ-M-CNPJ EQUAL AS-ULTIMO-NOME

               MOVE ARQ-S-CNPJ-SOCIO TO LINDET04-REL-CNPJ-S
               MOVE ARQ-S-NOME       TO LINDET04-REL-NOME-S

               DISPLAY LINDET04-REL

               MOVE LINDET04-REL     TO ARQ-RELATO
               WRITE ARQ-RELATO

           ELSE
               MOVE ARQ-M-CNPJ       TO LINDET03-REL-CNPJ
               MOVE ARQ-M-NOME       TO LINDET03-REL-NOME-M

               DISPLAY LINDET03-REL

               MOVE LINDET03-REL     TO ARQ-RELATO
               WRITE ARQ-RELATO

               MOVE ARQ-M-CNPJ       TO AS-ULTIMO-NOME

               MOVE ARQ-S-CNPJ-SOCIO TO LINDET04-REL-CNPJ-S
               MOVE ARQ-S-NOME       TO LINDET04-REL-NOME-S

               DISPLAY LINDET04-REL

               MOVE LINDET04-REL     TO ARQ-RELATO
               WRITE ARQ-RELATO

           END-IF
           .
       2400-GERAR-RELATO-FIM.
           EXIT.

      *******************************************************************
      *    FINALIZAR PROGRAMA                                           *
      *******************************************************************
       3000-FINALIZAR                SECTION.
      *    CHAMO AQUI OS PERFORMS PARA PEGAR O ULTIMO REGISTRO DA MATRIZ
           PERFORM 2000-PROCESSAR
           PERFORM 2100-LER-MATRIZ
           PERFORM 2200-LER-SOCIOS
           PERFORM 2300-CONSOLIDAR-IGUAIS
           PERFORM 2400-GERAR-RELATO

           CLOSE MATRIZ.
           CLOSE SOCIOS.

           IF AS-STATUS-E1 NOT EQUAL ZEROS
               DISPLAY 'ERRO NO FECHAMENTO' AS-STATUS-E1
           END-IF

           IF AS-STATUS-E2 NOT EQUAL ZEROS
               DISPLAY 'ERRO NO FECHAMENTO DO RELATO' AS-STATUS-E2
           END-IF

           CLOSE RELATO.
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ERRO NO FECHAMENTO DO RELATO' AS-STATUS-S
           END-IF

           STOP RUN
           .
       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM CBLZJP03.
