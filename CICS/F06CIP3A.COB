      *================================================================J00001005
       IDENTIFICATION                            DIVISION.              00002005
      *================================================================*00003005
                                                                        00004005
       PROGRAM-ID.  F06CIP3.                                            00005005
                                                                        00006005
      *================================================================*00007005
      *                        TREINAMENTO                             *00008005
      *================================================================*00009005
      *     PROGRAMA....: F06CI03                                      *00010005
      *     PROGRAMADOR.: JOEI LORENTI                                 *00020005
      *     ANALISTA....: IVAN SANCHES                                 *00030005
      *     DATA........: 21/06/2022                                   *00040005
      *----------------------------------------------------------------*00050005
      *     OBJETIVO....: PROGRAMA DE MANIPULACAO DE DADOS DA TABELA   *00070012
      *                   DB2 (FUNC) ATRAVES DE MAPA BMS (F06CIM1)     *00070112
      *                                                                *00070205
      *----------------------------------------------------------------*00070305
      *     ARQUIVOS....:                                              *00070405
      *       DDNAME               I/O                 INCLUDE/BOOK    *00070505
      *----------------------------------------------------------------*00070605
      *     BASE DE DADOS:                                             *00070705
      *       DDNAME               I/O                 INCLUDE/BOOK    *00070805
      *       FOUR001.FUNC          I                    BOOKFUNC       00070905
      *----------------------------------------------------------------*00071005
      *     MODULOS.....:                                              *00071105
      *                                                                *00071205
      *================================================================*00071305
                                                                        00071405
      *================================================================*00071505
       ENVIRONMENT                               DIVISION.              00071605
      *================================================================*00071705
                                                                        00071805
      *----------------------------------------------------------------*00071905
       CONFIGURATION                             SECTION.               00072005
      *----------------------------------------------------------------*00072105
                                                                        00072205
       SPECIAL-NAMES.                                                   00072305
           DECIMAL-POINT                         IS COMMA.              00072405
                                                                        00072505
      *----------------------------------------------------------------*00072605
       INPUT-OUTPUT                              SECTION.               00072705
      *----------------------------------------------------------------*00072805
                                                                        00072905
      *================================================================*00073005
       DATA                                      DIVISION.              00073105
      *================================================================*00073205
                                                                        00073305
      *----------------------------------------------------------------*00073405
       WORKING-STORAGE                           SECTION.               00073505
      *----------------------------------------------------------------*00073605
                                                                        00073705
      *----------------------------------------------------------------*00073705
       01 FILLER                       PIC  X(050)         VALUE        00073705
           '***  FR06CIP2 - INICIO DA AREA DE WORKING   ***'.           00073705
      *----------------------------------------------------------------*00073705
                                                                        00073705
      *----------------------------------------------------------------*00073705
       01 FILLER                       PIC  X(050)         VALUE        00073705
           '***  AREA DO MAPA - F06CIM1   ***'.                         00073705
      *----------------------------------------------------------------*00073705
                                                                        00073705
           COPY F06CIM1.                                                00073805
           COPY DFHAID.                                                 00074008
      *----------------------------------------------------------------*00073705
       01 FILLER                       PIC  X(050)         VALUE        00073705
           '***  VARIAVEIS DE NULIDADE ***'.                            00073705
      *----------------------------------------------------------------*00073705
                                                                        00073705
       77 WRK-EMAIL-NULL               PIC S9(04) COMP.                 00074908

      *----------------------------------------------------------------*00073705
       01 FILLER                       PIC  X(050)         VALUE        00073705
           '***  VARIAVEIS DE APOIO  ***'.                              00073705
      *----------------------------------------------------------------*00073705

       01 WRK-RET                      PIC S9(04) COMP.

       01 WRK-DADOS.
          05 WRK-CHAVE                 PIC X(05).
          05 WRK-NOME                  PIC X(30).
          05 WRK-SETOR                 PIC X(04).
          05 WRK-SALARIO               PIC X(10).
          05 WRK-DATAADM               PIC X(10).
          05 WRK-EMAIL                 PIC X(40).

       77 WRK-SQLCODE                  PIC -999.                        00075008

       77 WRK-FLAG                     PIC 9  VALUE ZEROS.              00075008

       77 WRK-SALI                     PIC S9(04).                      00075008
                                                                        00075108
       01 WRK-DATA.                                                     00075208
         05 WRK-DIA                    PIC X(02) VALUE SPACE.           00075710
         05 FILLER                     PIC X(01) VALUE '-' .            00075410
         05 WRK-MES                    PIC X(02) VALUE SPACE.           00075508
         05 FILLER                     PIC X(01) VALUE '-' .            00075610
         05 WRK-ANO                    PIC X(04) VALUE SPACE.           00075310
                                                                        00075805
       77 WRK-ACU-INCONS               PIC 9(02) VALUE ZEROS.           00075805
                                                                        00075805
       77 WRK-SAIDA                    PIC X(30) VALUE                  00075805
                                       'ENCERRADO PELO USUARIO'.        00075805
                                                                        00075805
      *----------------------------------------------------------------*00075905
                                                                        00076005
      *================================================================*00076105
       PROCEDURE                                 DIVISION.              00076205
      *================================================================*00076305
                                                                        00076405
      ******************************************************************00076511
      *                     PROCESSAMENTO PRINCIPAL                    *00076611
      ******************************************************************00076711
      *----------------------------------------------------------------*00076811
       0000-PRINCIPAL                            SECTION.               00076911
      *----------------------------------------------------------------*00077011
                                                                        00077111
           PERFORM 1000-INICIAR.                                        00077211
                                                                        00077311
            IF WRK-FLAG EQUAL 2                                         00077311
               PERFORM 2000-PROCESSAR                                   00077411
            END-IF.                                                     00077311
                                                                        00077311
           PERFORM 3000-FINALIZAR.                                      00077511
                                                                        00077511
                                                                        00077511
             EXEC CICS                                                  00088716
               RETURN TRANSID('T063')                                   00088816
             END-EXEC.                                                  00088916

      *----------------------------------------------------------------*00078005
       0000-99-FIM.                              EXIT.                  00078105
      *----------------------------------------------------------------*00078205
                                                                        00078305
      *----------------------------------------------------------------*00078405
       1000-INICIAR                              SECTION.               00078505
      *----------------------------------------------------------------*00078605
                                                                        00078705
           EXEC CICS RECEIVE                                            00080305
             MAPSET('F06CIM1')                                          00080405
             MAP('MAPTEST')                                             00080505
             INTO(MAPTESTI)                                             00080605
             RESP(WRK-RET)
           END-EXEC.                                                    00080705
                                                                        00080805
           IF WRK-RET = DFHRESP(MAPFAIL)                                00080805
              MOVE 1 TO WRK-FLAG                                        00080805
           ELSE                                                         00080805
              MOVE 2 TO WRK-FLAG                                        00080805
           END-IF.                                                      00080805
      *----------------------------------------------------------------*00078605
       1000-99-FIM.                              EXIT.                  00079605
      *----------------------------------------------------------------*00078605

      *----------------------------------------------------------------*00078605
       2000-PROCESSAR                            SECTION.               00080005
      *----------------------------------------------------------------*00078605
                                                                        00080105
           IF EIBAID = '3'                                              00081305
              PERFORM 2100-RETURN                                       00081412
           END-IF.                                                      00081905
                                                                        00082005
           IF EIBAID = '5'                                              00082512
              PERFORM 2200-READ                                         00082612
           END-IF.                                                      00086512
                                                                        00086612
           IF EIBAID = ''''
              PERFORM 2300-ENTER
           END-IF.

           EXEC CICS SEND                                               00088116
             MAPSET('F06CIM1')                                          00088216
             MAP('MAPTEST')                                             00088316
             DATAONLY                                                   00088416
           END-EXEC.                                                    00088516
                                                                        00089012
      *----------------------------------------------------------------*00078605
       2000-99-FIM.                              EXIT.                  00089112
      *----------------------------------------------------------------*00078605
                                                                        00089312
      *----------------------------------------------------------------*00078605
       2100-RETURN                               SECTION.               00089412
      *----------------------------------------------------------------*00078605
                                                                        00089612
           EXEC CICS SEND TEXT                                          00089712
             FROM (WRK-SAIDA)                                           00089812
             ERASE                                                      00089812
           END-EXEC                                                     00089916
           EXEC CICS                                                    00089916
             RETURN                                                     00089916
           END-EXEC.                                                    00089916
                                                                        00089916
                                                                        00090012
      *----------------------------------------------------------------*00089512
       2100-99-FIM.                              EXIT.                  00090212
      *----------------------------------------------------------------*00089512

      *----------------------------------------------------------------*00089512
       2200-READ                                 SECTION.               00090612
      *----------------------------------------------------------------*00089512
                                                                        00090812
              MOVE IDI                 TO WRK-CHAVE                     00090912
                                                                        00091012
              EXEC CICS READ                                            00091012
                 FILE ('FUNC')                                          00091012
                 RIDFLD(WRK-CHAVE)                                      00091012
                 INTO(WRK-DADOS)                                        00091012
                 RESP(WRK-RET)                                          00091012
              END-EXEC                                                  00091012
                                                                        00091012
                                                                        00091012
              EVALUATE TRUE                                             00091012
                  WHEN WRK-RET EQUAL DFHRESP(NORMAL)                    00091012
                                                                        00091012
                  MOVE WRK-CHAVE       TO IDO                           00091012
                  MOVE WRK-NOME        TO NOMEO                         00091112
                  MOVE WRK-SETOR       TO SETORO                        00091212
                                                                        00091212
                  IF   WRK-SALARIO     NOT NUMERIC OR                   00091412
                       WRK-SALARIO     EQUAL ZEROS                      00091412
                    MOVE 'SAL INCONS'                                   00091412
                                       TO MSGO(14:10)                   00091412
                    ADD 1              TO WRK-ACU-INCONS                00091412
                  ELSE                                                  00091412
                    MOVE WRK-SALARIO     TO SALO                        00091312
                  END-IF                                                00091312
                  MOVE WRK-DATAADM     TO WRK-DATA                      00091312
                  MOVE WRK-ANO         TO DATAANOO                      00091412
                  MOVE WRK-MES         TO DATAMESO                      00091412
                  MOVE WRK-DIA         TO DATADIAO                      00091412
                  MOVE WRK-EMAIL       TO EMAILO                        00091712
                  MOVE 'ENCONTRADO - ' TO MSGO(01:13)                   00091512
                                                                        00091512
                  WHEN WRK-RET EQUAL DFHRESP(NOTFND)                    00091612
                                                                        00091612
                  MOVE 'NAO ENCONTRADO '                                00091812
                                       TO MSGO                          00091812
                                                                        00091812
                WHEN OTHER                                              00091812
                                                                        00091812
                   MOVE 'ERRO..:'      TO MSGO                          00091812
                                                                        00091812
                                                                        00091812
               END-EVALUATE.                                            00091812
                                                                        00091812
                                                                        00091812
      *----------------------------------------------------------------*00094912
       2200-99-FIM.                              EXIT.                  00095012
      *----------------------------------------------------------------*00095112
                                                                        00108615
      *----------------------------------------------------------------*00108715
       2300-ENTER                                SECTION.               00108815
      *----------------------------------------------------------------*00108915

              EXEC CICS SEND                                            00108615
                MAPSET('F06CIM1')                                       00108615
                MAP('MAPTEST')                                          00108615
                ERASE                                                   00108615
                MAPONLY                                                 00108615
              END-EXEC.                                                 00108615

             MOVE 1 TO WRK-FLAG.
                                                                        00108615
             EXEC CICS                                                  00088716
               RETURN TRANSID('T063')                                   00088816
             END-EXEC.                                                  00088916
                                                                        00088916

      *----------------------------------------------------------------*00109115
       2300-99-FIM.                             EXIT.                   00109215
      *----------------------------------------------------------------*00110012
      *----------------------------------------------------------------*00108715
       3000-FINALIZAR                            SECTION.               00108815
      *----------------------------------------------------------------*00108915
                                                                        00108615
           IF WRK-FLAG EQUAL 1                                          00108615
              EXEC CICS SEND                                            00108615
                MAPSET('F06CIM1')                                       00108615
                MAP('MAPTEST')                                          00108615
                ERASE                                                   00108615
                MAPONLY                                                 00108615
              END-EXEC                                                  00108615
           ELSE                                                         00108615
                                                                        00108615
             EXEC CICS SEND                                             00108615
               MAPSET('F06CIM1')                                        00108615
               MAP('MAPTEST')                                           00108615
               DATAONLY                                                 00108615
             END-EXEC                                                   00108615
           END-IF.                                                      00108615
                                                                        00108615
      *----------------------------------------------------------------*00109115
       3000-99-FIM.                             EXIT.                   00109215
      *----------------------------------------------------------------*00110012
      *----------------------------------------------------------------*00108715
       9000-TRATAR-ERRO                          SECTION.               00108815
      *----------------------------------------------------------------*00108915
                                                                        00109015
      *----------------------------------------------------------------*00109115
       9000-99-FIM.                             EXIT.                   00109215
      *----------------------------------------------------------------*00110012
