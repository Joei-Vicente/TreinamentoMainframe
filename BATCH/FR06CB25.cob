      *================================================================J00001001
       IDENTIFICATION                  DIVISION.                        00002001
      *================================================================*00003001
                                                                        00004001
       PROGRAM-ID. FR06CB25.                                            00005007
                                                                        00006001
      *================================================================*00007001
      *                                                                 00008001
      *     AUTOR.....: JOEI LORENTI                                    00009002
      *     ANALISTA..: IVAN SANCHES                                    00009102
      *     EMPRESA...: FOURSYS                                         00010002
      *     DATA......: 27/04/2022                                      00020002
      *                                                                 00021001
      *----------------------------------------------------------------*00022001
      *                                                                 00023001
      *     OBJETIVO: RECEBER NAS VARIAVEIS WRK-NOTA1 -> 0800(2DEC),    00024001
      *               WRK-NOTA2 -> 0700(2DEC).                          00025001
      *               CALCULAR A MEDIA(WRK-MEDIA)-REGRAS(STATUS):       00026001
      *               * MENOR QUE 2 - REPROVADO                         00027001
      *               * ENTRE 2 E 5,99 - RECUPERACAO                    00028001
      *               * ENTRE 6 E 8,99 - APROVADO                       00029001
      *               * ENTRE 9 E 10 - APROVADO COM LOUVOR              00030001
      *               * MAIOR QUE 10 - MEDIA ERRADA                     00040001
      *               COMANDO EVALUATE.                                 00050001
      *================================================================*00060001
       ENVIRONMENT                     DIVISION.                        00070009
      *================================================================*00071001
                                                                        00071101
      *----------------------------------------------------------------*00071201
       CONFIGURATION                   SECTION.                         00071309
      *----------------------------------------------------------------*00071401
                                                                        00071505
       SPECIAL-NAMES.                                                   00071601
           DECIMAL-POINT IS COMMA.                                      00071701
                                                                        00071805
      *================================================================J00071909
       DATA                            DIVISION.                        00072009
      *================================================================*00072101
                                                                        00072201
      *----------------------------------------------------------------*00072309
       WORKING-STORAGE                 SECTION.                         00072409
      *----------------------------------------------------------------*00072501
                                                                        00072601
       01 WRK-NOTAS.                                                    00072701
          05 WRK-NOTA1                 PIC 9(02)V99     VALUE ZEROS.    00072808
          05 WRK-NOTA2                 PIC 9(02)V99     VALUE ZEROS.    00072908
                                                                        00073001
       77 WRK-MEDIA                    PIC 9(02)V99     VALUE ZEROS.    00073108
                                                                        00073201
       77 WRK-STATUS                   PIC X(45)        VALUE SPACES.   00073301
                                                                        00073601
      *================================================================*00073701
       PROCEDURE                       DIVISION.                        00073809
      *================================================================*00073901
                                                                        00074001
      *----------------------------------------------------------------*00074101
       0000-PRINCIPAL                  SECTION.                         00074209
      *----------------------------------------------------------------*00074301
                                                                        00074409
           PERFORM 0100-INICIAR.                                        00074509
               IF WRK-NOTAS NOT EQUAL ZEROS                             00074611
                  PERFORM 0200-PROCESSAR.                               00074711
                                                                        00074809
           PERFORM 0300-FINALIZAR.                                      00074909
                                                                        00075009
           STOP RUN.                                                    00075109
                                                                        00075209
      *----------------------------------------------------------------*00075309
       0000-99-FIM.                    EXIT.                            00075409
      *----------------------------------------------------------------*00075509
                                                                        00075609
      *----------------------------------------------------------------*00075709
       0100-INICIAR                    SECTION.                         00075809
      *----------------------------------------------------------------*00075909
                                                                        00076009
           ACCEPT WRK-NOTAS            FROM SYSIN.                      00076109
                                                                        00076209
      *----------------------------------------------------------------*00076309
       0100-99-FIM.                    EXIT.                            00076409
      *----------------------------------------------------------------*00076509
                                                                        00076601
      *----------------------------------------------------------------*00076709
       0200-PROCESSAR                  SECTION.                         00076809
      *----------------------------------------------------------------*00076909
                                                                        00077009
           COMPUTE WRK-MEDIA = ( WRK-NOTA1 + WRK-NOTA2 ) / 2.           00077104
               EVALUATE WRK-MEDIA                                       00077201
                   WHEN 0 THRU 1,99                                     00077308
                     MOVE 'REPROVADO '                                  00077410
                                       TO WRK-STATUS                    00077510
                   WHEN 2,00 THRU 5,99                                  00077608
                     MOVE 'RECUPERACAO'                                 00077710
                                       TO WRK-STATUS                    00077810
                   WHEN 6,00 THRU 8,99                                  00077908
                     MOVE 'APROVADO'                                    00078010
                                       TO WRK-STATUS                    00078110
                   WHEN 9,00 THRU 10,00                                 00078208
                     MOVE 'NAO FEZ MAIS QUE A OBRIGACAO '               00078301
                                       TO WRK-STATUS                    00078409
                   WHEN OTHER                                           00078503
                     MOVE '***ALGO DE ERRADO NAO ESTA CERTO!!***'       00078604
                                       TO WRK-STATUS                    00078709
                END-EVALUATE.                                           00078801
                                                                        00078901
      *----------------------------------------------------------------*00079009
       0200-99-FIM.                    EXIT.                            00079109
      *----------------------------------------------------------------*00079209
                                                                        00079309
      *----------------------------------------------------------------*00079409
       0300-FINALIZAR                  SECTION.                         00079509
      *----------------------------------------------------------------*00079609
                                                                        00079709
            DISPLAY 'MEDIA....' WRK-MEDIA.                              00079806
            DISPLAY 'STATUS...' WRK-STATUS.                             00079906
                                                                        00080001
      *----------------------------------------------------------------*00080109
       0300-99-FIM.                    EXIT.                            00080209
      *----------------------------------------------------------------*00080309
      *----------------------------------------------------------------*00080401
       9000-TRATAR-ERRO                SECTION.                         00080501
      *----------------------------------------------------------------*00080601
                                                                        00080709
      *----------------------------------------------------------------*00080801
       9000-99-FIM.                    EXIT.                            00081001
      *----------------------------------------------------------------*00090001
