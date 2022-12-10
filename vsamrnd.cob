//HERC01N  JOB (COBOL),'VSAMRND',CLASS=A,MSGCLASS=H,                    00000101
//             REGION=8M,TIME=1440,                                     00000200
//             MSGLEVEL=(1,1)                                           00000300
//VSAMRND  EXEC COBUCG,                                                 00000405
//             PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'        00000500
//COB.SYSPUNCH DD DUMMY                                                 00000600
//COB.SYSIN DD *                                                        00000700
      ********************************************************          00000800
      * A COBOL PROGRAM TEMPLATE                                        00000900
      ********************************************************          00001000
       IDENTIFICATION DIVISION.                                         00001100
       PROGRAM-ID. VSAMRND.                                             00001205
       AUTHOR. KRIS W KEENER.                                           00001300
       INSTALLATION.   THE LAB.                                         00001400
       DATE-WRITTEN.   DECEMBER 04 2021.                                00001500
       DATE-COMPILED.  DECEMBER 04 2021.                                00001600
       SECURITY. HOME USE ONLY.                                         00001700
       REMARKS. AN EXAMPLE PROGRAM DEMONSTRATING RANDOM                 00001801
           VSAM FILE ACCESS.                                            00001900
      *>                                                                00002000
      *>                                                                00002100
       ENVIRONMENT DIVISION.                                            00002200
      **                                                                00002300
       CONFIGURATION SECTION.                                           00002400
       SOURCE-COMPUTER.    IBM-370.                                     00002500
       OBJECT-COMPUTER.    IBM-370.                                     00002600
      **                                                                00002700
       INPUT-OUTPUT SECTION.                                            00002800
      *                                                                 00002900
       FILE-CONTROL.                                                    00003000
           SELECT VALTRAN  ASSIGN TO UT-S-VALTRAN.                      00003100
           SELECT INVMAST  ASSIGN TO UT-I-INVMAST                       00003200
                           ORGANIZATION IS INDEXED                      00003300
                           ACCESS IS RANDOM                             00003403
                           RECORD KEY IS MR-ITEM-NO                     00003500
                           FILE STATUS IS INVMAST-ERROR-CODE.           00003600
           SELECT ERRTRAN  ASSIGN TO UT-S-ERRTRAN.                      00003700
      *>                                                                00003800
       DATA DIVISION.                                                   00003900
      *                                                                 00004000
       FILE SECTION.                                                    00004100
      *                                                                 00004200
       FD  VALTRAN                                                      00004300
           LABEL RECORDS ARE STANDARD                                   00004400
           RECORD CONTAINS 21 CHARACTERS.                               00004500
      *                                                                 00004600
       01  VALID-TRANSACTION-AREA           PIC X(21).                  00004700
      *                                                                 00004800
       FD  INVMAST                                                      00004900
           LABEL RECORDS ARE STANDARD                                   00005000
           RECORD CONTAINS 50 CHARACTERS.                               00005100
      *                                                                 00005200
       01  MASTER-RECORD-AREA                                           00005300
           05  MR-ITEM-NO                  PIC X(5)                     00005400
           05  FILLER                      PIC X(45).                   00005500
      *                                                                 00005600
       FD  ERRTRAN                                                      00005700
           LABEL RECORDS ARE STANDARD                                   00005800
           RECORD CONTAINS 21 CHARACTERS   PIC X(21).                   00005900
      *                                                                 00006000
       01  ERROR-TRANSACTION               PIC X(21).                   00006100
      *                                                                 00006200
       WORKING-STORAGE SECTION.                                         00006300
      *                                                                 00006400
       01  SWITCHES.                                                    00006500
           05  VALTRAN-EOF-SWITCH              PIC X   VALUE   'N'.     00006603
               88  VALTRAN-EOF                         VALUE   'Y'.     00006703
           05  MASTER-FOUND-SWITCH             PIC X   VALUE   'N'.     00006803
               88  MASTER-FOUND                        VALUE   'Y'.     00006903
      *                                                                 00007000
       01  FILE-STATUS-FIELD.                                           00007100
           05 INVMAST-ERROR-CODE           PIC XX.                      00007200
      *                                                                 00007300
       01  INVENTORY-TRANSACTION-RECORD.                                00007400
           05  IT-ITEM-NO                  PIC X(5).                    00007500
           05  IT-VENDOR-NO                PIC X(5).                    00007600
           05  IT-RECEIPT-DATE             PIC X(6).                    00007700
           05  IT-RECEIPT-QUANTITY         PIC S9(5).                   00007800
      *                                                                 00007900
       01  INVENTORY-MASTER-RECORD.                                     00008000
           05  IM-DESCRIPTIVE-DATA.                                     00008100
               10  IM-ITEM-NO              PIC X(5).                    00008200
               10  IM-ITEM-DESC            PIC X(20).                   00008300
               10  IM-UNIT-COST            PIC S999V99.                 00008400
               10  IM-UNIT-PRICE           PIC S999V99.                 00008500
           05  IM-INVENTORY-DATA.                                       00008600
               10  IM-REORDER-POINT        PIC S9(5).                   00008700
               10  IM-ON-HAND              PIC S9(5).                   00008800
               10  IM-ON-ORDER             PIC S9(5).                   00008900
      *>                                                                00009000
       PROCEDURE DIVISION.                                              00009100
      *                                                                 00009200
       000-UPDATE-INVENTORY-FILE.                                       00009300
           OPEN    INPUT   VALTRAN                                      00009400
                   I-O     INVMAST.                                     00009503
                   OUTPUT  ERRTRAN.                                     00009603
           PERFORM 300-PROCESS-INVENTORY-TRAN                           00009700
               UNTIL VALTRAN-EOF                                        00009803
           CLOSE   VALTRAN, INVMAST, ERRTRAN.                           00009900
           DISPLAY 'VSAMRND    1   1   NORMAL EOJ'.                     00010003
           STOP RUN.                                                    00010100
          *                                                             00010200
           300-PROCESS-INVENTORY-TRAN.                                  00010300
               PERFORM 310-READ-INVENTORY-TRAN.                         00010400
                   IF NOT VALTRAN-EOF                                   00010503
                       PERFORM 320-GET-INVENTORY-MASTER                 00010603
                       IF MASTER-FOUND                                  00010703
                           PERFORM 330-UPDATE-INVENTORY-MASTER          00010803
                           PERFORM 340-REWRITE-INVENTORY-MASTER         00010903
               ELSE                                                     00011000
                   PERFORM 350-WRITE-ERROR-TRAN.                        00011103
                       PERFORM 350-UPDATE-INVENTORY-MASTER              00011200
                   ELSE                                                 00011300
                       PERFORM 360-WRITE-ERROR-TRAN.                    00011400
          *                                                             00011500
           310-READ-INVENTORY-TRAN.                                     00011600
               READ VALTRAN INTO INVENTORY-TRANSACTION-RECORD           00011700
                   AT  END                                              00011800
                       MOVE 'Y' TO VALTRAN-EOF-SWITCH.                  00011903
          *                                                             00012000
           320-READ-INVENTORY-MASTER.                                   00012103
               MOVE    IT-ITEM-NO TO MR-ITEM-NO.                        00012204
               READ    INVMAST INTO INVENTORY-MASTER-RECORD.            00012304
               IF      INVMAST-ERROR-COD = '00'                         00012404
                   MOVE 'Y' TO MASTER-FOUND-SWITCH                      00012504
               ELSE                                                     00012600
                   MOVE 'N' TO MASTER-FOUND-SWITCH.                     00012704
          *                                                             00012800
           330-UPDATE-INVENTORY-MASTER.                                 00012904
               ADD IT-RECEIPT-QUANTITY TO IM-ON-HAND.                   00013004
          *                                                             00013104
           340-REWRITE-INVENTORY-MASTER.                                00013200
               REWRITE MASTER-RECORD-AREA FROM                          00013300
                   INVENTORY-MASTER-RECORD.                             00013400
               IF INVMAST-ERROR-CODE   NOT =   '00'                     00013500
                  DISPLAY 'VSAMRND   A 2   REWRITE ERROR FOR INVMAST'   00013604
                  DISPLAY 'VSAMRND   A 2   ITEM NUMBER = ' IM-ITEM-NO.  00013704
                  DISPLAY 'VSAMRND   A 2   FILE STATUS = '              00013804
                       INVMAST-ERROR-CODE                               00013900
                  MOVE 'Y' TO VALTRAN-EOF-SWITCH.                       00014004
          *                                                             00014100
           350-WRITE-ERROR-TRAN.                                        00014204
               WRITE ERROR-TRANSACTION FROM                             00014300
                   INVENTORY-TRANSACTION-RECORD.                        00014400
      *>                                                                00014500
/*                                                                      00014600
//COB.SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                             00014700
//COB.VALTRAN DD DSNAME=HERC01.SAMPLE.VALTRAN,DSP=SWR                   00014800
//COB.INVMAST DD DSNAME=HERC01.SAMPLE.INVMAST,DSP=SWR                   00014900
//COB.ERRTRAN DD DSNAME=HERC01.SAMPLE.ERRTRAN,DSP=SWR                   00015000
//GO.SYSOUT DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)         00015100
//GO.SYSIN DD *                                                         00015200
/*                                                                      00015300
/&                                                                      00015400
