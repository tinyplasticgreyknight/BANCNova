RAW 31521, 10001, 700, 108
SYSCALL @1521
SET @1521, SUBSTR(@1521)
STARTCOND @1276 == 1
SET @1563, ADD(20)
RAW 31563, 10001, 50, 209
ENDCOND
STARTCOND @220 == 0
DATAGET1L @222, @222
DATAGET1N @220, @1489
SET @221, DATE(@222), ADD(@1489)
SET @220, ADD(1)
ENDCOND
COND @221 == 0
SET @221, ADD(@10)
SET @290, ADD(1)
STARTCOND ISNULL(@161)
SET @1543, ADD(@10)
SET @1529, LEN(@10)
STARTCOND @1529 == 8
SET @1540, SYSTEM(@1543), SUBSTR(2), ADD(1)
SET @1541, SYSTEM(@1543), SUBSTR(3), ADD(2)
SET @1542, SYSTEM(@1543), SUBSTR(5), ADD(4)
ENDCOND
STARTCOND ISNULL(@161)
STARTCOND @1529 == 9
SET @1540, SYSTEM(@1543), SUBSTR(2), ADD(2)
SET @1541, SYSTEM(@1543), SUBSTR(4), ADD(2)
SET @1542, SYSTEM(@1543), SUBSTR(6), ADD(4)
ENDCOND
STARTCOND ISNULL(@161)
SET @161, SUB(@1541), SUB(@1540), SUB(@1542)
SET @1539, SYSTEM(@1539), SYSTEM(0)
SET @1529, LEN(@1539)
STARTCOND @1529 == 8
SET @1540, SYSTEM(@1539), SUBSTR(1), ADD(2)
SET @1541, SYSTEM(@1539), SUBSTR(4), ADD(2)
SET @1542, SYSTEM(@1539), SUBSTR(7), ADD(2)
ENDCOND
STARTCOND ISNULL(@161)
STARTCOND @1529 == 7
SET @1540, SYSTEM(@1539), SUBSTR(1), ADD(1)
SET @1541, SYSTEM(@1539), SUBSTR(3), ADD(2)
SET @1542, SYSTEM(@1539), SUBSTR(6), ADD(2)
ENDCOND
SET @1543, SUB(@1540), SUB(@1541), SUB(@1542)
SET @1530, LEN(@161)
COND @1530 < 9
SET @161, ADD(@161), SUB(@1543)
COND @1550 == 0
RAW 7900, 18112, 101, 0
VIDEOMODE 3
SET @1538, SYSTEM(@1612), SUBSTR(6), ADD(1)
SET @1267, ADD(@1410), SUB(@1538)
SET @1550, ADD(1)
SET @1534, ADD(1)
STARTCOND @1276 != 1
DATAGET0N @201, @201
DATAGET1L @202, @202
DATAGET1L @203, @203
DATAGET1L @204, @204
DATAGET1L @209, @209
DATAGET1N @1543, @1543
DATAGET1N @1544, @1544
SET @204, ADD(@204), ADD(@1543), ADD(@1544)
SET @1543, SUBSTR(@1543)
SET @1544, SUBSTR(@1544)
DATAGET1L @205, @205
DATAGET1L @206, @206
DATAGET1L @207, @207
DATAGET1N @208, @208
SET @1530
DATAPUT1L @1248, @1530
ENDCOND
RAW 8607, 0, 0, 1
SET @1521, SUBSTR(@1521)
SET @1521, SYSTEM(@784), SUBSTR(2), ADD(1)
RAW 31521, 10001, 30, 109
RAW 31521, 10001, 641, 108
RAW 31521, 10001, 30, 109
RAW 16200, 10, 0, 0
STARTCOND @160 < 2
SAVEADDR
RAW 8550, 100, 4301, 1
ENDCOND
RAW 16900, 0, 0, 0
SHOW @1600, 201, , -201
NEWPAGE 1
SHOW @1605, 223, , 223
SHOW @1641, 334, , 334
SHOW @351, 603, 15, 614
RCOND ISNOTNULL(@351)
SHOW @352, 630, 10, 637
SHOW @353, 648, 21, 653
RCOND ISNOTNULL(@353)
SHOW @1604, 1003, , 1003
SHOW @563, 1103, , 1103
SHOW @367, , 11, 1117
SET @1528, LEN(@367)
COND ISNOTNULL(@367)
RCOND @1528 < 11
SET @1528
SET @910, ADD(@367)
SHOW @562, 1503, , 1503
SHOW @368, , 12, 1526
SET @1529, SYSTEM(@368), SUBSTR(2), ADD(2)
RCOND @1529 != 0
SET @1533, DATE(@368), MUL(@10)
RCOND @1533 >= 0
SET @1546, ADD(@368)
SET @1529, LEN(@1546)
STARTCOND @1529 == 9
SET @1538, SYSTEM(@1546), SUBSTR(6), ADD(4)
SET @1539, SYSTEM(@1546), SUBSTR(2), ADD(2)
SET @1540, SYSTEM(@1546), SUBSTR(4), ADD(2)
ENDCOND
STARTCOND @1529 == 8
SET @1538, SYSTEM(@1546), SUBSTR(5), ADD(4)
SET @1539, SYSTEM(@1546), SUBSTR(2), ADD(1)
SET @1531, ADD(@1539)
SET @1531, ADD(@1531), ADD(100)
SET @1539, ADD(@1531)
SET @1539, SYSTEM(@1539), SUBSTR(3), ADD(2)
SET @1540, SYSTEM(@1546), SUBSTR(3), ADD(2)
ENDCOND
SET @1530, ADD(1000), ADD(900), SUB(@1538)
COND @1530 >= 0
SET @1538, ADD(1000), ADD(900)
SET @1541, SYSTEM(@1538), SUBSTR(3), ADD(2)
SET @1544, SUB(@1541), SUB(@1539), SUB(@1540)
SHOW @1315, 1903, 1, 1928
COND @1315 != 'Y'
RCOND @1315 == 'N'
SET @1547, SUBSTR(@1547)
STARTCOND @1315 == 'Y'
STARTCOND @1261 == 0
SET @1256, ADD(1)
SET @1280
ENDCOND
SET @1547, SUBSTR(@1547)
STARTCOND @1315 == 'N'
SET @1256
SET @1280, ADD(1)
ENDCOND
SHOW @1547, , 1, 2301
SET @1529
DATAGET1L @1261, @1261
COND @1276 != 1
COND @1261 == 1
COND @1315 == 'Y'
GOTO 7
GOTO 10
SHOW @1547, , 1, -2301
WINDOW 23, 101, 1380
SHOW @1603, 226, , 226
SET @354, ADD(@351), ADD(@352), ADD(@353)
SHOW @354, , 1, 420
SET @350, ADD(@354)
NEWPAGE 1
SET @1530
STARTCOND ISNULL(@355)
SHOW @360, 703, , 703
COND ISNULL(@355)
SHOW @355, , 40, 711
RCOND ISNOTNULL(@355)
SHOW @356, , 40, 811
ENDCOND
SHOW @362, 905, , 905
COND ISNULL(@357)
SHOW @357, , 25, 911
SHOW @363, 938, , 938
COND ISNULL(@358)
SHOW @358, , 15, 944
SHOW @364, 960, , 960
COND ISNULL(@359)
SHOW @359, , 9, 964
RCOND ISNOTNULL(@359)
SET @1528
SHOW @378, 1011, , 1011
SHOW @379, 1044, , 1044
STARTCOND @366 < '0'
SHOW @366, , 12, 1025
SET @1529
SET @1529, LEN(@366)
RCOND @1529 > 7
RCOND @1529 < 12
ENDCOND
STARTCOND @365 < '0'
SHOW @365, , 12, 1055
SET @1529
SET @1530
SET @1533
SET @1529, LEN(@365)
RCOND @1529 <= 11
STARTCOND @1529 < 8
COND @1529 > 2
SET @1530, ADD(1)
COND @1529 == 2
SET @1533, ADD(@365)
COND @1533 != 0
SET @1530, ADD(1)
SET @1547, SUBSTR(@1547)
RCOND @1530 == 0
ENDCOND
SHOW @376, 1203, 1, 1211
COND @376 == 'Y'
SET @1528, ADD(1)
COND @376 == 'N'
SET @1528, ADD(2)
RCOND @1528 != 0
SET @1547, SUBSTR(@1547)
SET @1563, ADD(20)
RAW 31563, 10001, 50, 209
COND @376 == 'N'
GOTO 6
ENDCOND
SHOW @1547, , 1, 2301
SET @1528, ADD(3)
DATAPUT1N @1528, @1528
RAW 31568, 10001, 800, 107
SYSCALL @1568
STARTCOND @1261 == 1
COND @1256 == 1
COND ISNOTNULL(@1321)
RAW 8550, 101, 4201, 1
ENDCOND
GOTO 4
SHOW @1547, , 1, -2301
NEWPAGE 1
SET @1547, SUBSTR(@1547)
SHOW @1581, 226, , 226
SET @354, ADD(@351), ADD(@352), ADD(@353)
SHOW @354, , 1, 420
SET @1570, SUBSTR(@1570)
NEWPAGE 1
SHOW @313, 606, -1, 604
RAW 31570, 313, 336, 902
RCOND ISNOTNULL(@1570)
SHOW @261, , 1, 804
SHOW @1698, 806, , 806
COND ISNOTNULL(@261)
RCOND @261 == 'X'
SET @1547, SUBSTR(@1547)
STARTCOND ISNOTNULL(@261)
SHOW @262, , 1, 911
SHOW @1615, 913, , 913
COND ISNOTNULL(@262)
RCOND @262 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @271, , 1, 943
SHOW @1653, 945, , 945
COND ISNOTNULL(@271)
RCOND @271 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @281, 1013, 1, 1011
COND ISNOTNULL(@281)
RCOND @281 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @802, 1045, 1, 1043
COND ISNOTNULL(@802)
RCOND @802 == 'X'
ENDCOND
SET @1547, SUBSTR(@1547)
SHOW @265, , 1, 1204
SHOW @280, 1206, , 1206
COND ISNOTNULL(@265)
RCOND @265 == 'X'
SET @1547, SUBSTR(@1547)
STARTCOND ISNOTNULL(@265)
SHOW @263, , 1, 1311
SHOW @1727, 1313, , 1313
COND ISNOTNULL(@263)
RCOND @263 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @266, , 1, 1343
SHOW @1730, 1345, , 1345
COND ISNOTNULL(@266)
RCOND @266 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @282, 1445, 1, 1443
COND ISNOTNULL(@282)
RCOND @282 == 'X'
ENDCOND
SET @1547, SUBSTR(@1547)
SHOW @283, 1506, 1, 1504
COND ISNOTNULL(@283)
RCOND @283 == 'X'
SET @1547, SUBSTR(@1547)
STARTCOND ISNOTNULL(@283)
SHOW @272, , 1, 1611
SHOW @1731, 1613, , 1613
COND ISNOTNULL(@272)
RCOND @272 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @284, 1645, 1, 1643
COND ISNOTNULL(@284)
RCOND @284 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @285, 1713, 1, 1711
COND ISNOTNULL(@285)
RCOND @285 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @267, , 1, 1743
SHOW @1732, 1745, , 1745
COND ISNOTNULL(@267)
RCOND @267 == 'X'
ENDCOND
SET @1547, SUBSTR(@1547)
SHOW @286, 1906, 1, 1904
COND ISNOTNULL(@286)
RCOND @286 == 'X'
SET @1547, SUBSTR(@1547)
STARTCOND ISNOTNULL(@286)
SHOW @287, 2013, 1, 2011
COND ISNOTNULL(@287)
RCOND @287 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @274, , 1, 2043
SHOW @1733, 2045, , 2045
COND ISNOTNULL(@274)
RCOND @274 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @288, 2113, 1, 2111
COND ISNOTNULL(@288)
RCOND @288 == 'X'
SET @1547, SUBSTR(@1547)
SHOW @289, 2145, 1, 2143
COND ISNOTNULL(@289)
RCOND @289 == 'X'
ENDCOND
SET @1547, SUBSTR(@1547)
SHOW @307, 2304, 50, 2315
COND ISNOTNULL(@307)
SHOW @308, , 59, 2406
SHOW @1547, , 1, -2403
NEWPAGE 1
SET @354, ADD(@351), ADD(@352), ADD(@353)
SHOW @354, , 1, 419
NEWPAGE 1
SHOW @1734, 230, , 230
SHOW @1735, 704, , 704
SHOW @372, , -1, 721
RAW 31521, 372, 136, 702
RCOND @372 < 8
SHOW @301, 904, 1, 943
SET @1528
STARTCOND ISNOTNULL(@301)
COND @301 == 'Y'
SET @1528, ADD(1)
COND @301 == 'N'
SET @1528, ADD(1)
COND ISNULL(@301)
SET @1528, ADD(1)
RCOND @1528 == 1
SHOW @307, 1108, , 1108
SHOW @310, , 59, 1118
ENDCOND
SHOW @302, 1404, 1, 1446
SET @1528
STARTCOND ISNOTNULL(@302)
COND @302 == 'Y'
SET @1528, ADD(1)
COND @302 == 'N'
SET @1528, ADD(1)
COND ISNULL(@302)
SET @1528, ADD(1)
RCOND @1528 == 1
SHOW @307, 1608, , 1608
SHOW @311, , 59, 1618
ENDCOND
SHOW @303, 1904, 25, 1923
STARTCOND ISNOTNULL(@303)
SHOW @304, , 25, 2023
STARTCOND ISNOTNULL(@304)
SHOW @305, , 25, 2123
STARTCOND ISNOTNULL(@305)
SHOW @306, , 25, 2223
ENDCOND
SHOW @1547, , 1, 2303
GOTOF 1
SHOW @1547, , 1, -2303
WINDOW 48, 1301, 2272
SHOW @1642, 1415, , 1415
NEWPAGE 1
SHOW @360, 1603, 40, 1611
SHOW @361, , 40, 1711
SHOW @362, 1805, 25, 1811
SHOW @363, 1904, 15, 1911
SHOW @364, 1927, 9, 1931
SHOW @366, , 12, 2029
SET @1529
SET @1530
SET @1533
SET @1529, LEN(@366)
RCOND @1529 <= 11
STARTCOND @1529 < 8
COND @1529 > 2
SET @1530, ADD(1)
COND @1529 == 2
SET @1533, ADD(@366)
COND @1533 != 0
SET @1530, ADD(1)
ENDCOND
RCOND @1530 == 0
SHOW @365, , 12, 2059
SET @1529
SET @1530
SET @1533
SET @1529, LEN(@365)
RCOND @1529 <= 11
STARTCOND @1529 < 8
COND @1529 > 2
SET @1530, ADD(1)
COND @1529 == 2
SET @1533, ADD(@365)
COND @1533 != 0
SET @1530, ADD(1)
ENDCOND
RCOND @1530 == 0
SHOW @1547, , 1, 2201
SET @1547, SUBSTR(@1547)
SHOW @1699, 2011, , 2011
SHOW @378, 2015, , 2015
SHOW @1699, 2044, , 2044
SHOW @379, 2048, , 2048
SET @1528, ADD(3)
DATAPUT1N @1528, @1528
RAW 31568, 10001, 800, 107
SYSCALL @1568
STARTCOND @1261 == 1
COND @1256 == 1
COND ISNOTNULL(@1321)
RAW 8550, 101, 4201, 1
ENDCOND
GOTO 4
SHOW @1547, , 1, -2201
RAW 7100, 18812, 101, 0
VIDEOMODE 3
SET @1545, SUBSTR(@1545)
DATAPUT1N @1545, @1545
DATAGET1L @1261, @1261
STARTCOND @1261 == 1
SET @354, ADD(@351), ADD(@352), ADD(@353)
DATAPUT1L @354, @354
SET @1528
DATAPUT1L @1268, @1528
DATAPUT1L @367, @367
DATAPUT1N @1604, @367
DATAPUT1L @368, @1544
DATAPUT1N @368, @1544
SET @381, SYSTEM(@351), SUBSTR(1), ADD(1)
SET @381, ADD(@381), ADD(@353)
DATAPUT1L @381, @381
COND @1534 == 1
RAW 31543, 10001, 851, 107
COND @1534 > 1
RAW 31543, 10001, 852, 107
DATARUN0N 0, @1543
COND @171 > 1
RAW 8550, 505, 3401, 1
STARTCOND @171 == 1
SET @1261
DATAPUT1L @1261, @1261
SET @1529
GOTO 10
ENDCOND
STARTCOND @1261 == 1
DATAGET1N @1545, @1545
RAW 31532, 1545, 801, 401
STARTCOND @1532 < 5
COND @1532 == 1
SET @1534, ADD(2)
COND @1532 == 1
GOTO 11
COND @1532 > 1
RAW 8550, 505, 3401, 1
ENDCOND
SET @1528
SET @1529
STARTCOND @1261 == 1
DATAGET1L @1268, @1268
STARTCOND @1268 == 0
SET @1256
SET @1280, ADD(1)
GOTO 10
ENDCOND
COND @1261 != 1
GOTO 10
DATAGET1L @355, @355
DATAGET1L @1400, @1400
DATAGET1L @1401, @1401
DATAPUT1N @522, @367
DATAPUT1N @1604, @367
DATAPUT1L @488, @1544
DATAGET1L @370, @370
SET @1566, SUBSTR(@1566)
DATAGET1L @365, @1546
SET @1537, ADD(1)
ENDRCOND
SET @1538, SYSTEM(@1546), SUBSTR(@1537), ADD(1)
COND @1538 != '-'
COND @1538 != ' '
SET @1566, ADD(@1566), SUB(@1538)
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 12
SET @365, ADD(@1566)
SET @1566, SUBSTR(@1566)
DATAGET1L @366, @1546
SET @1537, ADD(1)
ENDRCOND
SET @1538, SYSTEM(@1546), SUBSTR(@1537), ADD(1)
COND @1538 != '-'
COND @1538 != ' '
SET @1566, ADD(@1566), SUB(@1538)
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 12
SET @366, ADD(@1566)
DATAGET1L @1270, @1270
DATAGET1L @1269, @1269
DATAGET1L @1244, @1244
SET @1537, ADD(999), ADD(322)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1327
SET @1537, ADD(999), ADD(330)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1335
SET @1537, ADD(999), ADD(338)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1343
SET @1537, ADD(999), ADD(346)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1351
SET @1537, ADD(999), ADD(354)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1359
SET @1537, ADD(999), ADD(362)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1367
SET @1537, ADD(999), ADD(370)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1375
SET @1537, ADD(999), ADD(378)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1383
SET @1537, ADD(999), ADD(386)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1391
SET @1537, ADD(999), ADD(394)
ENDRCOND
DATAGET1L 1537, 1537
SET @1537, ADD(@1537), ADD(1)
STARTRCOND @1537 > 1399
STARTCOND @1268 == 2
SET @1256, ADD(1)
SET @1538, SYSTEM(@1267), SUBSTR(1), ADD(1)
SET @1267, ADD(@1538), SUB(@1411)
SET @1280
ENDCOND
STARTCOND @1268 == 1
SET @1256, ADD(1)
SET @1280
ENDCOND
SET @1532
SET @1528
SET @1529
SET @1530
SET @1538, SUBSTR(@1538)
SET @1539, SUBSTR(@1539)
SET @1531
SET @1538, SYSTEM(@1400), SUBSTR(1), ADD(1)
SET @1400, SYSTEM(@1400), SUBSTR(1), ADD(42)
SET @1528, LEN(@1400)
SET @1498, SYSTEM(@1400), SUBSTR(3), ADD(1)
COND @1538 == 'A'
SET @356, SYSTEM(@1400), SUBSTR(3), ADD(42)
ENDRCOND
STARTCOND @1538 == 'C'
SET @1540, SYSTEM(@1400), SUBSTR(@1528), ADD(1)
COND @1540 != ' '
SET @1530, ADD(@1530), ADD(1)
SET @1531, ADD(@1528), ADD(1)
SET @1528, ADD(@1528), SUB(1)
COND @1540 == ' '
SET @359, SYSTEM(@1400), SUBSTR(@1531), ADD(@1530)
STARTRCOND @1540 == ' '
SET @1530
ENDCOND
ENDRCOND
STARTCOND @1538 == 'C'
SET @1540, SYSTEM(@1400), SUBSTR(@1528), ADD(1)
COND @1540 == ' '
SET @1532, ADD(1)
COND @1540 == ','
SET @1532, ADD(2)
COND @1532 == 0
SET @1530, ADD(@1530), ADD(1)
SET @1531, ADD(@1528), ADD(1)
SET @1528, ADD(@1528), SUB(1)
COND @1532 > 0
COND @1528 >= 1
SET @358, SYSTEM(@1400), SUBSTR(@1531), ADD(@1530)
STARTRCOND @1532 > 0
SET @358, ADD(@358)
SET @1546, SYSTEM(@1400), SUBSTR(@1528), ADD(1)
STARTCOND @1498 != ' '
COND @1532 == 1
COND @1546 == ','
SET @1528, ADD(@1528), SUB(2)
STARTCOND @1528 > 1
SET @1528, ADD(@1528), SUB(1)
SET @357, SYSTEM(@1400), SUBSTR(3), ADD(@1528)
ENDCOND
SET @1528
SET @1532
SET @1529
SET @1530
SET @1538, SUBSTR(@1538)
SET @1498, SUBSTR(@1498)
SET @1539, SUBSTR(@1539)
SET @1531
SET @1539, SYSTEM(@1401), SUBSTR(1), ADD(1)
SET @1401, SYSTEM(@1401), SUBSTR(1), ADD(42)
SET @1528, LEN(@1401)
SET @1498, SYSTEM(@1401), SUBSTR(3), ADD(1)
STARTCOND @1539 == 'C'
ENDRCOND
SET @1540, SYSTEM(@1401), SUBSTR(@1528), ADD(1)
COND @1540 != ' '
SET @1530, ADD(@1530), ADD(1)
SET @1531, ADD(@1528), ADD(1)
SET @1528, ADD(@1528), SUB(1)
COND @1540 == ' '
SET @359, SYSTEM(@1401), SUBSTR(@1531), ADD(@1530)
STARTRCOND @1540 == ' '
SET @1530
ENDCOND
ENDRCOND
STARTCOND @1539 == 'C'
SET @1540, SYSTEM(@1401), SUBSTR(@1528), ADD(1)
COND @1540 == ' '
SET @1532, ADD(1)
COND @1540 == ','
SET @1532, ADD(2)
COND @1532 == 0
SET @1530, ADD(@1530), ADD(1)
SET @1531, ADD(@1528), ADD(1)
SET @1528, ADD(@1528), SUB(1)
COND @1532 > 0
COND @1528 >= 1
SET @358, SYSTEM(@1401), SUBSTR(@1531), ADD(@1530)
STARTRCOND @1532 > 0
SET @1546, SYSTEM(@1401), SUBSTR(@1528), ADD(1)
STARTCOND @1498 != ' '
COND @1532 == 1
COND @1546 == ','
SET @1528, ADD(@1528), SUB(2)
STARTCOND @1528 > 1
SET @1528, ADD(@1528), SUB(1)
SET @357, SYSTEM(@1401), SUBSTR(3), ADD(@1528)
ENDCOND
SET @1498, SUBSTR(@1498)
SET @1529
GOTO 10
SHOW @1547, , 1, -2301
WINDOW 23, 1301, 2380
SHOW @1409, 1820, , 1820
GOTO 3
SHOW @1547, , 1, -2301
WINDOW 23, 1301, 2380
SHOW @1314, 1820, , 1820
GOTO 3
SHOW @1547, , 1, -2301
COND @1276 == 0
COND @1261 == 1
COND @1315 == 'Y'
AUTOSAVE
SET @1276, ADD(1)
SET @1315, SUBSTR(@1315)
COND @1268 == 2
GOTO 8
COND @1261 != 1
GOTO 9
GOTO 3
SHOW @1547, , 1, -2301
RAW 31543, 10001, 853, 107
DATARUN0N 0, @1543
GOTO 7
SHOW @1547, , 1, -2301
