
劉碼輸入法 / 刘码输入法 / Liuma input method
repository - https://github.com/Weiqifan1/cjk-double-stroke-scala

author: - 劉可力 / 刘可力 c m l y k k e - h o t m a i l - c o m
description:
Release: 1.0 - 2024-10-16
First published 2024-10-16
劉碼 / 刘码 Liuma is a shape-based input system that you can learn in 10 minutes.
It contains 28.863 different single characters, and 179.226 multi-character words.
You can write any of the 5.000 most common characters without having to scroll,
using only 4 keystrokes per character (plus selection using the number keys).
It comes in to versions:
劉碼繁 Liumafan for writing traditional characters, and
刘码简 Liumajian for writing simplified characters.

This project is written in Scala-3 and generates .yaml files to be used with the 
RIME input method engine: 
https://rime.im/
https://github.com/rime

To generate the needed files, uncomment the 
test code in this file:
https://github.com/Weiqifan1/cjk-double-stroke-scala/tree/master/src/test/scala/GenerateOutput
and run it. 
This will generate a number of .yaml files.
Take the matching .dict.yaml and .schema.yaml files and paste it 
into you local rime folder. Then edit your preexisting default.custom.yaml
file as needed.

Information about the source files used to create Liuma:

    Stoke data source and main single character source:
    https://github.com/stroke-input/stroke-input-data/
    version: v1.33.1 (2024-09-29)  retrieved 2024-10-16
    Compiled manually by Conway (@yawnoc).
    Part of 'Conway Stroke Data',
    Licensed under Creative Commons Attribution 4.0 International (CC-BY-4.0)

    IDS data source:
    https://github.com/cjkvi/cjkvi-ids/blob/master/ids.txt
    retrieved 2024-09-08
    # Copyright (c) 2014-2017 CJKVI Database
    # Based on CHISE IDS Database
    https://github.com/cjkvi/cjkvi-ids/blob/master/ids.txt

    Multi-character word vocabulary source:
    CC-CEDICT - release: 2024-10-16 06:08:17 GMT
    Number of entries: 122652
    https://www.mdbg.net/chinese/dictionary?page=cedict

    Frequency data source - single characters:
    
    simplified:
    https://lingua.mtsu.edu/chinese-computing/statistics/char/list.php?Which=MO
    Data last updated 最近更新: 2004-03-30
    
    traditional:
    http://technology.chtsai.org/charfreq/sorted.html
    Updated: 2005-08-03

    Frequency data source - multi-character words:
    
    simplified:
    The Beijing Language and Culture University corpus of 15 billion characters.
    It’s based on news (人民日报 1946-2018，人民日报海外版 2000-2018), literature (books by 472 authors,
    including a significant portion of non-Chinese writers), non-fiction books,
    blog and weibo entries as well as classical Chinese.
    retrieved 2024-10-04
    https://www.plecoforums.com/threads/word-frequency-list-based-on-a-15-billion-character-corpus-bcc-blcu-chinese-corpus.5859/
    
    traditional:
    List by K. J. Chen and the CKIP Group of the Academia Sinica
    Full list contains 10,000 highest frequency Mandarin words that are used in Taiwan."
    retrieved 2024-10-04:
    https://en.wiktionary.org/wiki/Appendix:Mandarin_Frequency_lists
