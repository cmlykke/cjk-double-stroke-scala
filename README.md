
劉碼輸入法 / 刘码输入法 / Liuma input method
main repository: https://github.com/Weiqifan1/rime-liuma
code repository: https://github.com/Weiqifan1/cjk-double-stroke-scala

author: - 劉可力 / 刘可力 c m l y k k e - h o t m a i l - c o m
description:
Version: 1.2 - 2024-11-04
First published 2024-10-16
劉碼 / 刘码 Liuma is a shape-based input system that you can memorize in a few minutes.
It contains 29.512 different single characters, and 179.799 multi-character words
(counting traditional and simplified words separately).
You can write any of the 5.000 most common characters without having to scroll,
using only 4 letters per character (plus selection using the number keys).
It comes in to versions:
劉碼繁 liumafan where traditional characters are prioritized, and
刘码简 liumajian where simplified characters are prioritized.
Read more at https://github.com/Weiqifan1/rime-liuma/blob/main/README.md

This project is written in Scala-3 and generates .yaml files to be used with the 
RIME input method engine: 
https://rime.im/
https://github.com/rime

To generate the needed files, uncomment the 
test code in this file:
https://github.com/Weiqifan1/cjk-double-stroke-scala/tree/master/src/test/scala/GenerateOutput
and run it. 
This will generate a number of .yaml files into the GenerateOutput folder
Take the matching .dict.yaml and .schema.yaml files and paste it 
into you local rime folder. Then edit your preexisting default.custom.yaml
file as needed.

