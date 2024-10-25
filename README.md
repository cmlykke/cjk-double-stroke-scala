
劉碼輸入法 / 刘码输入法 / Liuma input method
main repository: https://github.com/Weiqifan1/rime-liuma
code repository: https://github.com/Weiqifan1/cjk-double-stroke-scala

author: - 劉可力 / 刘可力 c m l y k k e - h o t m a i l - c o m
description:
Release: 1.0 - 2024-10-16
First published 2024-10-16
劉碼 / 刘码 Liuma is a shape-based input method for typing chinese that you can learn in 10 minutes.
It contains 28.863 different single characters, and 179.226 multi-character words.
You can write any of the 5.000 most common characters without having to scroll,
using a maximum of 4 keystrokes per character (plus selection using the number keys).
It comes in two versions:
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
This will generate a number of .yaml files into the GenerateOutput folder
Take the matching .dict.yaml and .schema.yaml files and paste it 
into you local rime folder. Then edit your preexisting default.custom.yaml
file as needed.

The complete .yaml files for Liuma version 1.0 can be found in this folder:
cjk-double-stroke-scala\src\v1_0

To see detailed information about the Liuma input method or how to configure RIME, 
go to the folder:
cjk-double-stroke-scala\src\info