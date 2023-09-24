echo off
prompt $
cls

//recycle.exe -f *.exe
//recycle.exe -f *.dcu

del  /s *.exe
del  /s *.dcu
del  /s *.map
del  /s *.dsm
del  /s *.ddp
del  /s *.bak
del  /s *.drc
del  /s *.local
del  /s *.identcache
del  /s *.stat
del  /s *.~dsk

delay 500 s b