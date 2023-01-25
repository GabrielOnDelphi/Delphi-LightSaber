echo off
prompt $
cls

del  /s *.map
del  /s *.dsm
del  /s *.ddp
del  /s *.bak
del  /s *.drc
del  /s *.local
del  /s *.identcache
del  /s *.stat
del  /s *.~dsk

"Demo\Clean all except PAS.cmd"

delay 500 s b