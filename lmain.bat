REM Link step of for aoc.exe
cd obj_janus
link  -subsystem:console -entry:mainCRTStartup -out:%1.exe %1.obj libcmt.lib kernel32.lib user32.lib -map:%1.map /NODEFAULTLIB:"libc.lib"
move %1.exe ..\bin\aoc.exe
cd ..
