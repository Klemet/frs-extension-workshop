@echo off
setlocal enableDelayedExpansion
set "cmd=!cmdcmdline!"
if "!cmd:cmd.exe /c ""%~0"^=!^" neq "!cmd!" "%comspec%" /k ^""%~0" %*^" & exit /b
endlocal
landis-ii-7 "scenario-no-aggregation.txt"
pause