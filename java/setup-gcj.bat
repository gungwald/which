rem @echo off

rem Setup for gcj
set JAVA_HOME=C:\opt\thisiscool-gcc\gcc-3.4

rem Add to path if it is not already there.
echo %PATH:~0,64% | findstr /l %JAVA_HOME% 
if not %ERRORLEVEL%==0 set PATH=%JAVA_HOME%\bin;%PATH%
