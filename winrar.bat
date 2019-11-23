@ECHO OFF
"C:\Program Files\WinRAR\Winrar.exe" a -r "c:\etc\etcbck.rar" "C:\etc"
IF %ERRORLEVEL% == 0 (
ECHO %date%, %time% : Backup succeeded >> "C:\etc\Data_Backup_Log.txt"
) ELSE (
IF %ERRORLEVEL% == 1 (
ECHO %date%, %time% : Backup succeeded with warnings >> "C:\etc\Data_Backup_Log.txt"
) ELSE (
ECHO %date%, %time% : Backup failed >> "C:\etc\Data_Backup_Log.txt"
) )
