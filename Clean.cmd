
@SET CURRENT_PATH=%~dp0
@CD /d %CURRENT_PATH%


@ECHO .
@ECHO CURRENT FOLDER %CURRENT_PATH%
@ECHO .

@IF "%SKIP_TIMEOUT%" NEQ "1" timeout 5

@del .\*.~* /s
@del .\*.dsk /s
@del .\*.ddp /s
@del .\*.dcu /s
@del .\*.cbk /s
@del .\*.local /s
@del .\*.identcache /s

@IF "%SKIP_TIMEOUT%" NEQ "1" timeout 5
