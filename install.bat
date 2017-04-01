@echo off

rem TODO: optimizise backup
rem TODO: check administator privileges

set DOT_EMACS_PATH=%~dp0
set WINDOWS_HOME_PATH=%HOMEDRIVE%%HOMEPATH%\AppData\Roaming

rem change drive (C:), then cd to home path
%HOMEDRIVE%
cd %WINDOWS_HOME_PATH%

rem backup old config files
if exist .emacs.bkp (
	echo delete old backup .emacs ?
	rd /s .emacs.bkp
)

if exist .emacs (
	echo old .emacs file existed, backup ...
	move .emacs .emacs.bkp
)

if exist .emacs.d.bkp (
	echo delete old backup .emacs.d ?
	rd /s .emacs.d.bkp
)

if exist .emacs.d (
	echo old .emacs.d directory existed, backup ...
	move .emacs.d .emacs.d.bkp
)

echo link files ...
mkdir .emacs.d
mklink .emacs %DOT_EMACS_PATH%\init.el
mklink /d .emacs.d\lisp %DOT_EMACS_PATH%\lisp
mklink /d .emacs.d\plugins %DOT_EMACS_PATH%\plugins
mklink /d .emacs.d\themes %DOT_EMACS_PATH%\themes
mklink /d .emacs.d\templates %DOT_EMACS_PATH%\templates
mklink /d .emacs.d\ac-dict %DOT_EMACS_PATH%\ac-dict

:exit
echo All Done
g:
pause