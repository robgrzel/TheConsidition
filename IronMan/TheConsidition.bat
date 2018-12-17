
resize -s 90 150

set FPATH=%~dp0
set FNAME=%~n0
set FXT=%~x0
set FNAMEXT=%~nx0


@echo off
cd /d %FPATH%

dos2unix %FNAME%.sh

rm tmp.path.txt

wsl wslpath -a '%FPATH%' > tmp.path.txt

dos2unix tmp.path.txt

set /p WSL_FPATH=<tmp.path.txt

echo %WSL_FPATH%


start bash -c "cd %WSL_FPATH%; ./TheConsidition.sh; read"
#start bash -c "cd %WSL_FPATH%; ./TheConsidition.sh"






