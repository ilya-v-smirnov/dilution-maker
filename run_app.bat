@echo off
rem ===========================================================================
rem run_app.bat - start Lab Dilution Planner by double-click (Windows).
rem Requires R (https://cran.r-project.org/). Uses the R version recorded in
rem renv.lock if installed, otherwise the newest installed R. Packages are
rem installed from renv.lock automatically on the first run.
rem ===========================================================================
setlocal
cd /d "%~dp0"

set "RSCRIPT="
for /f "usebackq delims=" %%i in (`powershell -NoProfile -ExecutionPolicy Bypass -File "tools\find_rscript.ps1"`) do set "RSCRIPT=%%i"

rem Fallback: Rscript on PATH
if not defined RSCRIPT (
  for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
)

if not defined RSCRIPT (
  echo R was not found. Install R from https://cran.r-project.org/ and try again.
  pause
  exit /b 1
)

echo Using "%RSCRIPT%"
echo The app opens in your browser. Close this window to stop it.
"%RSCRIPT%" run_app.R
if errorlevel 1 (
  echo.
  echo The app stopped with an error. See the messages above.
  pause
)
