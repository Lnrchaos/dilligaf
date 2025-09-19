@echo off
REM DILIGAF Installation Script for Windows
REM Installs DILIGAF as a native command in the shell

setlocal enabledelayedexpansion

REM Colors (Windows doesn't support colors in batch, so we'll use text)
set "RED=[ERROR]"
set "GREEN=[OK]"
set "YELLOW=[WARNING]"
set "BLUE=[INFO]"

REM Print banner
echo.
echo ╔══════════════════════════════════════════════════════════════╗
echo ║  ██████╗ ██╗██╗     ██╗ ██████╗  █████╗ ███████╗  ║
echo ║  ██╔══██╗██║██║     ██║██╔════╝ ██╔══██╗██╔════╝  ║
echo ║  ██║  ██║██║██║     ██║██║  ███╗███████║█████╗    ║
echo ║  ██║  ██║██║██║     ██║██║   ██║██╔══██║██╔══╝    ║
echo ║  ██████╔╝██║███████╗██║╚██████╔╝██║  ██║██║       ║
echo ║  ╚═════╝ ╚╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝       ║
echo ║                                                      ║
echo ║  DILIGAF Installation System                          ║
echo ║  Embedding DILIGAF into your shell                    ║
echo ╚══════════════════════════════════════════════════════╝
echo.

REM Check if SBCL is installed
where sbcl >nul 2>&1
if %errorlevel% neq 0 (
    echo %RED% Error: SBCL (Steel Bank Common Lisp) is not installed.
    echo Please install SBCL first:
    echo   Download from https://www.sbcl.org/
    echo   Or use Chocolatey: choco install sbcl
    pause
    exit /b 1
)
echo %GREEN% SBCL found

REM Detect shell
if "%COMSPEC%"=="%SystemRoot%\system32\cmd.exe" (
    set "SHELL=cmd"
) else if "%COMSPEC%"=="%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" (
    set "SHELL=powershell"
) else (
    set "SHELL=cmd"
)
echo %GREEN% Detected shell: %SHELL%

REM Build DILIGAF if needed
if not exist "diligaf-native.exe" (
    echo %YELLOW% Building DILIGAF...
    if exist "build-diligaf.lisp" (
        sbcl --script build-diligaf.lisp
    ) else (
        echo %RED% Error: build-diligaf.lisp not found
        pause
        exit /b 1
    )
)
echo %GREEN% DILIGAF built

REM Create DILIGAF directory
set "DILIGAF_DIR=%USERPROFILE%\.diligaf"
if not exist "%DILIGAF_DIR%" mkdir "%DILIGAF_DIR%"
echo %GREEN% DILIGAF directory created: %DILIGAF_DIR%

REM Create DILIGAF wrapper script
echo Creating DILIGAF wrapper script...
(
echo @echo off
echo REM DILIGAF Wrapper Script
echo REM This script embeds DILIGAF into the shell
echo.
echo set "DILIGAF_DIR=%%USERPROFILE%%\.diligaf"
echo set "DILIGAF_EXECUTABLE=%%DILIGAF_DIR%%\diligaf-native.exe"
echo.
echo REM Function to run DILIGAF
echo :run_diligaf
echo if exist "%%DILIGAF_EXECUTABLE%%" ^(
echo     "%%DILIGAF_EXECUTABLE%%" %%*
echo ^) else ^(
echo     echo DILIGAF not found. Please run 'diligaf-install' first.
echo     exit /b 1
echo ^)
echo goto :eof
echo.
echo REM Function to show DILIGAF help
echo :show_help
echo echo DILIGAF - Direct Interface for Low-level Intrusion and General-purpose Attack Framework
echo echo Usage: diligaf [command] [options] [file]
echo echo.
echo echo Commands:
echo echo   --repl, -r          Start DILIGAF REPL
echo echo   --compile, -c       Compile DILIGAF file to native
echo echo   --run, -r           Run DILIGAF file
echo echo   --help, -h          Show this help
echo echo   --version, -v       Show version
echo echo   --install           Install DILIGAF
echo echo   --uninstall         Uninstall DILIGAF
echo echo.
echo echo Examples:
echo echo   diligaf --repl                    # Start REPL
echo echo   diligaf --compile myfile.diligaf  # Compile to native
echo echo   diligaf myfile.diligaf            # Run file
echo echo   diligaf --version                 # Show version
echo goto :eof
echo.
echo REM Function to show version
echo :show_version
echo echo DILIGAF v1.0.0 - Self-Hosting Hacker Language
echo echo Built: %date% %time%
echo echo Architecture: %PROCESSOR_ARCHITECTURE%
echo echo OS: Windows
echo goto :eof
echo.
echo REM Function to install DILIGAF
echo :install_diligaf
echo echo Installing DILIGAF...
echo if not exist "%%DILIGAF_DIR%%" mkdir "%%DILIGAF_DIR%%"
echo if exist "diligaf-native.exe" ^(
echo     copy diligaf-native.exe "%%DILIGAF_EXECUTABLE%%"
echo     echo ✓ DILIGAF executable installed
echo ^) else ^(
echo     echo ✗ DILIGAF executable not found. Please build DILIGAF first.
echo     exit /b 1
echo ^)
echo echo DILIGAF installation complete!
echo echo You can now use 'diligaf' command anywhere.
echo goto :eof
echo.
echo REM Function to uninstall DILIGAF
echo :uninstall_diligaf
echo echo Uninstalling DILIGAF...
echo rmdir /s /q "%%DILIGAF_DIR%%"
echo echo ✓ DILIGAF uninstalled
echo goto :eof
echo.
echo REM Main command processing
echo if "%%1"=="--repl" goto :run_diligaf
echo if "%%1"=="-r" goto :run_diligaf
echo if "%%1"=="--compile" goto :compile
echo if "%%1"=="-c" goto :compile
echo if "%%1"=="--run" goto :run
echo if "%%1"=="-r" goto :run
echo if "%%1"=="--help" goto :show_help
echo if "%%1"=="-h" goto :show_help
echo if "%%1"=="--version" goto :show_version
echo if "%%1"=="-v" goto :show_version
echo if "%%1"=="--install" goto :install_diligaf
echo if "%%1"=="--uninstall" goto :uninstall_diligaf
echo if "%%1"=="" goto :show_help
echo if "%%1"=="--*" ^(
echo     echo Unknown option: %%1
echo     goto :show_help
echo     exit /b 1
echo ^)
echo.
echo REM If it's a file, run it
echo if exist "%%1" ^(
echo     call :run_diligaf --script "%%1"
echo ^) else ^(
echo     echo Error: File not found: %%1
echo     exit /b 1
echo ^)
echo goto :eof
echo.
echo :compile
echo if "%%2"=="" ^(
echo     echo Error: No file specified for compilation
echo     exit /b 1
echo ^)
echo call :run_diligaf --compile "%%2" --output "%%~n2"
echo goto :eof
echo.
echo :run
echo if "%%2"=="" ^(
echo     echo Error: No file specified for execution
echo     exit /b 1
echo ^)
echo call :run_diligaf --script "%%2"
echo goto :eof
) > "%DILIGAF_DIR%\diligaf.bat"
echo %GREEN% Wrapper script created

REM Copy DILIGAF files
echo Copying DILIGAF files...
if exist "diligaf-native.exe" (
    copy "diligaf-native.exe" "%DILIGAF_DIR%\"
    echo %GREEN% DILIGAF executable copied
) else (
    echo %RED% Error: diligaf-native.exe not found
    pause
    exit /b 1
)

REM Copy source files
set "source_files=diligaf-self-host.diligaf diligaf-compiler.diligaf diligaf-runtime.diligaf"
for %%f in (%source_files%) do (
    if exist "%%f" (
        copy "%%f" "%DILIGAF_DIR%\"
        echo %GREEN% Copied %%f
    )
)

REM Install to PATH
echo Installing DILIGAF to PATH...
setx PATH "%DILIGAF_DIR%;%PATH%" >nul 2>&1
echo %GREEN% DILIGAF installed to PATH

REM Create desktop shortcut
echo Creating desktop shortcut...
set "DESKTOP=%USERPROFILE%\Desktop"
(
echo [InternetShortcut]
echo URL=file:///%DILIGAF_DIR%\diligaf.bat
echo IconFile=%DILIGAF_DIR%\diligaf-native.exe
echo IconIndex=0
) > "%DESKTOP%\DILIGAF.url"
echo %GREEN% Desktop shortcut created

REM Final instructions
echo.
echo ====================================
echo %GREEN% DILIGAF Installation Complete!
echo ====================================
echo.
echo DILIGAF has been installed to: %DILIGAF_DIR%
echo DILIGAF is now available as 'diligaf' command
echo.
echo Usage:
echo   diligaf --repl                    # Start REPL
echo   diligaf --compile myfile.diligaf  # Compile to native
echo   diligaf myfile.diligaf            # Run file
echo   diligaf --version                 # Show version
echo.
echo To start using DILIGAF:
echo   1. Restart your command prompt or PowerShell
echo   2. Type 'diligaf --repl' to start the REPL
echo   3. Type 'diligaf --help' for more options
echo.
echo %GREEN% DILIGAF is now embedded in your shell!
echo %GREEN% Just type 'diligaf' and start hacking!
echo.
pause
