#!/usr/bin/env sbcl --script
;; DILIGAF Installation Script
;; Installs DILIGAF as a native command in the shell

(defun print-install-banner ()
  (format t "~%")
  (format t "╔══════════════════════════════════════════════════════════════╗~%")
  (format t "║  ██████╗ ██╗██╗     ██╗ ██████╗  █████╗ ███████╗  ║~%")
  (format t "║  ██╔══██╗██║██║     ██║██╔════╝ ██╔══██╗██╔════╝  ║~%")
  (format t "║  ██║  ██║██║██║     ██║██║  ███╗███████║█████╗    ║~%")
  (format t "║  ██║  ██║██║██║     ██║██║   ██║██╔══██║██╔══╝    ║~%")
  (format t "║  ██████╔╝██║███████╗██║╚██████╔╝██║  ██║██║       ║~%")
  (format t "║  ╚═════╝ ╚╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝       ║~%")
  (format t "║                                                      ║~%")
  (format t "║  DILIGAF Installation System                          ║~%")
  (format t "║  Embedding DILIGAF into your shell                    ║~%")
  (format t "╚══════════════════════════════════════════════════════╝~%")
  (format t "~%"))

(defun detect-shell ()
  "Detect the user's shell"
  (let ((shell (uiop:getenv "SHELL")))
    (cond
      ((string= shell "/bin/bash") :bash)
      ((string= shell "/bin/zsh") :zsh)
      ((string= shell "/bin/fish") :fish)
      ((string= shell "/bin/sh") :sh)
      ((string= shell "cmd.exe") :cmd)
      ((string= shell "powershell.exe") :powershell)
      (t :bash))))

(defun get-shell-config-file (shell)
  "Get the shell configuration file path"
  (let ((home (uiop:getenv "HOME")))
    (case shell
      (:bash (format nil "~a/.bashrc" home))
      (:zsh (format nil "~a/.zshrc" home))
      (:fish (format nil "~a/.config/fish/config.fish" home))
      (:sh (format nil "~a/.profile" home))
      (:cmd (format nil "~a/autoexec.bat" home))
      (:powershell (format nil "~a/Documents/WindowsPowerShell/Microsoft.PowerShell_profile.ps1" home))
      (t (format nil "~a/.bashrc" home)))))

(defun create-diligaf-wrapper ()
  "Create DILIGAF wrapper script"
  (let ((wrapper-content "#!/bin/bash
# DILIGAF Wrapper Script
# This script embeds DILIGAF into the shell

DILIGAF_DIR=\"$HOME/.diligaf\"
DILIGAF_EXECUTABLE=\"$DILIGAF_DIR/diligaf-native\"

# Function to run DILIGAF
run_diligaf() {
    if [ -f \"$DILIGAF_EXECUTABLE\" ]; then
        \"$DILIGAF_EXECUTABLE\" \"$@\"
    else
        echo \"DILIGAF not found. Please run 'diligaf-install' first.\"
        exit 1
    fi
}

# Function to show DILIGAF help
show_help() {
    echo \"DILIGAF - Direct Interface for Low-level Intrusion and General-purpose Attack Framework\"
    echo \"Usage: diligaf [command] [options] [file]\"
    echo \"\"
    echo \"Commands:\"
    echo \"  --repl, -r          Start DILIGAF REPL\"
    echo \"  --compile, -c       Compile DILIGAF file to native\"
    echo \"  --run, -r           Run DILIGAF file\"
    echo \"  --help, -h          Show this help\"
    echo \"  --version, -v       Show version\"
    echo \"  --install           Install DILIGAF\"
    echo \"  --uninstall         Uninstall DILIGAF\"
    echo \"\"
    echo \"Examples:\"
    echo \"  diligaf --repl                    # Start REPL\"
    echo \"  diligaf --compile myfile.diligaf  # Compile to native\"
    echo \"  diligaf myfile.diligaf            # Run file\"
    echo \"  diligaf --version                 # Show version\"
}

# Function to show version
show_version() {
    echo \"DILIGAF v1.0.0 - Self-Hosting Hacker Language\"
    echo \"Built: $(date)\"
    echo \"Architecture: $(uname -m)\"
    echo \"OS: $(uname -s)\"
}

# Function to install DILIGAF
install_diligaf() {
    echo \"Installing DILIGAF...\"
    mkdir -p \"$DILIGAF_DIR\"
    
    # Copy DILIGAF executable
    if [ -f \"diligaf-native\" ]; then
        cp diligaf-native \"$DILIGAF_EXECUTABLE\"
        chmod +x \"$DILIGAF_EXECUTABLE\"
        echo \"✓ DILIGAF executable installed\"
    else
        echo \"✗ DILIGAF executable not found. Please build DILIGAF first.\"
        exit 1
    fi
    
    # Copy DILIGAF source files
    if [ -d \"diligaf-source\" ]; then
        cp -r diligaf-source/* \"$DILIGAF_DIR/\"
        echo \"✓ DILIGAF source files installed\"
    fi
    
    echo \"DILIGAF installation complete!\"
    echo \"You can now use 'diligaf' command anywhere.\"
}

# Function to uninstall DILIGAF
uninstall_diligaf() {
    echo \"Uninstalling DILIGAF...\"
    rm -rf \"$DILIGAF_DIR\"
    echo \"✓ DILIGAF uninstalled\"
}

# Main command processing
case \"$1\" in
    --repl|-r)
        run_diligaf --repl
        ;;
    --compile|-c)
        if [ -z \"$2\" ]; then
            echo \"Error: No file specified for compilation\"
            exit 1
        fi
        run_diligaf --compile \"$2\" --output \"${2%.*}\"
        ;;
    --run|-r)
        if [ -z \"$2\" ]; then
            echo \"Error: No file specified for execution\"
            exit 1
        fi
        run_diligaf --script \"$2\"
        ;;
    --help|-h)
        show_help
        ;;
    --version|-v)
        show_version
        ;;
    --install)
        install_diligaf
        ;;
    --uninstall)
        uninstall_diligaf
        ;;
    --*)
        echo \"Unknown option: $1\"
        show_help
        exit 1
        ;;
    \"\")
        show_help
        ;;
    *)
        # If it's a file, run it
        if [ -f \"$1\" ]; then
            run_diligaf --script \"$1\"
        else
            echo \"Error: File not found: $1\"
            exit 1
        fi
        ;;
esac
"))
    (write-file "diligaf" wrapper-content)))

(defun create-diligaf-launcher ()
  "Create DILIGAF launcher script"
  (let ((launcher-content "#!/bin/bash
# DILIGAF Launcher
# Quick launcher for DILIGAF

# Set DILIGAF environment
export DILIGAF_HOME=\"$HOME/.diligaf\"
export PATH=\"$DILIGAF_HOME:$PATH\"

# Launch DILIGAF
exec \"$DILIGAF_HOME/diligaf\" \"$@\"
"))
    (write-file "diligaf-launcher" launcher-content)))

(defun create-diligaf-completion (shell)
  "Create shell completion for DILIGAF"
  (case shell
    (:bash
     (let ((completion-content "# DILIGAF Bash Completion
_diligaf_completion() {
    local cur=\"${COMP_WORDS[COMP_CWORD]}\"
    local prev=\"${COMP_WORDS[COMP_CWORD-1]}\"
    
    case \"$prev\" in
        --compile|-c)
            COMPREPLY=($(compgen -f -- \"$cur\" | grep '\\.diligaf$'))
            ;;
        --run|-r)
            COMPREPLY=($(compgen -f -- \"$cur\" | grep '\\.diligaf$'))
            ;;
        *)
            COMPREPLY=($(compgen -W '--repl --compile --run --help --version --install --uninstall' -- \"$cur\"))
            ;;
    esac
}

complete -F _diligaf_completion diligaf
"))
       (write-file "diligaf-completion.bash" completion-content)))
    (:zsh
     (let ((completion-content "# DILIGAF Zsh Completion
_diligaf() {
    local context state line
    _arguments '1: :->command' '*::arg:->args'
    
    case $state in
        command)
            _values 'diligaf commands' \\
                '--repl[Start DILIGAF REPL]' \\
                '--compile[Compile DILIGAF file]' \\
                '--run[Run DILIGAF file]' \\
                '--help[Show help]' \\
                '--version[Show version]' \\
                '--install[Install DILIGAF]' \\
                '--uninstall[Uninstall DILIGAF]'
            ;;
        args)
            case $line[1] in
                --compile|-c|--run|-r)
                    _files -g '*.diligaf'
                    ;;
            esac
            ;;
    esac
}

compdef _diligaf diligaf
"))
       (write-file "diligaf-completion.zsh" completion-content)))
    (:fish
     (let ((completion-content "# DILIGAF Fish Completion
complete -c diligaf -s r -l repl -d 'Start DILIGAF REPL'
complete -c diligaf -s c -l compile -d 'Compile DILIGAF file' -r
complete -c diligaf -s r -l run -d 'Run DILIGAF file' -r
complete -c diligaf -s h -l help -d 'Show help'
complete -c diligaf -s v -l version -d 'Show version'
complete -c diligaf -l install -d 'Install DILIGAF'
complete -c diligaf -l uninstall -d 'Uninstall DILIGAF'
complete -c diligaf -f -a '*.diligaf'
"))
       (write-file "diligaf-completion.fish" completion-content)))))

(defun install-diligaf-to-shell (shell)
  "Install DILIGAF to shell configuration"
  (let ((config-file (get-shell-config-file shell))
        (diligaf-dir (format nil "~a/.diligaf" (uiop:getenv "HOME"))))
    
    (case shell
      (:bash
       (let ((bash-config (format nil "
# DILIGAF Configuration
export DILIGAF_HOME=\"~a\"
export PATH=\"$DILIGAF_HOME:$PATH\"

# DILIGAF completion
if [ -f \"$DILIGAF_HOME/diligaf-completion.bash\" ]; then
    source \"$DILIGAF_HOME/diligaf-completion.bash\"
fi

# DILIGAF alias
alias diligaf='$DILIGAF_HOME/diligaf'
" diligaf-dir)))
         (append-to-file config-file bash-config)))
      
      (:zsh
       (let ((zsh-config (format nil "
# DILIGAF Configuration
export DILIGAF_HOME=\"~a\"
export PATH=\"$DILIGAF_HOME:$PATH\"

# DILIGAF completion
if [ -f \"$DILIGAF_HOME/diligaf-completion.zsh\" ]; then
    source \"$DILIGAF_HOME/diligaf-completion.zsh\"
fi

# DILIGAF alias
alias diligaf='$DILIGAF_HOME/diligaf'
" diligaf-dir)))
         (append-to-file config-file zsh-config)))
      
      (:fish
       (let ((fish-config (format nil "
# DILIGAF Configuration
set -gx DILIGAF_HOME \"~a\"
set -gx PATH \"$DILIGAF_HOME\" $PATH

# DILIGAF completion
if test -f \"$DILIGAF_HOME/diligaf-completion.fish\"
    source \"$DILIGAF_HOME/diligaf-completion.fish\"
end

# DILIGAF alias
alias diligaf='$DILIGAF_HOME/diligaf'
" diligaf-dir)))
         (append-to-file config-file fish-config)))
      
      (:cmd
       (let ((cmd-config (format nil "
@echo off
REM DILIGAF Configuration
set DILIGAF_HOME=~a
set PATH=%DILIGAF_HOME%;%PATH%
" diligaf-dir)))
         (append-to-file config-file cmd-config)))
      
      (:powershell
       (let ((ps-config (format nil "
# DILIGAF Configuration
$env:DILIGAF_HOME = \"~a\"
$env:PATH = \"$env:DILIGAF_HOME;$env:PATH\"

# DILIGAF alias
Set-Alias -Name diligaf -Value \"$env:DILIGAF_HOME\\diligaf\"
" diligaf-dir)))
         (append-to-file config-file ps-config))))))

(defun append-to-file (filename content)
  "Append content to file"
  (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
    (write-sequence content stream)))

(defun create-diligaf-directory ()
  "Create DILIGAF installation directory"
  (let ((diligaf-dir (format nil "~a/.diligaf" (uiop:getenv "HOME"))))
    (uiop:run-program (format nil "mkdir -p ~a" diligaf-dir))
    diligaf-dir))

(defun copy-diligaf-files (diligaf-dir)
  "Copy DILIGAF files to installation directory"
  (let ((files (list "diligaf-native" "diligaf-self-host.diligaf" "diligaf-compiler.diligaf" "diligaf-runtime.diligaf")))
    (dolist (file files)
      (if (probe-file file)
          (progn
            (uiop:run-program (format nil "cp ~a ~a/" file diligaf-dir))
            (format t "✓ Copied ~a~%" file))
          (format t "✗ File not found: ~a~%" file)))))

(defun make-diligaf-executable (diligaf-dir)
  "Make DILIGAF executable"
  (let ((diligaf-exe (format nil "~a/diligaf" diligaf-dir)))
    (uiop:run-program (format nil "chmod +x ~a" diligaf-exe))))

(defun create-diligaf-man-page ()
  "Create DILIGAF man page"
  (let ((man-content ".TH DILIGAF 1 \"$(date +%Y-%m-%d)\" \"DILIGAF v1.0.0\" \"DILIGAF Manual\"
.SH NAME
diligaf - Direct Interface for Low-level Intrusion and General-purpose Attack Framework

.SH SYNOPSIS
.B diligaf
[\fIOPTIONS\fR] [\fIFILE\fR]

.SH DESCRIPTION
DILIGAF is a self-hosting hacker-focused programming language built for penetration testing, exploitation, and system manipulation. It provides direct access to system internals, dynamic code modification, and built-in tools for reconnaissance and exploitation.

.SH OPTIONS
.TP
.BR --repl ", " -r
Start DILIGAF interactive REPL

.TP
.BR --compile ", " -c
Compile DILIGAF file to native executable

.TP
.BR --run ", " -r
Run DILIGAF file

.TP
.BR --help ", " -h
Show help message

.TP
.BR --version ", " -v
Show version information

.TP
.BR --install
Install DILIGAF

.TP
.BR --uninstall
Uninstall DILIGAF

.SH EXAMPLES
.TP
Start DILIGAF REPL:
.B diligaf --repl

.TP
Compile DILIGAF file:
.B diligaf --compile myfile.diligaf

.TP
Run DILIGAF file:
.B diligaf myfile.diligaf

.TP
Show version:
.B diligaf --version

.SH FILES
.TP
.B ~/.diligaf/
DILIGAF installation directory

.TP
.B ~/.diligaf/diligaf
DILIGAF executable

.TP
.B ~/.diligaf/diligaf-self-host.diligaf
DILIGAF self-hosting interpreter

.SH AUTHOR
DILIGAF Development Team

.SH SEE ALSO
.BR bash (1), ", " zsh (1), ", " fish (1)
"))
    (write-file "diligaf.1" man-content)))

(defun install-man-page (diligaf-dir)
  "Install DILIGAF man page"
  (let ((man-dir (format nil "~a/share/man/man1" diligaf-dir)))
    (uiop:run-program (format nil "mkdir -p ~a" man-dir))
    (uiop:run-program (format nil "cp diligaf.1 ~a/" man-dir))
    (format t "✓ Man page installed~%")))

(defun main ()
  "Main installation function"
  (print-install-banner)
  
  (format t "Installing DILIGAF to your shell...~%")
  (format t "====================================~%~%")
  
  ;; Detect shell
  (let ((shell (detect-shell)))
    (format t "Detected shell: ~a~%" shell))
  
  ;; Create DILIGAF directory
  (format t "Creating DILIGAF directory...~%")
  (let ((diligaf-dir (create-diligaf-directory)))
    (format t "✓ DILIGAF directory created: ~a~%" diligaf-dir))
  
  ;; Create wrapper script
  (format t "Creating DILIGAF wrapper script...~%")
  (create-diligaf-wrapper)
  (format t "✓ Wrapper script created~%")
  
  ;; Create launcher script
  (format t "Creating DILIGAF launcher script...~%")
  (create-diligaf-launcher)
  (format t "✓ Launcher script created~%")
  
  ;; Create shell completion
  (format t "Creating shell completion...~%")
  (create-diligaf-completion (detect-shell))
  (format t "✓ Shell completion created~%")
  
  ;; Copy DILIGAF files
  (format t "Copying DILIGAF files...~%")
  (copy-diligaf-files diligaf-dir)
  
  ;; Make executable
  (format t "Making DILIGAF executable...~%")
  (make-diligaf-executable diligaf-dir)
  (format t "✓ DILIGAF executable created~%")
  
  ;; Create man page
  (format t "Creating DILIGAF man page...~%")
  (create-diligaf-man-page)
  (install-man-page diligaf-dir)
  
  ;; Install to shell
  (format t "Installing DILIGAF to shell configuration...~%")
  (install-diligaf-to-shell (detect-shell))
  (format t "✓ DILIGAF installed to shell~%")
  
  ;; Final instructions
  (format t "~%====================================~%")
  (format t "DILIGAF Installation Complete!~%")
  (format t "====================================~%~%")
  
  (format t "DILIGAF has been installed to: ~a~%" diligaf-dir)
  (format t "DILIGAF is now available as 'diligaf' command~%~%")
  
  (format t "Usage:~%")
  (format t "  diligaf --repl                    # Start REPL~%")
  (format t "  diligaf --compile myfile.diligaf  # Compile to native~%")
  (format t "  diligaf myfile.diligaf            # Run file~%")
  (format t "  diligaf --version                 # Show version~%~%")
  
  (format t "To start using DILIGAF:~%")
  (format t "  1. Restart your shell or run: source ~a~%" (get-shell-config-file (detect-shell)))
  (format t "  2. Type 'diligaf --repl' to start the REPL~%")
  (format t "  3. Type 'diligaf --help' for more options~%~%")
  
  (format t "DILIGAF is now embedded in your shell!~%")
  (format t "Just type 'diligaf' and start hacking!~%~%"))

;; Run installation
(main)
